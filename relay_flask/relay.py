import os
import signal_helpers as sighelp
import merge_queues as mqs
import datetime as dt

from collections import defaultdict
from db.DatabaseManager import DatabaseManager

from flask import Flask, jsonify
from flask import g
from flask import json
from flask import redirect
from flask import render_template
from flask import request, Response
from flask import session, escape
from flask import url_for

from sqlalchemy import *

app = Flask(__name__)
# app.secret_key = os.environ.get('APP_SECRET', None)
# cm_api_key = os.environ.get('CM_API', 'e440fa3faa334156831adb28596d54a0')

db = create_engine('sqlite:///db/relay.db')

@app.route('/')
def index():
    # spawn something to keep track of information for simulated intersections
    setup_simulation()

    return redirect(url_for('static', filename='index.html'))

@app.route('/request_intersections', methods=['POST'])
def get_intersections():
    if request.method == 'POST':
        bounds = request.json # dictionary of: minlat, maxlat, minlong, maxlong
        qstr = '''
            SELECT 
                i.long, 
                i.lat, 
                i.name, 
                i.type, 
                i.type_short, 
                i.int_id,
                s.status as status,
                s.timestamp as status_time,
                p.plan as plan,
                p.plan_time as plan_time,
                b.bhvr as behaviour,
                b.bhvr_time as bhvr_time
            FROM
                intersections as i
                LEFT OUTER JOIN int_status as s 
                    ON i.int_id = s.int_id
                LEFT OUTER JOIN int_plans as p
                    ON i.int_id = p.int_id
                LEFT JOIN int_bhvrs as b
                    ON i.int_id = b.int_id
            WHERE
                i.lat>=:minlat AND
                i.lat<=:maxlat AND
                i.long>=:minlong AND
                i.long<=:maxlong AND
                i.type_short IN  ('MJRML', 'MJRSL', 'MAJINT');
            '''
        intersects = g.db.query('relay_main', qstr, bounds, as_dict=True) 

        return createJSON(intersects)

@app.route('/request_roads', methods=['POST'])
def get_roads():
    if request.method == 'POST':
        bounds = request.json # dictionary of: minlat, maxlat, minlong, maxlong
        # qstr = '''SELECT * FROM roads;''' 
        # #WHERE lat>=:minlat AND lat<=:maxlat AND
        #  #   long>=:minlong AND long<=:maxlong and type_short IN  ('MJRML', 'MJRSL');'''
        # roads = g.db.query('relay_main', qstr, bounds, as_dict=True)
        return createJSON(bounds)

@app.route('/request_plan', methods=['POST'])
def get_plan():
    if request.method == 'POST':
        int_id = request.json # dictionary of: minlat, maxlat, minlong, maxlong
        qstr = '''
            SELECT 
                timestamp,
                value 
            FROM 
                int_metrics
            WHERE
                name = 'plan' AND
                int_id = :int_id;
            ''' 
        plan = g.db.query('relay_main', qstr, int_id, as_dict=True)
        return createJSON(plan)

@app.route('/request_status', methods=['POST'])
def get_status():
    if request.method == 'POST':
        int_id = request.json # dictionary of: minlat, maxlat, minlong, maxlong
        qstr = '''
            SELECT 
                timestamp,
                value 
            FROM 
                int_metrics
            WHERE
                name = 'status' AND
                int_id = :int_id;
            ''' 
        stat = g.db.query('relay_main', qstr, int_id, as_dict=True)
        return createJSON(stat)

@app.route('/request_all_events', methods=['POST'])
def get_all_evts():
    if request.method == 'POST':
        num_events = request.json
        qstr = '''
            SELECT 
                timestamp,
                value,
                int_id 
            FROM 
                int_events
            LIMIT 
                :num_events;
            ''' 
        evts = g.db.query('relay_main', qstr, num_events, as_dict=False)
        return createJSON(evts)

@app.route('/request_int_events', methods=['POST'])
def get_int_evts():
    if request.method == 'POST':
        int_id = request.json 
        qstr = '''
            SELECT 
                timestamp,
                value,
                int_id 
            FROM 
                int_events
            WHERE
                int_id = :int_id;
            ''' 
        evts = g.db.query('relay_main', qstr, int_id, as_dict=True)
        return createJSON(evts)

@app.route('/request_dashboard', methods=['POST'])
def get_dash():
    if request.method == 'POST':
        params = request.json

        # Get new values from erlang processes
        int_id = params['int_id']

        if int_id in g.sim_ids:
            b_res = update_behaviour(int_id)
            p_res = update_plan(int_id)
            g.sim_queues, new_qs_dict = mqs.fetch_queues(int_id) #merge_simulated_queues(int_id, g.sim_queues, g.db)
        else:
            new_qs_dict = mqs.make_queues()

        q1 = '''
            SELECT 
                s.status as status,
                s.timestamp as status_time,
                p.plan as plan,
                p.plan_time as plan_time,
                b.bhvr as behaviour,
                b.bhvr_time as bhvr_time
            FROM
                int_status as s
                LEFT OUTER JOIN int_plans as p
                    ON s.int_id = p.int_id
                LEFT JOIN int_bhvrs as b
                    ON s.int_id = b.int_id
            WHERE
                s.int_id = :int_id;
            '''

        q2 = '''
            SELECT 
                timestamp,
                value,
                int_id 
            FROM 
                int_events
            WHERE
                int_id = :int_id;
            '''

        info = g.db.query('relay_main', q1, params, as_dict=True)
        info.append({'events': g.db.query('relay_main', q2, params, as_dict=True)})
        info.append(new_qs_dict)

        return createJSON(info)

@app.route('/request_flows', methods=['POST'])
def get_flows():
    if request.method == 'POST':
        flow_params = request.json # int_id, dt, duration
        # qstr = '''
        #     SELECT 
        #         timestamp,
        #         value,
        #         in_node_id,
        #         out_node_id 
        #     FROM 
        #         int_metrics
        #     WHERE
        #         name = 'flow' AND
        #         int_id = :int_id AND
        #         timestamp >= 0
        #     ORDER BY
        #         timestamp ASC;
        #     '''
        # # strftime('%s', :intial_time)
        # flows = g.db.query('relay_main', qstr, int_id, as_dict=True)

        flows = gen_flows()

        # flow_arrays = create_flow_arrs(flows)
        return createJSON(flows)

def gen_flows():
    A, B, C, D, E = sighelp.generate_signals(250)
    signals = [A, B, C, D]

    return sighelp.create_hist_dict(signals, 1)

@app.route('/api/charts', methods=['POST'])
def get_chart():
    if request.method == 'POST':
        int_id = request.json
        qstr = '''
        SELECT
            a.time,
            a.value as costUD,
            b.value as costLR,
            c.value as vol,
            d.value as perf
        from
            intstats as a
            inner join intstats as b
            inner join intstats as c
            inner join intstats as d
        on
            a.time = b.time
            and b.time = c.time
            and c.time = d.time
        where
            a.series_type = 'costUD'
            and b.series_type = 'costLR'
            and c.series_type = 'vol'
            and d.series_type = 'perf'
            and a.int_id = :int_id
            and b.int_id = :int_id
            and c.int_id = :int_id
            and d.int_id = :int_id;
        '''
        int_id['int_id'] = 13463459  # Change this eventually
        chart_data = g.db.query('relay_main', qstr, int_id, as_dict=True)
        pivot = defaultdict(lambda: [])
        for row in chart_data:
            for k, v in row.iteritems():
                pivot[k].append(v)

        return createJSON(pivot)

@app.before_request
def before_request():
    '''Open the database connections in preparation for response'''
    # connection = db.connect()
    x = os.path.join(os.getcwd(), 'db/relay.db')
    g.db = DatabaseManager(db_info={'relay_main': x})
    setup_simulation()

def setup_simulation():
    if not hasattr(g, 'sim_ids'):
        g.sim_ids = [13464373, 13464094, 13463747, 13463548, 13463436]
        g.sim_queues = [] # [IN, OUT, REMOTE]

@app.after_request
def after_request(response):
    '''Close the database connections'''
    g.db.close_all()
    return response

@app.errorhandler(404)
def page_not_found(error):
    return 'This page does not exist.', 404

def createJSON(vals):
    ''' Use this for constructing JSON to send to app. '''
    try:
        js = json.dumps(vals)
        return Response(js, status=200, mimetype='application/json')
    except Exception as e:
        return "Error: " + str(e)


if __name__ == '__main__':
    port = int(os.environ.get('PORT', 5000))
    app.run(host='localhost', port=port, debug=True)

## UPDATING FUNCTIONS ##########################################################
def update_plan(int_id):
    '''
    Given an intersection ID, we want to query erlang and get the newest plan.
    Then take this plan, turn it into a "code" and update the intersections entry
    in the database.
    '''
    def get_plan(int_id):
        '''placeholder for erl call'''
        return ('SWT', dt.datetime.now().strftime('%s'))

    plan, plan_time = get_plan(int_id)
    params = {'int_id': int_id, 'plan': plan, 'plan_time': plan_time}
    qstr = '''
        UPDATE
            int_plans
        SET
            plan = :plan,
            plan_time = :plan_time,
            timestamp = strftime('%s','now')
        WHERE
            int_id = :int_id;
        '''
    result = g.db.query('relay_main', qstr, params, as_dict=True)
    return result

def update_behaviour(int_id):
    '''
    Given an intersection ID, we want to query erlang and get the current bhvr.
    Then take this bhvr, turn it into a "code" and update the intersections entry
    in the database.
    '''
    def get_bhvr(int_id):
        '''placeholder for erl call'''
        return ('SWT', dt.datetime.now().strftime('%s'))

    bhvr, bhvr_time = get_bhvr(int_id)
    params = {'int_id': int_id, 'bhvr': bhvr, 'bhvr_time': bhvr_time}
    qstr = '''
        UPDATE
            int_bhvrs
        SET
            bhvr = :bhvr,
            bhvr_time = :bhvr_time,
            timestamp = strftime('%s','now')
        WHERE
            int_id = :int_id;
        '''
    result = g.db.query('relay_main', qstr, params, as_dict=True)
    return result
