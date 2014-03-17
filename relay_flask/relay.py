import os
import erlang_functions as erlfuncs
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
                s.timestamp * 1000 as status_time,
                p.plan as plan,
                p.plan_time * 1000 as plan_time,
                b.bhvr as behaviour,
                b.bhvr_time * 1000 as bhvr_time
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

@app.route('/request_all_intersections', methods=['POST'])
def get_all_intersections():
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
                s.timestamp * 1000 as status_time,
                p.plan as plan,
                p.plan_time * 1000 as plan_time,
                b.bhvr as behaviour,
                b.bhvr_time * 1000 as bhvr_time
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
                i.long<=:maxlong;
            '''
        intersects = g.db.query('relay_main', qstr, bounds, as_dict=True) 

        return createJSON(intersects)

@app.route('/request_roads', methods=['GET'])
def get_roads():
    if request.method == 'GET':
        qstr = '''SELECT obj_id, lf_name as name, lfn_id, TNODE, FNODE FROM roads;''' 

        roads = g.db.query('relay_main', qstr, as_dict=True)
        return createJSON(roads)

@app.route('/request_plan', methods=['POST'])
def get_plan():
    if request.method == 'POST':
        int_id = request.json # dictionary of: minlat, maxlat, minlong, maxlong
        qstr = '''
            SELECT 
                timestamp * 1000 as timestamp,
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
                timestamp * 1000 as timestamp,
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
                timestamp * 1000 as timestamp,
                value,
                int_id 
            FROM 
                int_events
            LIMIT 
                :num_events;
            ''' 
        evts = g.db.query('relay_main', qstr, num_events, as_dict=True)
        return createJSON(evts)

@app.route('/request_int_events', methods=['POST'])
def get_int_evts():
    if request.method == 'POST':
        int_id = request.json 
        qstr = '''
            SELECT 
                timestamp * 1000 as timestamp,
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
        length = params['duration']

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
                timestamp * 1000,
                value,
                int_id 
            FROM 
                int_events
            WHERE
                int_id = :int_id;
            '''

        if int_id in g.sim_ids:
            new_status_info, new_qs_dict = erlfuncs.fetch_status_info(int_id, 150)
            print new_qs_dict
        else:
            new_status_info = erlfuncs.make_status_info()
            new_qs_dict = erlfuncs.make_queues()

        info = g.db.query('relay_main', q1, params, as_dict=True)
        info.append({'events': g.db.query('relay_main', q2, params, as_dict=True)})
        info.append(new_qs_dict)
        info.append(new_status_info)

        return createJSON(info)

@app.route('/request_network', methods=['GET'])
def request_netowkr():
    if request.method == 'GET':
        qstr = '''
            SELECT
                status,
                count(*) as num
            FROM 
                int_status
            GROUP BY
                status;
            '''
        evts = g.db.query('relay_main', qstr, as_dict=True)

        return createJSON(evts)

@app.route('/request_flows', methods=['GET'])
def get_flows():
    if request.method == 'GET':
        flows = mqs.make_queues()
        return createJSON(flows)

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
        # g.sim_queues = [] # [IN, OUT, REMOTE]

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
