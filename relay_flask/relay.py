import os
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
    return redirect(url_for('static', filename='index.html'))

@app.route('/request_intersections', methods=['POST'])
def get_intersections():
    if request.method == 'POST':
        bounds = request.json # dictionary of: minlat, maxlat, minlong, maxlong
        qstr = '''SELECT long, lat, name, type, type_short, int_id FROM
            intersections WHERE lat>=:minlat AND lat<=:maxlat AND
            long>=:minlong AND long<=:maxlong and type_short IN  ('MJRML', 'MJRSL');'''
        intersects = g.db.query('relay_main', qstr, bounds, as_dict=True)
        return createJSON(intersects)

@app.route('/request_roads', methods=['POST'])
def get_roads():
    if request.method == 'POST':
        bounds = request.json # dictionary of: minlat, maxlat, minlong, maxlong
        qstr = '''SELECT * FROM roads;''' 
        #WHERE lat>=:minlat AND lat<=:maxlat AND
         #   long>=:minlong AND long<=:maxlong and type_short IN  ('MJRML', 'MJRSL');'''
        roads = g.db.query('relay_main', qstr, as_dict=True)
        return createJSON(roads)

# @app.route('/get_intersections', methods=['get'])
# def get_intersections():
#     connection = db.connect()
#     qstr = '''SELECT * FROM
#             intersections WHERE type_short IN  ('MJRML', 'MJRSL');'''
#     result = connection.execute(qstr)
#     data = []
#     return jsonify(data)

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


@app.after_request
def after_request(response):
    '''Close the database connections'''
    # connection.close()
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
