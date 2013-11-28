import os
from db.DatabaseManager import DatabaseManager

from flask import Flask
from flask import g
from flask import json
from flask import redirect
from flask import render_template
from flask import request, Response
from flask import session, escape
from flask import url_for

app = Flask(__name__)
app.secret_key = os.environ.get('APP_SECRET', None)
cm_api_key = os.environ.get('CM_API', 'e440fa3faa334156831adb28596d54a0')


@app.route('/')
def home():
    return 'Hello World'


@app.route('/default_map')
def default_map():
    return redirect(url_for('static', filename='default_map.html'))


@app.route('/map/<api_key>')
def show_map(api_key=cm_api_key):
    return render_template('default_map.html', api_key=api_key)


@app.route('/request_intersections', methods=['POST'])
def get_intersections():
    if request.method == 'POST':
        bounds = request.json # dictionary of: minlat, maxlat, minlong, maxlong
        qstr = '''SELECT long, lat, name, type, type_short, int_id FROM 
            intersections WHERE lat>=:minlat AND lat<=:maxlat AND 
            long>=:minlong AND long<=:maxlong and type_short='MJRSL';'''
        intersects = g.db.query('relay_main', qstr, bounds, True)
        return createJSON(intersects)


@app.before_request
def before_request():
    '''Open the database connections in preparation for response'''
    x = os.path.join(os.getcwd(), 'db/relay.db')
    g.db = DatabaseManager(db_info={'relay_main': x})


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
        return Response(js, status=200, mimetype='applicaiton/json')
    except Exception as e:
        return "Error: " + str(e)


if __name__ == '__main__':
    port = int(os.environ.get('PORT', 5000))
    app.run(host='localhost', port=port, debug=True)
