import os
from db.DatabaseManager import DatabaseManager

from flask import Flask
from flask import json
from flask import redirect
from flask import render_template
from flask import request, Response
from flask import session, escape
from flask import url_for

app = Flask(__name__)
app.secret_key = os.environ.get('APP_SECRET', None)
cm_api_key = os.environ.get('CM_API', 'e440fa3faa334156831adb28596d54a0')
db = DatabaseManager({'relay_main': 'db/relay.db'})

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
        qstr = ''' SELECT long, lat, name FROM intersections WHERE lat>=:minlat
            AND lat<=:maxlat AND long>=:minlong AND long<=:maxlong;'''
        intersects = db.query('relay_main', qstr, bounds, True)
        return createJSON(intersects)

# we used passed dictionary's into this before.
def createJSON(vals):
    ''' Use this for constructing JSON to send to app. '''
    try:
        js = json.dumps(vals)
        return Response(js, status=200, mimetype='applicaiton/json')
    except Exception as e:
        return "Error: " + str(e)

# To read json from post request: request.json

if __name__ == '__main__':
    port = int(os.environ.get('PORT', 5000))
    app.run(host='0.0.0.0', port=port, debug=True)
