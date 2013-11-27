import os

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

@app.route('/')
def home():
    return 'Hello World'

@app.route('/default_map')
def default_map():
    return redirect(url_for('static', filename='default_map.html'))

@app.route('/map/<api_key>')
def show_map(api_key=cm_api_key):
    return render_template('default_map.html', api_key=api_key)

if __name__ == '__main__':
    port = int(os.environ.get('PORT', 5000))
    app.run(host='0.0.0.0', port=port, debug=True)
