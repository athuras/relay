import os

from flask import Flask
from flask import redirect, url_for

app = Flask(__name__)
app.secret_key = os.environ.get('APP_SECRET', None)

@app.route('/')
def home():
    return 'Hello World'

@app.route('/maps')
def show_map():
    return redirect(url_for('static', filename='default_map.html'))

if __name__ == '__main__':
    port = int(os.environ.get('PORT', 5000))
    #cloudmade_api_key = str(os.environ.get('CM_API', 'e440fa3faa334156831adb28596d54a0'))
    app.run(host='0.0.0.0', port=port, debug=True)
