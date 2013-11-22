import os

from flask import Flask

app = Flask(__name__)
app.secret_key = os.environ.get('APP_SECRET', None)

@app.route('/')
def home():
    return 'Hello World'

if __name__ == '__main__':
    port = int(os.environ.get('PORT', 5000))
    app.run(host='0.0.0.0', port=port, debug=True)