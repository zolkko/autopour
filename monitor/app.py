import json
from flask import Flask, render_template, redirect, request, flash, session, make_response
from flask.ext.login import LoginManager, login_required, login_user, logout_user
from flask_peewee.db import Database
from flask_peewee.utils import make_password, check_password
from peewee import Model, CharField, IntegerField, DoesNotExist
from config import settings


app = Flask(__name__)
app.config.update(settings)

db = Database(app)

login_manager = LoginManager()
login_manager.init_app(app)


class User(db.get_model_class()):
    id = IntegerField(primary_key=True)
    name = CharField(unique=True, max_length=20)
    password = CharField(max_length=32)

    def is_active(self):
        return True

    def is_authenticated(self):
        return True

    def is_anonymous(self):
        return False


@login_manager.user_loader
def load_user(id):
    app.logger.debug('Retrieving a user {} from the database.'.format(id))
    try:
        return User.get(User.id == id)
    except DoesNotExist:
        app.logger.error('User {} does not exists in the database'.format(id))
        return None


@login_manager.unauthorized_handler
def unauthorized_handler():
    """ Handles unauthorized requests. """
    app.logger.debug('Unauthorized access to the application')
    return redirect('/sign-in')


@app.route('/')
@app.route('/plants')
@app.route('/plants/<int:plant_id>')
@app.route('/gateway')
@app.route('/device')
@login_required
def index(*args, **kwargs):
    """ Application entry point. Browser should access only this particular view.
    The rest of APIs are JSON based. """
    return render_template('index.html')


@app.route('/api/plants')
def plants():
    # from time import sleep
    # sleep(3)
    response = make_response(json.dumps([{
        'id': 1,
        'plantName': 'Rose Number 1',
        'lastUpdate': '2013-11-13 13:55',
        'plantCondition': 'normal',
        'sunLevel': 12,
        'sunStats': 21,
        'waterLevel': 55,
        'waterEstimate': 32
    }, {
        'id': 2,
        'plantName': 'Rose Number 2',
        'lastUpdate': '2013-11-13 09:55',
        'plantCondition': 'good',
        'sunLevel': 22,
        'sunStats': 33,
        'waterLevel': 44,
        'waterEstimate': 55
    }]))
    response.mimetype = 'application/json'
    return response


@app.route('/sign-in', methods=('GET',))
def sign_in():
    """ This function renders sign-in form """
    users = tuple(User.select().limit(1))
    if not tuple(users):
        app.logger.debug('Database is empty. Starting initialization process.')
        return render_template('register.html')

    return render_template('sign-in.html')


@app.route('/sign-in', methods=('POST',))
def sign_in_user():
    """ Method handles attempts to login into the system.
    """
    user_name = request.form.get('username', '')
    password = request.form.get('password', '')
    retyped_password = request.form.get('repassword', '')

    app.logger.debug('User attempt to sign-in into the system. User name "{}" and password "{}".'.format(
        user_name,
        password
    ))

    if user_name and password:
        has_users = tuple(User.select().limit(1)) != ()
        if not has_users:
            if password != retyped_password:
                app.logger.debug('Passwords do not match.')
                session['sign-in-username'] = user_name
                flash('Passwords do not match')
            else:
                app.logger.debug('Registering user "{}".'.format(user_name))
                User.create(name=user_name, password=make_password(password))
        else:
            users = tuple(User.select().where(User.name == user_name).limit(1))
            if users:
                user = users[0]
                if users and check_password(password, user.password):
                    app.logger.debug('Logging user "{}".'.format(user_name))

                    login_user(user)
                    if 'sign-in-username' in session:
                        del session['sign-in-username']
                    return redirect('/')

            app.logger.debug('Invalid password or username provided.')
            flash('Invalid credentials provided.')
            session['sign-in-username'] = user_name
    else:
        app.logger.debug('Empty username and password provided')
    return redirect('/sign-in')


@app.route('/sign-out')
@login_required
def sign_out():
    """ When a user hits this link it gets sign out and redirected to the sign-in screen. """
    logout_user()
    return redirect('/sign-in')


def ensure_db():
    User.create_table(fail_silently=True)


if __name__ == '__main__':
    ensure_db()
    app.run()
