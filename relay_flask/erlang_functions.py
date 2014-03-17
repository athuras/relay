import numpy as np
import signal_helpers as sighelp
import merge_queues as mq
import datetime as dt
import requests as rqs
import db.bhvr_parser as b_parser

def fetch_queues(int_id, length=150):
    ''' get queues from erlang and turn them into histos for front end'''

    # new queues, queue dict

def fetch_status_info(int_id, length=150):
    r = rqs.get("http://localhost:8081/?agent=Agent1")
    data = r.json()

    erl_info = [{'bhvr_mtx': data['behaviour']}]

    plan_codes = [b_parser.parse(b) for b in data['current_plan']]
    erl_info.append({'plans': plan_codes, 'plan_times': data['current_timing']})

    new_queues = [data['ingress'],data['egress']]
    up_qs = mq.merge_simulated_queues(new_queues, length=150)

    return erl_info, {'in': up_qs[0], 'out': up_qs[1], 'prediction': 0}

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

# DUMMY FOR FAKE INTERSECTIONS
def make_queues():
    A, B, C, D, E = sighelp.generate_signals(30)
    signals = [A, B, C, D]
    qs = sighelp.create_hist_dict(signals, 1)

    return {'in': qs, 'out': qs, 'prediction': qs}

def make_status_info():
    status_info = [{'bhvr_mtx': [[ 0. ,  0.1,  0.6,  0.3],
        [ 1. ,  0. ,  0. ,  0. ], 
        [ 0.7,  0.2,  0. ,  0.1],
        [ 0. ,  0. ,  1. ,  0. ]]}]

    p = 'EWT'
    p2 = 'NST'
    # if np.random.rand(1) > 0.5:
    #     p = 'NST'
    status_info.append({'plans': [p, p2], 'plan_times': 
        [int(dt.datetime.now().strftime('%s')) + np.random.randint(15,45)]})

    return status_info