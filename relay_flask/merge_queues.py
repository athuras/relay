import numpy as np
import signal_helpers as sighelp
import datetime as dt

def fetch_queues(int_id, length=150):
    ''' get queues from erlang and turn them into histos for front end'''

    # new queues, queue dict

def make_queues():
    A, B, C, D, E = sighelp.generate_signals(250)
    signals = [A, B, C, D]
    qs = sighelp.create_hist_dict(signals, 1)
    return {'in': qs, 'out': qs, 'prediction': qs}

def merge_simulated_queues(new_queues, sim_queues, length):
    #pull from database the timestamps for this intersection
    #arrange them into order based on node id
    #merge with new queue
    #write new queue back to database
    if sim_queues = None:
        updated_queues = [reduce_qs(qs) for qs in new_queues]
    else:
        updated_queues = [merge_queues(q, sim_queues[i], length) for i, q in enumerate(new_queues)]
    return updated_queues

def reduce_qs(qs):
    return [reduce_q(q) for q in qs]

def merge_queues(new_qs, old_qs, length):
    '''take two sets of queues (one for each node) and merge them'''
    return [initiate_merge(nq, old_qs[i], length) for i, q in enumerate(new_qs)]

def initiate_merge(new_queue, old_queue, length):
    ''' merge the two queues from same node'''

    r_new_queue = reduce_q(new_queue)
    old_max_time = max(r_new_queue, key=lambda x: x[0])
    new_min_time = min(old_queue, key=lambda x: x[0])
    merged_q = execute_merge(r_new_queue, new_min_time, 
        old_queue, old_max_time, length)

    return merged_q

def execute_merge(n_q, n_mint, o_q, o_maxt, l):
    # gives the first time that we want to look for in the old queue
    first_t = int(datetime.datetime.now().strftime('%s')) - l

    temp_q = [p for p in o_q if p[0] >= n_mint]
    merged_q = [p for p in o_q if p[0] < n_mint and p[0] >= first_t]

    for i, q_pair in enumerate(temp_q):
        t, v = q_pair
        v += n_q[i][1] - v
        merged_q.append((t,v))

    merged_q += [p for p in n_q if p[0] > o_maxt]

    return merged_q

def reduce_q(q):
    r_q = []
    if len(q)>0:
        t, w = q[0]
        t_last = t
        w_last = w

        for t, w in q[1:]:
            if t == t_last:
                w_last += w
            else:
                r_q.append((t_last, w_last))
                w_last = w
            t_last = t
            
        r_q.append((t_last, w_last))

    return r_q