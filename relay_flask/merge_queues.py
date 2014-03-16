import numpy as np
import signal_helpers as sighelp

def fetch_queues(int_id=None):
    ''' get queues from erlang and turn them into histos for front end'''

    # new queues, queue dict

def make_queues():
    A, B, C, D, E = sighelp.generate_signals(250)
    signals = [A, B, C, D]
    qs = sighelp.create_hist_dict(signals, 1)
    return {'in': qs, 'out': qs, 'r_out': qs}

def merge_simulated_queues(int_id, sim_queues, db):
    #pull from database the timestamps for this intersection
    #arrange them into order based on node id
    #merge with new queue
    #write new queue back to database

    def get_queues(int_id):
        return [2,3,2,2,3,2,1,2]

    # new_queues = [inlet_queues, outlet_queues, remote_outlet_queues]
    new_queues = get_queues(int_id)

    updated_queues = [merge_queues(q, sim_queues[i]) for i, q in enumerate(new_queues)]

def merge_queues(new_qs, old_qs):
    '''take two sets of queues (one for each node) and merge them'''
    return [initiate_merge(nq, old_qs[i]) for i, q in enumerate(new_qs)]

def initiate_merge(new_queue, old_queue):
    ''' merge the two queues from same node'''

    r_new_queue = reduce_q(new_queue)
    old_max_time = max(r_new_queue, key=lambda x: x[0])
    new_min_time = min(old_queue, key=lambda x: x[0])
    merged_q = execute_merge(r_new_queue, new_min_time, old_queue, old_max_time)

    return merged_q

def execute_merge(n_q, n_mint, o_q, o_maxt):
    temp_q = [p for p in o_q if p[0] >= n_mint]

    merged_q = [p for p in o_q if p[0] < n_mint]
    for i, q_pair in enumerate(temp_q):
        t, v = q_pair
        v += n_q[i][1] - v
        merged_q.append((t,v))

    merged_q += [p for p in n_q if p[0] > o_maxt]

    return merged_q

def reduce_q(q):
    r_q = []
    t_last = None
    v_last = 0

    for t, v in q:
        if t == t_last:
            v_last += v
        else:
            r_q.append((t_last, v_last))
            v_last = v

        t_last = t

    return r_q