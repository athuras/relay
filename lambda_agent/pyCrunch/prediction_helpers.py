import numpy as np

from scipy import stats

def create_bhvr_prob_mtx(bhvr):
    bhvr_prob_mtx = np.zeros([4,4])
    for edge in bhvr:
        bhvr_prob_mtx[edge[0], edge[1]] = edge[2]

    return bhvr_prob_mtx

def create_gamma(edge, dt):
    ''' (a, b, (shape, loc, scale, gain)) -> ((a, b, [sampled gamma],) '''

    a, b, params = edge
    max_time = 100 # could do something smarter here
    
    l = np.linspace(0, max_time, max_time/dt)

    return (a, b, stats.gamma(params[0], params[1], params[2]).pdf(l))

def extract_probs(outlet, probs):
    '''
    eliminates the probably of the outlet node itself, 
    only keeping ones that feed it.

    :outlet    -> outlet on neighbor
    :probs    -> neighbors behaviour probability matrix
    '''
    outlet_probs = probs[:,outlet]
    correct_probs = [v for k,v in enumerate(outlet_probs) if k != outlet]
    
    return np.array(correct_probs)

def transform_sigs(signal, prob):
    '''
    Accepts a tuple of signals from each inlet of a neighbor and the current
    behaviour probability matrix, and returns a scaled signal.

    :signal    -> histogram of counts at each neighbor inlet
    :prob    -> the outlets connecting probabilities

    takes the last element of histogram off to keep lengths same.
    '''

    trans_sigs = []
    for i, p in enumerate(prob):
        trans_sigs.append((signal[i][0] * p, signal[i][1][0:-1]))
    
    return trans_sigs

def combine_sig_ests(signals):
    '''
    combines the scaled signals.

    :signals    -> tuple of scaled signals
    '''

    total_est = np.zeros(len(signals[0][0]))
    for signal in signals:
        total_est += signal[0]
    
    return (total_est, signals[0][1])

def extend_kernel_sig(kernel):
    '''
    Takes the kernel and double its length, so we can keep its time delay

    :kernel    -> time delay signal to convovle with combined neighbor signals
    '''

    return np.concatenate((np.zeros(len(kernel)), kernel))