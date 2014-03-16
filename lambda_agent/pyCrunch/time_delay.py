import numpy as np
import prediction_helpers as prh
import predictions as pr

from datetime import datetime
from scipy import stats
from scipy.optimize import leastsq

def estimate_time_delay(nbr_signals, nbr_bhvrs, edges, inlet_signals, dt=1.):
    '''
    Given a set of data, need to learn the parameters of it's gamma

    :nbr_signals    -> signal observed at each neighbor inlet
    :nbr_bhvrs  -> current neighbor behaviour edge probs
    :edges    -> (inlet_node, outlet_node, (shape, loc, scale, gain))
    :inlet_signal   -> real signal observed at inlet node (inlet, outlet, signal)

    TODO: Make this do stuff
    '''

    NBPM = [prh.create_bhvr_prob_mtx(b) for b in nbr_bhvrs]

    lsq_solns = optimize(NBPM, nbr_signals, inlet_signals, edges, dt)

    # ADD LEARNING PARAM SO DOESN"T JUST TAKE NEW SOLUTION

    opt_time = datetime.now()

    return lsq_solns, opt_time

def remove_infeasible_events(signal_est, nbr_in_sig, edge_params, dt):
    '''
    Trim values from beginning and end of inlet signal that
    we expect didn't come from the behaviour
    '''

    # E = (shape * scale) + location
    buffering = 1.1
    shape, loc, scale, z = edge_params
    expectation = (shape * scale + loc) / buffering

    # number indicies to remove depends on sampling time
    index = np.round(expectation / dt, 0)

    len_out = signal_est.shape[0]
    len_in = nbr_in_sig.shape[0]

    # - determine number of elements to trim from end of inlet signal
    # - if expectation + outlet into future, then just take the end of 
    #   inlet array as max (i.e. don't remove anything)
    end_index = np.min([len_out + expectation, len_in])

    nbr_in_sig[:index] = 0
    nbr_in_sig[end_index:] = 0
    return nbr_in_sig

# Optimization
def optimize(NBPM, nbr_signals, inlet_signals, edges, dt):
    '''
    perform the optimzation.

    :NBPM    -> tensor of neighbor behvaiour probability matrices
    :nbr_signals    -> tensor of signals from neighbor inlets
    :inlet_signals    -> real signals measured at inlets
    :edges   -> each connecting edges time delay props
    :dt    -> sample rate
    '''

    leastsq_solns = [calc_lsq(NBPM[i], nbr_signals[i], inlet_signals[i],
        edge, dt) for i, edge in enumerate(edges)]

    return leastsq_solns

def residuals(p, y, x):
    '''
    calculates the error between real inlet signal and estimated

    :p    -> optmization params. gamma parameters and gain (a)
    :y    -> real signal
    :x    -> estimated outlet signal from neighbor
    '''

    shape, loc, scale, gain = p

    l = np.linspace(0, y.shape[0], y.shape[0])
    kernel = stats.gamma(shape, loc=loc, scale=scale).pdf(l)

    n = l.shape[0]
    err = y - np.convolve(kernel * gain, x, mode='full')[:n]

    return err

def calc_lsq(BPM, nbr_signal, inlet_signal, edge, dt):
    '''    Perform the least squares optimization    '''

    # edge[0] = my inlet, edge[1] = nbr out, edge[2] = (shape, loc, scale, gain)
    a, b, params = edge

    out_probs = prh.extract_probs(b, BPM)
    scaled_sigs = prh.transform_sigs(nbr_signal, out_probs)
    comb_sig_est = prh.combine_sig_ests(scaled_sigs)
    #td = prh.create_gamma(params, dt)

    x = comb_sig_est[0]
    y_real = remove_infeasible_events(x, inlet_signal, params, dt)

    # - Use old params as starting location for optimization
    # - y_real = the measured signal at inlet node
    # - x = estimated outgoing signal from neighbor
    lsq = leastsq(residuals, params, args=(y_real, x))

    return (a, b, lsq[0], x, y_real)