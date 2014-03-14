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

def remove_infeasible_events(signal_est, inlet_signal, edge_params, dt):
    '''
    Need to remove events from nbr that could not have reached inlet by the
    time the reading was taken. Also trim values from beginning of inlet that
    could not have come from the behaviour
    '''

    # E = (shape * scale) + location
    buffering = 1.1
    shape, loc, scale, z = edge_params
    expectation = (shape * scale + loc) / buffering

    # number indicies to remove depends on sampling time
    index = np.round(expectation / dt, 0)

    len_out = signal_est.shape[0]
    len_in = inlet_signal.shape[0]

    # - determine number of elements to trim from end of inlet signal
    # - if expectation + outlet into future, then just take the end of 
    #   inlet array as max (i.e. don't remove anything)
    end_index = np.min([len_out + expectation, len_in])

    signal_est[:index] = 0
    signal_est[end_index:] = 0

    return signal_est

# Optimization
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
    err = y - np.convolve(kernel, x, mode='full')[:n]

    return err

def optimize(NBPM, nbr_signals, inlet_signals, edges, dt):
    '''
    perform the optimzation.

    :NBPM    -> tensor of neighbor behvaiour probability matrices
    :nbr_signals    -> tensor of signals from neighbor inlets
    :inlet_signals    -> real signals measured at inlets
    :edges   -> each connecting edges time delay props
    :dt    -> sample rate
    '''

    leastsq_solns = []
    for i, edge in enumerate(edges):
        # edge[0] = my inlet, edge[1] = nbr out, edge[2] = (shape, loc, scale, gain)
        a, b, params = edge

        out_probs = prh.extract_probs(b, NBPM[i])
        scaled_sigs = prh.transform_sigs(nbr_signals[i], out_probs)
        comb_sig_est = prh.combine_sig_ests(scaled_sigs)
        #td = prh.create_gamma(params, dt)

        x = remove_infeasible_events(comb_sig_est[0], inlet_signals[i], params, dt)
        y_real = inlet_signals[i]

        # - Use old params as starting location for optimization
        # - y_real = the measured signal at inlet node
        # - x = estimated outgoing signal from neighbor
        lsq = leastsq(residuals, params, args=(y_real, x))

        leastsq_solns.append((a, b, lsq[0]))

    return leastsq_solns
