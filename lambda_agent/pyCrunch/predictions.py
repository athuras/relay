import numpy as np
import prediction_helpers as prh

from datetime import datetime
from scipy import stats

def gen_prediction(nbr_signals, nbr_bhvrs, edges, dt=1.):
    '''
    :nbr_signals    -> real signals seen at each neighbors inlets
    :nbr_bhvrs   -> current bhvr (& probs, or pull from db)
    :edges  -> set of all connecting edges and their gamma props
        --> (a, b, (shape, loc, scale, gain))
    :dt    -> sample rate
    '''

    # tensor of BPMs for each neighbor
    NBPMs = [prh.create_bhvr_prob_mtx(nb) for nb in nbr_bhvrs]
    TDs = [prh.create_gamma(edge, dt) for edge in edges]

    predictions = calc_prediction(NBPMs, nbr_signals, TDs, dt)
    prediction_time = datetime.now()

    return predictions, prediction_time

def gen_full_prediction(nbr_predictions, nbr_signals, nbr_bhvrs, edges, t, dt=1.):
    '''
    Calcualtes prediction t seconds into the future. Uses prediction for this
    plus predictions at neighboring nodes to create full prediction.

    :nbr_predictions    -> predictions from all neighbors (Pi's)
    :nbr_signals    -> real signals seen at each neighbors inlets
    :nbr_bhvrs  -> current bhvr (& probs, or pull from db)
    :edges  -> set of all connecting edges and their gamma props
    :t    -> length of predition in seconds
    :dt    -> sample rate
    '''

    prediction, pred_time = gen_prediction(nbr_signals, nbr_bhvrs, edges, td)
    steps = t/dt # how many items we want to pull from the full prediction

    full_preditions = []
    for pred in predictions:
        if len(pred) > steps:
            full_preditions.append(pred[0:steps])
        else:
            # get predition from neighbors to extend knowledge
            # TODO:
            #   - make a call to erlang to get preds from nbr or use what is passed
            full_preditions.append(pred)

    return full_preditions, pred_time


def calc_prediction(NBPM, SIG, TD, dt):
    '''
    Calculates the prediction for t seconds in the future at intersection

    :NBPM    -> tensor of neighbor behvaiour probability matrices
    :SIG    -> tensor of signals from neighbor inlets
    :TD    -> each connecting edges time delay properties
    :dt    -> sample rate
    '''
    predicted_signals = []
    for i, edge in enumerate(TD):
        # edge[0] = my inlet, edge[1] = nbr out, edge[2] = TD

        out_probs = prh.extract_probs(edge[1], NBPM[i])
        scaled_sigs = prh.transform_sigs(SIG[i], out_probs)
        comb_sig_est = prh.combine_sig_ests(scaled_sigs)
        td = edge[2]

        n = td.shape[0]
        predicted_signals.append(
            np.convolve(td, comb_sig_est[0], mode='full')[:n])

    return predicted_signals