import numpy as np

from datetime import datetime
from scipy import stats

def gen_prediction(nbr_signals, nbr_bhvrs, edges, dt=1.):
    '''
    :nbr_signals    -> real signals seen at each neighbors inlets
    :nbr_bhvrs   -> current bhvr (& probs, or pull from db)
    :edges  -> set of all connecting edges and their gamma props
    :dt    -> sample rate
    '''

    # tensor of BPMs for each neighbor
    NBPMs = [create_bhvr_prob_mtx(nb) for nb in nbr_bhvrs]

    SIG = nbr_signals
    #NP = nbr_predictions # or function call to get them
    TDs = create_gammas(edges, dt)
    prediction = calc_prediction(NBPMs, SIG, TDs, dt)

    prediction_time = datetime.now()

    return prediction, prediction_time

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
    '''
    predicted_signals = []
    for i, edge in enumerate(TD):
        # edge[0] = my inlet, edge[1] = nbr out, edge[2] = TD

        out_probs = extract_probs(edge[1], NBPM[i])
        scaled_sigs = transform_sigs(SIG[i], out_probs)
        comb_sig_est = combine_sig_ests(scaled_sigs)
        extended_td = extend_kernel_sig(edge[2])

        predicted_signals.append(
            np.convolve(extended_td, comb_sig_est[0], mode='same'))

    return predicted_signals


# Parsing Functions
def create_bhvr_prob_mtx(bhvr):
    bhvr_prob_mtx = np.zeros([4,4])
    for edge in bhvr:
        bhvr_prob_mtx[edge[0], edge[1]] = edge[2]

    return bhvr_prob_mtx

# Distribution and Sampling Functions 
def create_gammas(edges, dt):
    ''' (a, b, (shape, loc, scale)) -> ((a, b, [sampled gamma],) '''

    max_time = 150 # could do something smarter here
    l = np.linspace(0, max_time, max_time/dt)

    return [(a, b, stats.gamma(g[0], g[1], g[2]).pdf(l)) for a, b, g in edges]


# Helper Functions
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
    Accepts a tuple of signals from each inlet of a neighbor and the current behaviour
    probability matrix, and returns a scaled signal.

    :signal    -> histogram of counts at each neighbor inlet
    :prob    -> the outlets connecting probabilities

    takes the last element of histogram off to keep lengths same.
    '''

    trans_sigs = []
    for i in range(len(prob)):
        trans_sigs.append((signal[i][0] * prob[i], signal[i][1][0:-1]))
    
    return trans_sigs

def combine_sig_ests(signals):
    '''
    combines the scaled signals.

    :signals    -> tuple of scaled signals
    '''

    total_est = np.zeros(len(signals[0][0]))
    for i in range(len(signals)):
        total_est += signals[i][0]
    
    return (total_est, signals[0][1])

def extend_kernel_sig(kernel):
    '''
    Takes the kernel and double its length, so we can keep its time delay

    :kernel    -> time delay signal to convovle with combined neighbor signals
    '''

    return np.concatenate((np.zeros(len(kernel)), kernel))