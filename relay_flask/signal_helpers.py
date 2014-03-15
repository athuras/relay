import numpy as np

from scipy import stats

def generate_signals(amount):
    '''
    generate set of signals

    * amount: the number of samples to generate
    '''

    sample = stats.foldnorm(1, scale=3)
    
    A = sample.rvs(amount)
    
    # Noise
    n1 = stats.foldnorm(1, scale=2)
    n2 = stats.foldnorm(1, loc=5, scale=1)
    noise = n1.rvs(size=A.size) + n2.rvs(size=A.size)
    
    #lag = G.rvs(size=A.size)
    E = np.sort(A + 4.4 * noise)
    D = np.sort(A + 0.9 * noise)
    C = np.sort(A + 1.8 * noise)
    B = np.sort(A + 1.3 * noise)
    A = np.sort(A + noise)

    return A, B, C, D, E

def create_hist_dict(signals, dt=1.):
    '''
    Takes a set of arrays timestamps and turns them into histograms
    '''
    def create_hist(s, dt):
        s = np.array(s)

        min_time = s[0]
        max_time = np.max((100,s[-1] - min_time))
        s_mod = s - min_time

        l = np.linspace(0, max_time, max_time/dt)
        hist = np.histogram(s_mod, l)
        return {'flow': [hist[0].tolist(), np.round(hist[1],0).tolist()], 'min_time': min_time}

    hists = [create_hist(s, dt) for s in signals]
    return hists
