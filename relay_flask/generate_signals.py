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
    E = A + 4.4 * noise
    D = A + 0.9 * noise
    C = A + 1.8 * noise
    B = A + 1.3 * noise
    A = A + noise

    return A, B, C, D, E