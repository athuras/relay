# Queue Modelling and Simulation for use with Traffic Signals
import numpy as np

#  ODE Simulator ##############################################################
def ode_sim(x0, dx_dt, T, dt=1., dx_dt_args=[], dx_dt_kwargs={}):
    '''Simulate the first-order ODE dx_dt over the time-period T at 1/fs=dt,
    returns simulation of shape (T/dt, x0.size),

    Returns the state simulation, and the derivatives for each time step.
    '''
    d = x0.size
    n = int(T / dt)
    sim = np.empty(shape=(n, d), dtype=float)
    deltas = np.empty_like(sim)

    sim[0,:] = x0
    deltas[0,:] = 0

    for i in xrange(1, n):
        deltas[i,:] = dx_dt(sim[i-1, :], *dx_dt_args, **dx_dt_kwargs)
        sim[i,:] = sim[i-1,:] + deltas[i,:]

    return sim, deltas

#  Accumulators ###############################################################
def integrator(F, a, b, axis=1):
    return F[:, a:b].sum(axis)

def quadratic_integrator(F, a, b, alpha=0.2, dt=1.):
    '''Quadratic Integrator, essentially equivalent to integrate(alpha*t*F(t))
    Assumes axis=1 from a to b'''
    t = np.arange(int(b * dt), int(a * dt), -dt)
    t *= alpha * t
    cum = F[:, a:b] * t
    return cum.sum(axis=1)

def fixed_cost_integrator(F, a, b, cost, dt=1.):
    '''Used to force cyclic behaviour by accruing cost even for unserved inlets
    '''
    return integrator(F, a, b) + cost * dt


#  Relief Models ##############################################################
def linear_relief(a, B, rate=0.5, dt=1.):
    '''Rectified linear relief, the predicted output is a constant source of
    traffic at rate r.
    :a      -> accumulated values (vector of size d)
    :B      -> Mask, what will actually be released
    :rate   -> rate, r_i defines the relief rate for acc_i. Will broadcast.
    :dt     -> step-size

    Returns the delta'''
    mask = np.ones_like(a)
    mask[a == 0] = 0.
    mask *= B
    return -rate * dt * mask

def capillary_relief(a, B,  r_max=2.2, r_sat=0.1, growth_rate=2., dt=1.):
    '''Relief model for congested traffic (large queues),
    takes into account acceleration info
    :a      -> current accumulated value
    :B      -> What will actually be released, B = 1 - phi
    :r_max  -> maximum depletion rate
    :r_sat  -> minimum depletion rate (saturated)
    :growth_rate -> rate parameter for exponential decay
    :dt     -> time_step'''
    return -(r_max * np.exp(-np.abs(a) * growth_rate) + r_sat) * dt * B

#  Top-Level Queue Simulators #################################################
def cascading_relief(F, path, times, costs, BTable, Acc0, dt=1.,
        acc_model=integrator,
        acc_model_kwargs={},
        relief_model=linear_relief,
        relief_model_kwargs={"rate": 0.5},
        **kwargs):
    '''Do Fancy stuff that returns the transformed version of F
    :F          -> Prediction 2d-array of shape (d, int(T/dt))
    :times      -> [t_0, t_1, ..., t_d]
    :costs      -> [tau_{0, 1}, tau_{1, 2}, ..., tau_{d-1, d}]
    :B          -> {bid: array([])}
    :Acc0       -> array([a_0, a_1, ..., a_d]) current accumulator values
    :acc_model  -> {integrator, fixed_cost_integrator, quadratic_integrator}
    :relief_model -> {linear_relief, capillary_relief}

    There is NO LEAKING in the queues, which means that we are making the
    assumption that traffic incident on an under-served inlet is always
    accumulating, this is reasonable if cars must stop to 'leak' from the
    inlets, but doesn't represent the true 'wave-particle duality' of
    traffic under Relay.

    Returns: F', which accounts for stationary queue effects
    '''
    t_to_index = lambda t: int(t / dt)
    d, N = F.shape
    T = N * dt

    Z = F.copy()  # Don't obliterate the original F

    time_idx = [0] + map(t_to_index, times)
    cost_idx = [0] + map(t_to_index, costs)
    time_idx.append(N - 1)  # The final time within the prediction

    # For each state S, simulate the Queue effects and append to downstream
    Acc = Acc0
    deltas = 0.
    for i in xrange(len(costs) + 1):
        a, b = time_idx[i] + cost_idx[i], time_idx[i + 1]
        Tsim = (b - a) * dt  # The simulation time for this round
        if Tsim <= dt:
            continue

        B = BTable[path[i]]  # What is being served
        phi = 1 - B          # What is being blocked/not served

        # Simulate the queues being depleted over Tsim [a to b].
        sim, deltas = ode_sim(Acc, relief_model, T=Tsim, dt=dt,
                              dx_dt_args=[B],
                              dx_dt_kwargs=relief_model_kwargs)

        # Rectify all the things
        sim = np.maximum(0, sim)
        deltas = np.minimum(0, deltas)
        Acc = sim[-1,:]  # The final value of the queue after Tsim

        # Set the starting queue lengths for the next period
        Acc += acc_model(Z, a, b, **acc_model_kwargs) * phi

        # Alter the prediction for this time period with the new
        # Queue-depletion information
        Z[:, a:b] *= np.c_[B]
        Z[:, a:b] -= deltas.T

    return Z, Acc
