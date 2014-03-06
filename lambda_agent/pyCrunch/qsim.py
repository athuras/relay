# Queue Modelling and Simulation for use with Traffic Signals
import numpy as np

#  Top-Level Queue Simulators #################################################
def cascading_relief(F, path, times, costs, BTable, Acc0, dt=1.,
        acc_model=integrator,
        acc_model_args=[],
        acc_model_kwargs={},
        relief_model=linear_relief,
        relief_model_args=[],
        relief_model_kwargs={},
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

    Returns: F', which accounts for stationary queue nonlinearities
    '''
    t_to_index = lambda t: int(t / dt) - 1
    d, N = F.shape
    T = N * dt

    Z = F.T.copy()  # Don't obliterate the original F

    time_idx = [0] + map(t_to_index, times)
    cost_idx = [0] + map(t_to_index, costs)
    time_idx.append(N - 1)  # The final time within the prediction

    # For each state S, simulate the Queue effects and append to downstream
    Acc = Acc0
    deltas = 0.
    for i in xrange(len(costs) + 1):
        a, b = time_idx[i] + cost_idx[i], time_idx[i + 1]
        Tsim = (b - a) * dt  # The simulation time from last round
        phi = (1 - BTable[path[i]])  # What is going to be accumulated
        # Simulate the queues being depleted over Tsim [a to b].
        sim, deltas = ode_sim(Acc, relief_model, T=Tsim, dt=dt,
                              *relief_model_args,
                              **relief_model_kwargs)

        Acc = sim[-1,:]  # The final value of the queue after Tsim

        # Alter the prediction for this time period with the new
        # Queue-depletion information
        Z[a:b] -= phi * deltas

        # Accumulate over the period
        Acc += acc_model(Z, a, b, *acc_model_args,
                        **acc_model_kwargs)
        Acc *= phi

    return Z

#  ODE Simulator ##############################################################
def ode_sim(x0, dx_dt, T, dt=1., dx_dt_args=[], dx_dt_kwargs={},
        delta_only=False):
    '''Simulate the first-order ODE dx_dt over the time-period T at 1/fs=dt,
    returns simulation of shape (T/dt, x0.size),

    Returns the state simulation, and the derivatives for each time step.
    NOTE: Rectifies!!!'''
    d = x0.size
    n = int(T / dt)
    sim = np.empty(shape=(n, d), dtype=float)
    deltas = np.empty_like(sim)

    sim[0,:] = x0
    deltas[0,:] = 0

    for i in xrange(1, n):
        deltas[i,:] = np.maximum(dx_dt(sim[i-1, :],
                                *dx_dt_args, **dx_dt_kwargs), 0)
        sim[i,:] = sim[i-1,:] + deltas[i,:]

    return sim, deltas

#  Accumulators ###############################################################
def integrator(F, a, b, axis=1):
    return np.sum(F[a:b], axis=axis)

def quadratic_integrator(F, a, b, alpha=0.2, dt=1.):
    '''Quadratic Integrator, essentially equivalent to integrate(alpha*t*F(t))
    Assumes axis=1 from a to b'''
    t = np.arange(int(b * dt), int(a * dt), -dt)
    t *= alpha * t
    cum = F[:, a:b+1] * t
    return cum.sum(axis=1)

def fixed_cost_integrator(F, a, b, cost, dt=1.):
    '''Used to force cyclic behaviour by accruing cost even for unserved inlets
    '''
    return integrator(F, a, b) + cost * dt
#  Relief Models ##############################################################
def linear_relief(_, rate=0.5, dt=1.):
    '''Rectified linear relief, the predicted output is a constant source of
    traffic at rate r.
    :acc    -> accumulated values (vector of size d)
    :rate   -> rate, r_i defines the relief rate for acc_i. Will broadcast.
    :dt     -> step-size

    Returns the delta'''
    return -r * dt

def capillary_relief(a, r_max, r_sat, growth_rate, dt=1.):
    '''Relief model for congested traffic (large queues),
    takes into account acceleration info
    :a      -> current accumulated value
    :r_max  -> maximum depletion rate
    :r_sat  -> minimum depletion rate (saturated)
    :growth_rate -> rate parameter for exponential decay
    :dt     -> time_step'''
    return -(r_max * np.exp(-a * growth_rate) + r_sat) * dt
