from erlport.erlterms import Atom
from scipy.optimize import basinhopping
import numpy as np

class Bounds(object):
    '''Required for acceptance testing in scipy.optimize.basinhopping'''
    def __init__(self, xmin=1., xmax=0.):
        self.xmax = xmax
        self.xmin = xmin
    def __call__(self, **kwargs):
        x = kwargs["x_new"]
        tmax = bool(np.all(x <= self.xmax))
        tmin = bool(np.all(x >= self.xmin))
        return tmax and tmin

def optimize_path(paths, B_table, btg, prediction, dt=1.):
    '''Erlang Entry Point to Optimization Module'''
    B_table = parse_behaviours(behaviours)
    BTG = parse_edgelist(btg)
    F = parse_prediction(prediction)

    BestPath = best_path(paths, B_table, BTG, F, dt=dt)
    return BestPath.x

def best_path(paths, Behaviour_Table, BTG, F, dt=1.,
              maxiter=400):
    '''
    Perform the mixed ILP optimization (without queues, or memory), that yields
    the optimal behaviour transition through the BTG.

    :paths      -> iterable of path-iterables, path-domain for optimization
                   Each path-iterable contains only behaviour_id.
    :Behaviour_Table    -> Dict of the form {behaviour_id: <behaviour_vec>}
                   Must contain all behaviours in btg
    :btg        -> Behaviour Transition Graph, nodes are behaviour_ids,
                   dictionary of the form {(v_1, v_2): tau_1,2}
    :F          -> Prediction matrix, of shape (|b_vec|, n),
                    where n is int(T_max/dt)
    :dt         -> Prediction time-resolution
    '''
    # Given a particular path, find the optimal times to transition
    cum_F = np.cumsum(F, axis=1)

    Solutions = []
    t_max = int((F.shape[-1] - 1) * dt)
    for path in paths:
        (L, x0, bounds) = opt_params(path, Behaviour_Table,
                                    BTG, t_max, cum_F, dt=dt)
        result = basinhopping(L, x0, accept_test=bounds, stepsize=3*dt,
                                niter=maxiter, T=10.)
        Solutions.append(result)

    return min(((i, s) for i, s in enumerate(Solutions)), key=lambda x: x[1].fun)

def opt_params(path, B_Table, BTG, t_max, cum_F, dt):
    '''Generates the components necessary to completely specify (in the absence
    of a queuing/accumulating model) the best-path optimization routine.
    Returns:
    :Lagrangian Objective Function L(x) -> Contains a Barrier Component
    :x0     -> an initial realizeable solution
    :bounds -> a Bounds() object, that defines surrounding hyper-volume for x
    '''
    B = np.vstack(B_Table[bid] for bid in path)  # Behaviour Matrix (d,4)
    taus = transition_costs(path, BTG)
    x0 = initial_soln(path, t_max)

    cost = lambda x:  -1 * obj(x, B, cum_F, taus, dt=dt)  # Set to std form
    constraints = lambda x: 1000 * barrier(x, path, BTG)  # Constraint Programming
    L = lambda x: cost(x) + constraints(x)  # Combined Function

    bounds = Bounds(0., (cum_F.shape[-1] - 1) * dt)
    return L, x0, bounds


#  Parsers      ###############################################################
def parse_edgelist(edges):
    '''[(a, b, tau)] -> {(a, b): tau}'''
    return {(a, b): tau for a, b, tau in edges}

def parse_behaviours(behaviours, dtype=np.float32):
    '''[(bid, <bvec>)] -> {bid: <bvec>}'''
    return {bid: np.array(tuple(bvec), dtype=dtype) for bid, bvec in behaviours}

def parse_prediction(F):
    '''[[float]] -> np.array(...) of same shape'''
    return np.array(F)  # Might not work, will check back later


#  Optimization ###############################################################
def initial_soln(path, t_max):
    '''Evenly Distributed, no check for taus'''
    j = t_max / len(path)
    return np.array([(i + 1) * j for i in xrange(len(path) - 1)])

def transition_costs(path, btg):
    '''Sequence of transition costs associated with the prescribed path'''
    return [btg[(path[i], path[i+1])] for i in xrange(len(path) - 1)]

def range_sum(cum_F, a, b, penalty=-1000):
    '''Penalty brutally dominates any out-of-index operation...'''
    z = cum_F.shape[-1] - 1
    if (not 0 <= a <= z) or (not 0 <= b <= z):
        return np.ones(cum_F.shape[0]) * penalty
    return cum_F[..., b] - cum_F[..., a]

def flow_served(cum_F, times, costs, queue_model=None, dt=1.):
    '''Times: [t1, ..., td],
    costs: [t_{b0, b1}, t_{b1, b2}, ...]
    Returns the Fulfillment matrix associated with each behaviour segment.'''
    discr_index = lambda x: int(x / dt) - 1
    t_steps = map(discr_index, times)
    t_steps = [0] + t_steps
    t_steps.append(cum_F.shape[-1] - 1)  # t_max

    c_steps = map(discr_index, costs)
    c_steps = [0] + c_steps

    result = np.vstack([range_sum(cum_F, t_steps[i] + c_steps[i], t_steps[i + 1])
                        for i in xrange(len(costs) + 1)])
    return result

def obj(times, B, cum_F, costs, dt=1.):
    '''Objective Function for Hillclimbing'''
    Z = B * flow_served(cum_F, times, costs, dt=dt)
    return Z.sum()

def barrier(times, path, BTG):
    '''Handles Linear/causality Constraints with respect to transitions'''
    t = [0] + list(times)
    S = 0.
    for i in xrange(len(path) - 1):
        edge = (path[i], path[i + 1])
        tau = BTG[edge]
        S += min(0, (t[i + 1] - t[i] - tau))  # Only accrue if constraint is voilated
    return S
