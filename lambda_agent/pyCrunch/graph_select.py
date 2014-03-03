from erlport.erlterms import Atom, List
from scipy.optimize import anneal
import numpy as np


def optimize_path(paths, behaviours, btg, prediction, dt=1.):
    '''Erlang Entry Point to Optimization Module'''
    B_table = parse_behaviours(behaviours)
    BTG = parse_edgelist(btg)
    F = parse_prediction(prediction)

    BestPath = best_path(paths, B_table, BTG, F, dt=dt)
    return BestPath

def best_path(paths, Behaviour_Table, BTG, F, dt=1.):
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
    for path in paths:
        B = np.vstack(Behaviour_Table[bid] for bid in path)  # Behaviour Matrix (d,4)
        taus = gen_costs(path, BTG)
        x = initial_soln(path, int(F.shape[-1] * dt)

        cost = lambda x:  -1 * obj(x, B, cum_F, taus, dt=dt)  # Set to std form
        barrier = lambda x: 50 * barrier(x, path, BTG)  # Constraint Programming
        L = lambda x: cost(x) + barrier(x)  # Combined Function

        lower = np.cumsum(np.array(taus))
        upper = tmax - lower

        sol = anneal(L, x, upper=upper, lower=lower, maxiter=250)
        Solutions.append(sol)
    return min(Solutions, key=lambda x: x[1])


#  Parsers      ###############################################################
def parse_edgelist(edges):
    '''[(a, b, tau)] -> {(a, b): tau}'''
    return {(a, b): tau for a, b, tau in edges}

def parse_behaviours(behaviours):
    '''[(bid, <bvec>)] -> {bid: <bvec>}'''
    return {bid: np.array(tuple(bvec)) for bid, bvec in behaviours}

def parse_prediction(F):
    '''[[float]] -> np.array(...) of same shape'''
    return np.array(F)  # Might not work, will check back later


#  Optimization ###############################################################
def initial_soln(path, t_max):
    '''Evenly Distributed, no check for taus'''
    j = t_max / len(path)
    return np.array(i + j for i in xrange(len(path)))

def gen_costs(path, btg):
    return [btg[(path[i], path[i+1])] for i in xrange(len(path) - 1)]

def range_sum(cum_F, a, b):
    return cum_F[..., b] - cum_f[..., a]

def time_costs(cum_F, times, costs, dt=1.):
    '''Times: [t1, ..., td],
    costs: [t_{b0, b1}, t_{b1, b2}, ...]'''
    discr_index = lambda x: int(x / dt)
    t_steps = map(discr_index, times)
    t_steps = [0] + t_steps
    t_steps.append(cum_F.shape[-1])  # t_max

    c_steps = map(discr_index, costs)
    c_steps = [0] + c_steps

    result = np.vstack([range_sum(cum_F, t_steps[i] + c_steps[i], t_steps[i + 1])
                        for i in xrange(len(costs))])
    return result

def obj(times, B, cum_F, costs, dt=1.):
    '''Objective Function for Hillclimbing'''
    Z = B * time_costs(cum_F, times, costs, dt=dt)
    return Z.sum()

def barrier(times, path, taus):
    '''Handles Linear/causality Constraints with respect to transitions'''
    t = [0] + times
    S = 0.
    for i in xrange(len(path) - 1):
        edge = (path[i], path[i + 1])
        tau = taus[edge]
        S += abs(times[i + 1] - times[i] + tau)**2
    return S
