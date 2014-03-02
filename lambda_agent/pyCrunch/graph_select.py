import numpy as np
import igraph as ig
from erlport.erlterms import Atom, List
from collections import defaultdict


def find_path(start, BTG, B, P, d):
    '''Find the best path through BTG starting at *start*,
    * BTG: Behaviour Transition Graph [(v1, v2, tau_{v1,v2})]
    * B: Behaviour Vector Matrix of shape (n, d).
    * P: Prediction of shape (d, T/dt).
    * d: Max Depth (solution is of length d + 1).
    * start: initial behaviour.

    solves for D = [B_0, t_0, ..., B_d, t_d]
    s.t. t_i >= T_{i-1} - tau_{i, i-1},
    where tau_{a, b} = dead (red) time between states a, b.
    '''
    Nt, d = len(P), len(P[0])
    P = np.array(P).reshape((Nt, d))
    P_cumulative = np.cumsum(P, axis=1)

    BTG # A graph, probably as an edge-list
    return []


def best_path(paths, behaviours, btg, prediction, dt=1., tmax=200.):
    '''
    Perform the mixed ILP optimization (without queues, or memory), that yields
    the optimal behaviour transition through the BTG.

    :paths      -> iterable of path-iterables, path-domain for optimization
                   Each path-iterable contains only behaviour_id.
    :behaviours -> map of behaviour_id to  behaviour_vec
                   Must contain all behaviours in btg
    :btg        -> Behaviour Transition Graph, nodes are behaviour_ids,
                   Edges are of the form {v_1, v_2, tau_{1,2}}
    :prediction -> matrix, of shape (|b_vec|, n), where n is int(T_max/dt)
    '''
    from scipy.optimize import anneal  # We work hard, we play hard.

    B_table = parse_behaviours(behaviours)
    BTG = parse_edgelist(btg)
    F = parse_prediction(prediction)

    # Given a particular path, find the optimal times to transition
    cum_F = np.cumsum(F, axis=1)

    Solutions = []
    for path in paths:
        B = np.vstack(B_table[bid] for bid in path)  # Behaviour Matrix (d,4)
        taus = gen_costs(path, BTG)
        x = initial_soln(path, t_max)
        def cost(times): return -1 * obj(times, B, cum_F, taus, dt=dt)

        lower = np.cumsum(np.array(taus))
        upper = tmax - lower
        sol = anneal(cost, x, upper=upper, lower=lower, maxiter=250)
        Solutions.append(sol)
    (tmin, Jmin, T, feval,
        iters, accept, status) =  min(Solutions, key=lambda x: x[1])
    return tmin


#  Parsers      ###############################################################
def parse_edgelist(edges):
    '''[(a, b, tau)] -> {(a, b): tau}'''
    return {(a, b): tau for a, b, tau in edges}

def parse_behaviours(behaviours):
    '''[(bid, <bvec>)] -> {bid: <bvec>}'''
    return {bid: bvec for bid, bvec in behaviours}

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
