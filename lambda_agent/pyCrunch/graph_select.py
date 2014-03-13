from erlport.erlterms import Atom
from scipy.optimize import basinhopping
import numpy as np
import qsim

class Bounds(object):
    '''Required for acceptance testing in scipy.optimize.basinhopping'''
    def __init__(self, xmin, xmax, costs):
        self.xmax = xmax
        self.xmin = xmin
        self.costs = costs

    def is_valid(self, x):
        tmax = bool(np.all(x <= self.xmax))
        tmin = bool(np.all(x >= self.xmin))
        in_order = [x[i] + c <= x[i+1] for i, c in enumerate(self.costs[1:])]
        in_order.append(x[0] <= self.costs[0])
        return tmax and tmin and all(in_order)

    def __call__(self, **kwargs):
        x = kwargs["x_new"]
        return self.is_valid(x)

    def SLSQP_constraints(self):
        '''Return inequality constraints for SLSQP,
        in particular, assert that 0 >= x_i - x_i-1 forall i'''
        funs = [lambda x: x[i + 1] - x[i] + c
                for i, c in enumerate(self.costs[1:])]
        funs.append(lambda x: x[0] + self.costs[0])
        funs += [lambda x: x[i] for i in xrange(len(self.costs))]
        funs += [lambda x: -x[i]]

        # im matrix form
        n = len(self.costs)
        # -x_i <= 0
        neg = np.identity(n) * -1
        rhs1 = np.ones(n) * self.xmin
        rhs1[0] += self.costs[0]
        # tmax constraints
        tmax = np.identity(n)
        rhs2 = np.ones(n) * self.xmax
        # cost constraints
        A = np.vstack((neg, tmax))
        b = np.hstack((rhs1, rhs2))
        if n >= 2:
            root = [1, -1] + [0] * (n - 2)
            z = np.vstack([np.roll(root, i) for i in xrange(n-1)])
            rhs3 = np.array(self.costs[1:])
            A = np.vstack((A, z))
            b = np.hstack((b, rhs3))
        return {"slsqp": {'type': 'ineq', 'fun': lambda x: b - np.dot(A, x)},
                "cobyla": [{'type': 'ineq', 'fun': f} for f in funs]}

    def SLSQP_bounds(self):
        '''Return bounds as sequence'''
        return [(self.xmin, self.xmax) for i in xrange(len(self.costs))]



class Stepper(object):
    def __init__(self, bounds, stepsize=10, max_iter=20, deflate=0.5):
        self.bounds = bounds
        self.stepsize = stepsize
        self.max_iter = max_iter
        self.deflate = deflate

    def __call__(self, x):
        y = None
        for i in xrange(self.max_iter):
            B = self.deflate ** (i + 1)
            r = self.stepsize * B
            u =  np.random.uniform(-r, r, x.shape)
            if self.bounds.is_valid(x + u):
                x += u
                return x
        return x


def optimize_path(paths, behaviours, btg, prediction, dt=1.):
    '''Erlang Entry Point to Optimization Module'''
    B_table = parse_behaviours(behaviours)
    BTG = parse_edgelist(btg)
    F = parse_prediction(prediction)

    return best_path(paths, B_table, BTG, F, dt=dt)

def best_path(paths, Behaviour_Table, BTG, F, dt=1.,
              maxiter=400, Acc0=None, method="COBYLA"):
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
    :Acc0       -> Initial queue Accumulator (queue length) value, defaults 0.
    '''
    # Given a particular path, find the optimal times to transition
    Acc0 = np.zeros(F.shape[0]) if Acc0 is None else Acc0

    Solutions = []
    t_max = int((F.shape[-1] - 1) * dt)
    initial_T = F.sum() / len(paths[0])
    for path in paths:
        L, x0, bounds, step_taker = opt_params(path, Behaviour_Table,
                                    BTG, t_max, F, dt=dt, Acc0=Acc0)

        minimizer_kwargs = {'method': method, 'bounds': bounds.SLSQP_bounds(),
                            'constraints': bounds.SLSQP_constraints()[method.lower()]}
        result = basinhopping(L, x0.copy(),
                            accept_test=bounds,
                            take_step=step_taker, stepsize=10*dt,
                            niter=maxiter, T=initial_T,
                            interval=20,
                            minimizer_kwargs=minimizer_kwargs)
        Solutions.append(result)

    i, BestPath =  min(((i, s) for i, s in enumerate(Solutions)),
                        key=lambda x: x[1].fun)
    return paths[i], BestPath


def opt_params(path, BTable, BTG, t_max, F, dt, Acc0,
        q_acc_model=qsim.integrator, q_acc_model_args=[], q_model_kwargs={},
        q_relief_model=qsim.linear_relief,
        deadtime_penalty=4):
    '''Generates the components necessary to completely specify
    best-path optimization routine. (With a queue model)

    Returns:
    :Lagrangian Objective Function L(x) -> Contains a Barrier Component
    :x0     -> an initial realizeable solution
    :bounds -> a Bounds() object, that defines surrounding hyper-volume for x
    '''
    B = np.vstack(BTable[bid] for bid in path)  # Behaviour Matrix (d,4)
    taus = transition_costs(path, BTG)
    x0 = initial_soln(path, t_max)
    bounds = Bounds(0., (F.shape[-1] - 1) * dt, taus)

    def cost(x, p=deadtime_penalty):
        '''Simulate the queue effects, and then evaluate the objective function
        on the simulation result'''
        avg_rates = F.sum(1) / F.shape[1]
        Z, Acc = qsim.cascading_relief(F, path, x, costs=taus, BTable=BTable,
                Acc0=Acc0, relief_mode_kwargs={"rate": 0.5})
        cum_Z = np.cumsum(Z, axis=1)

        Deadtimes = np.where(Z == 0, 0, 1).sum(1)

        return -1 * obj(x, B, cum_Z, taus, dt=dt) + avg_rates.dot(Deadtimes) ** 2


    step_taker = Stepper(bounds, 10, 20)
    return cost, x0, bounds, step_taker


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
    t_steps = [0] + map(discr_index, times)
    t_steps.append(cum_F.shape[-1] - 1)  # t_max

    c_steps = [0] + map(discr_index, costs)

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
