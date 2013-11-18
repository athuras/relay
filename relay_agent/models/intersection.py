from itertools import imap
from itertools import izip
from utils import cache
import numpy as np

class Intersection(object):
    '''
    The model for the Simple Relay Intersection, designed assuming it's symmetric,
    may still work if it isn't.
    '''
    _default_o_key = lambda x: x['orientation']
    _default_p_key = lambda x: x['probability']

    def __init__(self, complete_graph, inputs, outputs, behaviours=[], **kwargs):
        '''Create a new intersection from:
        *complete_graph: a Graph object (NetworkX, or igraph).
            The node_ids of the inlets/outlets should correspond to the
            provided ingress/egress ports.
            The edges should have an 'orientation' property, corresponding from
            the vector subtraction of their terminal and initial nodes' positions
        *inputs: list of input nodes, should correspond to the inlets of the
            complete graph.
        *outputs: list of output nodes, should correspond to the outlets of the
            complete graph.
        *behaviours: list of behaviours

        kwargs:
        *o_key: function that is used to get the orientation vector from an edge,
            defaults to lambda x: x['orientation']
        *p_key: function that is used to get the internal probability from an
            edge. defaults to lambda x: x['probability']
        '''
        self.o_key = kwargs.get('o_key', Intersection._default_o_key)
        self.p_key = kwargs.get('p_key', Intersection._default_p_key)

        Intersection.validate_graph(complete_graph, self.o_key)

        self.complete_graph = complete_graph
        self.inputs = inputs
        self.outputs = outputs
        self.behaviours = behaviours

    @staticmethod
    def validate_graph(G):
        '''Confirms that the graph won't break shit'''
        for (i, o) in izip(G.in_degree_iter(), G.out_degreee_iter()):
            assert min(i[1], o[1]) == 0

        for edge in G.edges_iter(data=True):
            assert self.p_key(edge[2]) is not None
            assert self.o_key(edge[2]) is not None


class Behaviour(object):
    _default_p_key = lambda x: x['probability']
    _default_o_key = lambda x: x['orientation']
    _default_nid_key = lambda x: x.nid

    def __init__(self, graph, ingress, egress, *args, **kwargs):
        '''Create a new Behaviour Object:
        *graph: the 'legal' traversal graph corresponding to this behaviour
        *ingress: the input objects of the *complete* graph
        *egress: the output objects of the *complete* graph
        kwargs: various key-functions
            p_key: internal edge probability
            o_key: internal edge orientation
            nid_key: given an ingress/egress object, what node_id does it have?
        '''
        self.p_key = kwargs.get('p_key', Behaviour._default_p_key)
        self.o_key = kwargs.get('o_key', Behaviour._default_o_key)
        self.nid_key = kwargs.get('nid_key', Behaviour._default_nid_key)

        self.graph = graph
        self.ingress = ingress
        self.egress = egress

    @property
    def static_bias(self):
        '''Returns the vector sum of all oriented edges, scaled by their
        internal probabilities. A behaviour with a strong bias is one
        which encourages traffic to flow a certain direction.
        A roundabout would have zero-bias in a symmetric intersection.'''
        normalize = lambda x: x / np.linalg.norm(x)
        return sum(self.p_key(e[2]) * normalize(self.o_key(e[2]))
                for e in self.graph.out_edges_iter(data=True))

    def static_flow(self, iterator=False):
        '''The current probabalistic ingress rate associated with each ingress
        node, based the edge probabilities of the bahaviour graph.

        Graph must be of type NetworkX.DiGraph.
        '''
        ingress_ids = (self.nid_key(x) for x in self.ingress)
        get_p = lambda x: self.p_key(x[2])
        r = (sum(imap(get_p, self.graph.out_edges(i, data=True)))
             for i in ingress_ids)
        return r if iterator else list(r)

    def update_edge_data(self, master):
        '''Update the edge properties from the master graph.'''
        R = master.copy()
        current_edges = set(self.graph.edges_iter())
        R.remove_edges_from(e for e in master.edges_iter() if e not in current_edges)
        self.graph = R


class TestNode(object):
    def __init__(self, node_id, position):
        self.nid = node_id
        self.position = position
