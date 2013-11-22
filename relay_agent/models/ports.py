from time import time
import Queue

class IngressPort(object):
    '''A port for an intersection that contains vehicle queues, and an abstract
    cost function'''
    def __init__(self, nid, orientation, q_kwargs):
        '''A new Ingress Port with a multi-producer/consumer queue'''
        self.nid = nid
        self.orientation = orientation
        self.queue = Queue.Queue(q_kwargs)

    def foo(self):
        '''Do stuff'''
        # Each queue will have (entry-time, value) objects
        # 'real' cost can be found by subtracting entry-time from actual time
