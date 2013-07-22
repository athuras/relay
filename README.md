relay
=====

Distributed Traffic Control System.

Topology
===
The module that controls generating the local graphs:
Graph Types:
* Arterial: Edges are roads, vertices are Relays/Traffic Lights
* Cell: Edges are cross-artery flows, Vertices are Cells, or centroids of Arterial Loops.

Timer
===
The module that controls an agent's light switching/timing. Consists of a set of behaviours:
* Pattern: Complete light sequence, can be cycled.
* Behaviour: Ordered collection of Patterns. Can exist as an ACTIVE xor DORMANT state. ACTIVE Behaviours control realtime behaviour of traffic signal.
* Bias: Generic signal sent to timer, indicates degree to which traffic flow should be promoted spatially. Could also forseably promote dormant behaviours.

Chatter
===
The module that governs gossip between agents, and propagation of shared information. Communication protocol.

Overmind
===
The module that generates predictive models and operates on/alters the topological model.

Dispatch
===
Control Module/local event loop. The "app"

