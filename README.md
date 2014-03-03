relay
=

Today's roads, for the future, today.

A Transparently Distributed, Adaptive, Acyclic, Predictive Intelligent Traffic Control Framework.

relay-flask
=

The web-app and viewer. Window into the wild-wild world of adaptive traffic control and distributed simulation. Heatmaps of stuff. Moving histograms.

Probably powered by CartoDB or some combination of PostGIS/Postgres, Flask/some-other-framework, and Python.

* polls relay-agents
* does other stuff
* makes people reevaluate the choices they have made throughout their lives.
* paradigm shifts

Currently serves a default example map of London. Run python relay.py (within the relay-flask) directory, and navigate to localhost:5000/maps

&#955;-relay
=

Concurrent functional implementation of **relay-agent** in erlang.

* netmed: (NETworkMEDiator), handles all network communication between agents, and between some intra-agent processes.
* controller: Resposible for dictating when, how, and where the lights change, through the use of a behaviour-transition-graph, cost functions and bias vectors.
* prediction service: ML for any occasion. Contains a few different on-line models: macro-edges, internal intersection probabilities and other.
