relay
=

Today's roads, for the future, today.

relay-agent
=

The individual signal controller.
* Local intersection graph (how traffic can flow between lanes)
* Inter-agent graphs
* local edge modelling
* optimization engine
* event loop

relay-observer
=

The web-app and viewer. Window into the wild-wild world of adaptive traffic control and distributed simulation. Heatmaps of stuff. Moving histograms.

Probably powered by CartoDB or some combination of PostGIS/Postgres, Flask/some-other-framework, and Python.

* polls relay-agents
* does other stuff
* makes people reevaluate the choices they have made throughout their lives.
* paradigm shifts

&#955;-relay
=

Concurrent functional implementation of **relay-agent** in erlang. This may not ever be written, but would be awesome for simulating many concurrent agents operating in soft-realtime.
