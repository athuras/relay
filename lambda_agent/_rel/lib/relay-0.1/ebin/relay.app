{application, relay, [
	{description, "Relay Agent"},
	{vsn, "0.1"},
	{modules, [accumulator, agent_graphs, connection_handler, connection_manager, control_graph, control_handler, echo, graph_builder, graph_utils, jsonerl, lol_matrices, mochinum, netmed, netmed_graph, netmed_handler, plan, predict, python_manager, relay_agent, relay_agent_sup, relay_app, relay_store, relay_sup, relay_utils, spider, state_manager, testing, top_page_handler]},
	{registered, [relay_sup]},
	{applications, [
		kernel,
		stdlib,
		cowboy,
		erlport
	]},
	{mod, {relay_app, []}},
	{env, []}
]}.
