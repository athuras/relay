%%  Records for use with the request interface for cowboy.
-record(visible_state, {behaviour,
						current_plan,
						current_timing,
						delay_params,
						location,
						prediction,
						prediction_time,
						name}).

-record(visible_queues, {ingress, egress}
	).

-record(agent_state, {
		behaviour,
		current_plan,
		current_timing,
		delay_params,
		egress,
		ingress,
		location,
		name,
		prediction,
		prediction_time
		}
	).

%%  YOLOSWAG
-define(merge_to_agent_state(S, Q),
	#agent_state{
		behaviour=S#visible_state.behaviour,
		current_plan=S#visible_state.current_plan,
		current_timing=S#visible_state.current_timing,
		delay_params=S#visible_state.delay_params,
		location=S#visible_state.location,
		name=S#visible_state.name,
		prediction=S#visible_state.prediction,
		prediction_time=S#visible_state.prediction_time,
		egress=Q#visible_queues.egress,
		ingress=Q#visible_queues.ingress
		}
).
