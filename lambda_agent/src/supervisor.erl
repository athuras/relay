-module(supervisor).
-export([
	start/2,
	start_link/2,
	init/1,
	main/1
	]).

start(Mod, Args) ->
	spawn(?MODULE, init, [{Mod, Args}]).

start_link(Mod, Args) ->
	spawn_link(?MODULE, init, [{Mod, Args}]).

init({Mod, Args}) ->
	process_flag(trap_exit, true),
	main({Mod, start_link, Args}).

main({M, F, A}) ->
	Pid = apply(M, F, A),
	receive
		{'EXIT', _From, shutdown} ->
			exit(shutdown);  %%  Will kill children as well
		{'EXIT', Pid, Reason} ->
			io:format("Process ~p exited with ~p~n", [Pid, Reason]),
			main({M, F, A})
	end.
