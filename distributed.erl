-module(distributed).
-export([init/1, listening/1, join/1, standby/0, start/0]).

standby() -> 
	receive S -> 
		parallel:explore_state([S], fun anderson1:next/1), 
		parallel:idle(fun anderson1:next/1) 
	end.

listening(N) ->
	receive
		connect -> 
			io:fwrite("Client connected!~n"),
			listening(N);

		start ->
			lists:foreach(fun(Node) -> {shell, Node} ! begin_explore end, nodes()),
			prepare_exploration(N),
			combining_graph(),
			listening(N);

		begin_explore ->
			prepare_exploration(N),
			listening(N);

		{send_graphs, Pid} ->
			io:fwrite("Sending graph components..~n"),
			Pid ! {graph_component, sequential:list_of_edges()},
			listening(N)
	end.

wait_for_graphs(0) -> done;
wait_for_graphs(I) when I > 0 ->
	receive {graph_component, Arcs} ->
		combine_graph(Arcs),
		wait_for_graphs(I - 1)
	end.

combine_graph([]) -> done;
combine_graph([S, T | Xs]) ->
	case sequential:explored(S) of 
		false -> sequential:store_state(S); _ -> nothing 
	end,
	sequential:store_edge(S, T),
	combine_graph(Xs).

prepare_exploration(N) ->
	io:fwrite("Beginning state space exploration!~n"),
	io:fwrite("Phase 1: Creating workers and distributing their Process IDs to all participating nodes...~n"),
	parallel:create_tables(),
	Workers = create_workers(N),
	distribute_pids(Workers),
	receiving_worker_information(Workers, length(nodes())),
	sequential:print_worker_information(parallel:number_of_workers()),
	io:fwrite("Phase 1 done!~n"),
	begin_exploration(N).

create_workers(0) -> [];
create_workers(N) when N > 0 -> 
	Pid = spawn(distributed, standby, []),
	[{node(), N - 1, Pid} | create_workers(N - 1)].

distribute_pids(Pids) ->
	lists:foreach(fun(Node) -> {shell, Node} ! {workers, Pids} end, nodes()).

receiving_worker_information(Workers, 0) -> 
	store_worker_information(Workers);
receiving_worker_information(Workers, I) when I > 0 ->
	receive {workers, Pids} ->
		receiving_worker_information(Workers ++ Pids, I - 1)
	end.

store_worker_information(Pids) ->
	Sorted = lists:sort(fun compare/2, Pids),
	Numbered = number(0, Sorted),
	lists:foreach(fun(Worker) -> ets:insert(workers, Worker) end, Numbered).

compare({Node1, _, _}, {Node2, _, _}) when Node1 < Node2 -> true;
compare({Node1, _, _}, {Node2, _, _}) when Node1 > Node2 -> false;
compare({_, N1, _}, {_, N2, _}) when N1 =< N2 -> true;
compare(_, _) -> false.

number(_, []) -> [];
number(N, [{_, _, Pid} | Xs]) -> [{N, Pid} | number(N + 1, Xs)].

begin_exploration(N) ->
	io:fwrite("Phase 2: Starting with state space generation...~n"),
	put(begintime, sequential:current_time()),
	I = anderson1:initial(),
	parallel:owner(I) ! I,
	receive timeout -> sequential:print_information(N) end,
	io:fwrite("Phase 2 done!~n").

combining_graph() ->
	io:fwrite("Phase 3: Downloading and combining all components...~n"),
	lists:foreach(fun(Node) -> {shell, Node} ! {send_graphs, self()} end, nodes()),
	wait_for_graphs(length(nodes())),
	sequential:print_state_information(),
	io:fwrite("Phase 3 done!~n").


init(N) -> register(shell, spawn(distributed, listening, [N])).

join(Node) -> {shell, Node} ! connect.

start() -> shell ! start.
