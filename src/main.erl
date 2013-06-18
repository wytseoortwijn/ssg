-module(main).
-export([start/1, join/2, listener/1, explore/1, time/0, elapsed_time/0, status/0]).

%% This function returns the number of milliseconds that have passed since 00:00 GMT, Jan 1, 1970
time() -> {_, S, M} = erlang:now(), S * 1000 + M / 1000.

elapsed_time() -> round(main:time() - get(begintime)).

model(adding1) -> {adding1:initial(), fun adding1:next/1};
model(anderson1) -> {anderson1:initial(), fun anderson1:next/1};
model(anderson2) -> {anderson2:initial(), fun anderson2:next/1}.

broadcast([], _) -> done;
broadcast([X|Xs], Term) -> {shell, X} ! Term, broadcast(Xs, Term).

explore(Model, N) ->
	broadcast(nodes(), {initialize, Model}), 
	{Init, Next} = model(Model),
	explorer:initialize({Init, Next}, N),
	communicator:dispatch(Init).

listener(N) ->
	receive
		ping -> broadcast(nodes(), pong), listener(N);
		pong -> listener(N);
		{initialize, Model} -> explorer:initialize(model(Model), N), processing(N);
		{explore, Model} -> explore(Model, N), processing(N)
	end.

processing(N) ->
	receive
		{state, S} -> explorer:dispatch(S), processing(N);
		timeout -> print_information(N), listener(N);
		status -> io:fwrite("Number of states: ~p~n", [explorer:size(states)]), processing(N)
	end.

print_information(N) ->
	io:fwrite("Done!\n"),
	io:fwrite("Number of states found: ~p~n", [explorer:size(states)]),
	io:fwrite("Number of workers: ~p~n", [N]),
	io:fwrite("Processing time: ~p ms ~n", [main:elapsed_time() - 1000]).

start(N) -> register(shell, spawn(main, listener, [N])).

join(Node, N) -> start(N), {shell, Node} ! ping.

explore(Model) -> shell ! {explore, Model}.

status() -> shell ! status.