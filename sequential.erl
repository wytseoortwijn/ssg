-module(sequential).
-export([go/0, to_dot/0, to_binary/0, current_time/0, print_worker_information/1, print_state_information/0, elapsed_time/0, print_information/1, explore_edges/2, list_of_edges/0, explored/1, store_state/1, store_edge/2]).
-include("includes.hrl").

%% This function tests if the given state S is already explored. This is the case when
%% S is already in the set of vertices. In this case, the symbol 'true' is returned. Otherwise,
%% 'false' is returned
explored(S) -> 
	ets:member(vertices, S).

%% This function takes a vertex and stores it into the set of vertices. The set of vertices
%% is implemented as a ETS (Erlang Term Storage) set.
store_state(S) -> 
	ets:insert(vertices, {S}), S.

%% This function takes two state vectors and stores it as an edge into the set of edges.
%% The set of edges is actually implemented as a ets (Erlang Term Storage) set.
store_edge(S, T) -> 
	ets:insert(edges, {[S, T]}), T.

%% This function takes some state and all next states of that state. For each next state, this function
%% will store and edge between S and the next state vector. After performing this action, the 
%% next states will be returned.
explore_edges(_, []) -> [];
explore_edges(S, [X|Xs]) -> 
	store_edge(S, X), 
	[X | explore_edges(S, Xs)].

%% This function will explore a given set of state vectors in combination with a next state
%% function N. This function will recursively go through all given state vectors. When the
%% current state vector is unexplored, it will be stored inside the set of vertices. After that,
%% this function calls the explore_edges function to handle next states and edges.
explore([]) -> done;
explore([S|Xs]) -> 
	case explored(S) of
		true -> explore(Xs);
		false -> 
			store_state(S),
			Ys = explore_edges(S, ?next(S)),
			explore(Xs), explore(Ys)
	end.

%% This function can be called for testing the algorithm. By calling this function,
%% the sets for vertices and arcs/edges are created and the state space generation algorithm will
%% be started. After that, this function will print some information like execution times, number
%% of states found and number of arcs/edges found.
go() ->
	ets:new(vertices, [public, set, named_table]),
	ets:new(edges, [public, set, named_table]),
	put(begintime, current_time()),
	explore([?initial()]),
	print_information(1).

%% This function will print information about the execution of the algorithm. By calling this function,
%% the number of states and edges will be printed, together with the number of workers used (provided as
%%	the parameter N) and the processing time is determined and written to the console.
print_information(N) ->
	print_state_information(),
	print_worker_information(N),
	print_time_information().

print_state_information() ->
	io:fwrite("Number of states stored: ~p~n", [ets:info(vertices, size)]),
	io:fwrite("Number of edges stored: ~p~n", [ets:info(edges, size)]).

print_worker_information(N) ->
	io:fwrite("Number of workers: ~p~n", [N]),
	io:fwrite("Number of nodes: ~p~n", [length(nodes()) + 1]).

print_time_information() ->	
	io:fwrite("Processing time: ~p ms ~n", [elapsed_time() - 500]).

%% This function calculated the current time given in milliseconds
current_time() -> {_, S, M} = erlang:now(), S * 1000 + M / 1000.

%% This function calculates the elapsed time. To do this, this function assumes that the 'begintime'
%% value is registered.
elapsed_time() -> round(current_time() - get(begintime)).

%% This function states a state as binary vector and will convert it into an integer.
%% By doing that, we ask the active model what its state length is.
state_to_int(State) ->
	Length = anderson1:length(),
	<<S:Length>> = State, S.

%% This function can be called after performing state space generation and will transform the obtained graph
%% G = <V, A> into an .dot format. This file can then be opened by graph visualization software like GraphViz 
format_dot('$end_of_table') -> "";
format_dot(Edge = [S, T]) ->
	Left = io_lib:format("~p -> ~p; ", [state_to_int(S), state_to_int(T)]),
	Right = format_dot(ets:next(edges, Edge)),
	string:concat(Left, Right).

%% This function can be called after performing state space generation and will tranform and save 
%% the obtained graph to a file called 'to_dot.dot'.
to_dot() ->
	Content = format_dot(ets:first(edges)),
	File = string:concat(string:concat("digraph g { node [shape=plaintext]; ", Content), "}"),
	file:write_file("to_dot.dot", File).

%% This function will make one list of all edges obtained by the state space generator.
%% Let A = { [v_1, v'_1], ..., [v_n, v'_n] } be the set of arcs/edges obtained by the algorithm.
%% This function will turn A into a list [ v_1, v'_1, ..., v_n, v'_n ] of vertices.
%% This list can then be converted to a binary string, when in turn can be used to save or send
%% graph components to other processes or machines.
list_of_edges('$end_of_table') -> [];
list_of_edges(Edge = [S, T]) -> [S, T | list_of_edges(ets:next(edges, Edge))].
list_of_edges() -> list_of_edges(ets:first(edges)).

%% This function can be called after performing state space generation and will save the obtained
%% graph to a binary file. Note that we only save the set of edges. With this set, we can reconstruct
%% the set of vertices. The binary representation of the graph will be saved to 'binary.txt'.
to_binary() -> file:write_file("binary.txt", list_to_bitstring(list_of_edges())).