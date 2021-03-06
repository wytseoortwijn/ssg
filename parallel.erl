-module(parallel).
-export([start/1, idle/0, go/1, number_of_workers/0, owner/1, explore_state/1, create_tables/0]).
-include("includes.hrl").

%% This function is a shortcut to the standard hash function used by Erlang.
%% We did this, because we can now change the maximum integer that will be generated by the hash function
%% without having to change the whole program. Making this number smaller may cause speedups.
hash(S) -> 
	erlang:phash2(S).

%% This function returns the number of participating workers. We keep an hashtable with references to
%% each process to speed up the dispatch function, that sends states to their owner. This function simply
%% counts the number of entries that are in that hashtable.
number_of_workers() -> 
	ets:info(workers, size).

%% This function will determine the owning process of the state vector S. When an owning
%% process is found, that process is returned by this function.
owner(S) -> 
	ets:lookup_element(workers, hash(S) rem number_of_workers(), 2).

%% This function will explore a given set of state vectors in combination with a next state
%% function N. This function will recursively go through all given state vectors. When the
%% current state vector is unexplored, it will be stored inside the set of vertices. After that,
%% this function calls the explore_edges function to handle next states and edges.
explore_state([]) -> done;
explore_state([S|Xs]) -> 
	case sequential:explored(S) of
		true -> explore_state(Xs);
		false -> 
			sequential:store_state(S),
			Ys = dispatch(sequential:explore_edges(S, ?next(S))),
			explore_state(Xs), explore_state(Ys)
	end.

%% This function will wait for up to 500ms for a state to process. When receiving the state, the 
%% sequential exploration algorithm is called. The only difference is that we adjusted the next state
%% function to first dispatch the states owned by another process.
idle() -> 
	receive S -> explore_state([S]), idle() 
	after 500 -> shell ! timeout
	end.

%% This function will dispatch a list of states to their owners. This function is called by
%% one of the processes that is participating in the state space generation process. After having sent
%% all states to their owners, this function returns the list of states that are owned by the calling process.
dispatch([]) -> [];
dispatch([X|Xs]) -> 
	case owner(X) == self() of
		true -> [X | dispatch(Xs)];
		false -> owner(X) ! X, dispatch(Xs)
	end.

%% This function will recursively spawn N workers. After completing, this function returns the
%% symbol 'done'
create_workers(0) -> done;
create_workers(I) when I > 0 -> 
	ets:insert(workers, {I - 1, spawn(parallel, idle, [])}), 
	create_workers(I - 1).

%% This function is called when the algorithm is started. This function will create N workers
%% After that, it sends the initial state vector I to the owning process, which will start state space generation
start(Workers) when Workers > 0 ->
	create_workers(Workers),
	owner(?initial()) ! ?initial().

%% This function can be called for testing the algorithm with N workers. It will create the sets which will 
%% contain the vertices and edges of the directed graph to be created. After that, this function will print information
%% about the execution time, number of states found and number of edges found.
go(Workers) ->
	register(shell, self()),
	create_tables(),
	put(begintime, sequential:current_time()),
	start(Workers),
	receive timeout -> sequential:print_information(Workers) end.

create_tables() ->
	ets:new(workers, [public, named_table, {read_concurrency, true}, {write_concurrency, true}]),
	ets:new(vertices, [public, set, named_table, {read_concurrency, true}, {write_concurrency, true}]),
	ets:new(edges, [public, set, named_table, {write_concurrency, true}]).