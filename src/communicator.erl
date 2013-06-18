-module(communicator).
-export([initialize/0, dispatch/1]).

number_of_nodes() -> explorer:size(nodes).

hash(Term) -> erlang:phash2(Term).

owner(State) -> hash(hash(State)) rem number_of_nodes().

initialize() ->
	ets:new(nodes, [public, named_table, {read_concurrency, true}]),
	create_entries(lists:sort([node() | nodes()])).

create_entries([]) -> done;
create_entries([X|Xs]) ->
	ets:insert(nodes, {number_of_nodes(), X}),
	create_entries(Xs).

dispatch(State) ->
	Node = ets:lookup_element(nodes, owner(State), 2),
	case Node == node() of 
		true -> explorer:dispatch(State);
		false -> {shell, Node} ! {state, State}
	end.