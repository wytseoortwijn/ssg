-module(explorer).
-export([initialize/2, initial/3, size/1, dispatch/1]).

size(Table) -> ets:info(Table, size).

owner(State) -> erlang:phash2(State) rem explorer:size(buffers).

wakeup(Pid, Id, idle) -> ets:update_element(buffers, Id, {4, working}), Pid ! resume;
wakeup(_, _, working) -> done.

dispatch(State) -> 
	Owner = owner(State),
	[{_, Buffer, Pid, _}] = ets:lookup(buffers, Owner),
	ets:insert(Buffer, {State}),
	wakeup(Pid, Owner, ets:lookup_element(buffers, Owner, 4)).

buffer([]) -> done;
buffer([State|Xs]) -> communicator:dispatch(State), buffer(Xs).

insert(false, State) ->
	ets:insert(states, {State}),
	buffer((get(next))(State));
insert(true, _) -> done.

working('$end_of_table') -> worker();
working(State) ->
	Buffer = get(buffer),
	Next = ets:next(Buffer, State),
	ets:delete(Buffer, State),
	insert(ets:member(states, State), State),
	working(Next).

idle() ->
	ets:update_element(buffers, get(id), {4, idle}),
	receive resume -> worker()
	after 1000 -> shell ! timeout
	end.

worker() ->
	Buffer = get(buffer),
	case explorer:size(Buffer) of
		0 -> idle(); _ -> working(ets:first(Buffer))
	end.
	
initial(Id, Buffer, Next) ->
	put(id, Id), put(buffer, Buffer), put(next, Next),
	worker().

create_workers(0, _) -> done;
create_workers(N, Next) when N > 0 ->
	Id = N - 1,
	Buffer = ets:new(buffer, [public, bag, {write_concurrency, true}]),
	Pid = spawn(explorer, initial, [Id, Buffer, Next]),
	ets:insert(buffers, {Id, Buffer, Pid, idle}),
	create_workers(Id, Next).

initialize({_, Next}, N) when N > 0 ->
	io:fwrite("Beginning state space exploration\n"),
	ets:new(states, [public, named_table, {read_concurrency, true}, {write_concurrency, true}]),
	ets:new(buffers, [public, named_table, {read_concurrency, true}]),
	put(begintime, main:time()),
	communicator:initialize(),
	create_workers(N, Next).