-module(anderson1).
-export([next/1, initial/0, size/0]).

%% This function checks if a given bit is pointing to the logical '1'
slot(1, _, 0) -> true;
slot(_, 1, 1) -> true;
slot(_, _, _) -> false.

%% Given two bits, this function swaps one of these bits with a new value
swap(_, S2, 0, V) -> <<V:1, S2:1>>;
swap(S1, _, 1, V) -> <<S1:1, V:1>>.

%% The transition relation function, used internally to find the next state of some state given a process index
transition(<<S1:1, S2:1, N:8, _:8, 0:3, M2:8, P2:3>>, 0) -> 
	<<S1:1, S2:1, (N + 1):8, N:8, 1:3, M2:8, P2:3>>;
transition(<<S1:1, S2:1, N:8, M1:8, P1:3, _:8, 0:3>>, 1) -> 
	<<S1:1, S2:1, (N + 1):8, M1:8, P1:3, N:8, 1:3>>;

transition(<<S1:1, S2:1, N:8, 1:8, 1:3, M2:8, P2:3>>, 0) -> 
	<<S1:1, S2:1, ((256 + N - 2) rem 256):8, 1:8, 2:3, M2:8, P2:3>>;
transition(<<S1:1, S2:1, N:8, M1:8, P1:3, 1:8, 1:3>>, 1) -> 
	<<S1:1, S2:1, ((256 + N - 2) rem 256):8, M1:8, P1:3, 1:8, 2:3>>;

transition(<<S1:1, S2:1, N:8, M1:8, 1:3, M2:8, P2:3>>, 0) -> 
	<<S1:1, S2:1, N:8, (M1 rem 2):8, 2:3, M2:8, P2:3>>;
transition(<<S1:1, S2:1, N:8, M1:8, P1:3, M2:8, 1:3>>, 1) -> 
	<<S1:1, S2:1, N:8, M1:8, P1:3, (M2 rem 2):8, 2:3>>;

transition(<<S1:1, S2:1, N:8, M1:8, 2:3, M2:8, P2:3>>, 0) -> 
	case slot(S1, S2, M1) of true -> <<S1:1, S2:1, N:8, M1:8, 3:3, M2:8, P2:3>>; _ -> none end;
transition(<<S1:1, S2:1, N:8, M1:8, P1:3, M2:8, 2:3>>, 1) -> 
	case slot(S1, S2, M2) of true -> <<S1:1, S2:1, N:8, M1:8, P1:3, M2:8, 3:3>>; _ -> none end;

transition(<<S1:1, S2:1, N:8, M1:8, 3:3, M2:8, P2:3>>, 0) -> 
	<<A:1, B:1>> = swap(S1, S2, (M1 + 1) rem 2, 0), <<A:1, B:1, N:8, M1:8, 4:3, M2:8, P2:3>>;
transition(<<S1:1, S2:1, N:8, M1:8, P1:3, M2:8, 3:3>>, 1) -> 
	<<A:1, B:1>>= swap(S1, S2, (M2 + 1) rem 2, 0), <<A:1, B:1, N:8, M1:8, P1:3, M2:8, 4:3>>;

transition(<<S1:1, S2:1, N:8, M1:8, 4:3, M2:8, P2:3>>, 0) -> 
	<<A:1, B:1>> = swap(S1, S2, (M1 + 1) rem 2, 1), <<A:1, B:1, N:8, M1:8, 0:3, M2:8, P2:3>>;
transition(<<S1:1, S2:1, N:8, M1:8, P1:3, M2:8, 4:3>>, 1) -> 
	<<A:1, B:1>> = swap(S1, S2, (M2 + 1) rem 2, 1), <<A:1, B:1, N:8, M1:8, P1:3, M2:8, 0:3>>;

transition(_, _) -> none.

filter([]) -> [];
filter([none|Xs]) -> filter(Xs);
filter([X|Xs]) -> [X | filter(Xs)].

%% Given a state, this function returns a list containing the next possible state from the given state
next(S) -> filter([transition(S, 0), transition(S, 1)]).

%% Returns the initial state of the 'anderson1' model as a binary pattern of 32 bits
initial() -> <<1:1, 0:1, 0:8, 0:8, 0:3, 0:8, 0:3>>.

size() -> 32.