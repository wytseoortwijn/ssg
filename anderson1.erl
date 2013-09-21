-module(anderson1).
-export([next/1, initial/0, length/0]).

%% The definition of an 'anderson1' state as a binary string of 22 bits
-define(state(S1, S2, N, M1, P1, M2, P2), <<S1:1, S2:1, N:8, M1:8, P1:3, M2:8, P2:3>>).

%% This function checks if a given bit is pointing to the logical '1'.
slot(1, _, 0) -> true;
slot(_, 1, 1) -> true;
slot(_, _, _) -> false.

%% Given two bits, this function swaps one of these bits with a new value.
swap(_, S2, 0, V) -> <<V:1, S2:1>>;
swap(S1, _, 1, V) -> <<S1:1, V:1>>.

%% The transition relation function, used internally to find the next state of some state given a process index
transition(?state(S1, S2, N, _, 0, M2, P2), 0) -> 
	?state(S1, S2, (N + 1), N, 1, M2, P2);
transition(?state(S1, S2, N, M1, P1, _, 0), 1) -> 
	?state(S1, S2, (N + 1), M1, P1, N, 1);

transition(?state(S1, S2, N, 1, 1, M2, P2), 0) -> 
	?state(S1, S2, ((256 + N - 2) rem 256), 1, 2, M2, P2);
transition(?state(S1, S2, N, M1, P1, 1, 1), 1) -> 
	?state(S1, S2, ((256 + N - 2) rem 256), M1, P1, 1, 2);

transition(?state(S1, S2, N, M1, 1, M2, P2), 0) -> 
	?state(S1, S2, N, (M1 rem 2), 2, M2, P2);
transition(?state(S1, S2, N, M1, P1, M2, 1), 1) -> 
	?state(S1, S2, N, M1, P1, (M2 rem 2), 2);

transition(?state(S1, S2, N, M1, 2, M2, P2), 0) -> 
	case slot(S1, S2, M1) of true -> ?state(S1, S2, N, M1, 3, M2, P2); _ -> none end;
transition(?state(S1, S2, N, M1, P1, M2, 2), 1) -> 
	case slot(S1, S2, M2) of true -> ?state(S1, S2, N, M1, P1, M2, 3); _ -> none end;

transition(?state(S1, S2, N, M1, 3, M2, P2), 0) -> 
	<<A:1, B:1>> = swap(S1, S2, (M1 + 1) rem 2, 0), ?state(A, B, N, M1, 4, M2, P2);
transition(?state(S1, S2, N, M1, P1, M2, 3), 1) -> 
	<<A:1, B:1>>= swap(S1, S2, (M2 + 1) rem 2, 0), ?state(A, B, N, M1, P1, M2, 4);

transition(?state(S1, S2, N, M1, 4, M2, P2), 0) -> 
	<<A:1, B:1>> = swap(S1, S2, (M1 + 1) rem 2, 1), ?state(A, B, N, M1, 0, M2, P2);
transition(?state(S1, S2, N, M1, P1, M2, 4), 1) -> 
	<<A:1, B:1>> = swap(S1, S2, (M2 + 1) rem 2, 1), ?state(A, B, N, M1, P1, M2, 0);

transition(_, _) -> none.

filter([]) -> [];
filter([none|Xs]) -> filter(Xs);
filter([X|Xs]) -> [X | filter(Xs)].

%% Given a state, this function returns a list containing the next possible state from the given state
next(S) -> filter([transition(S, 0), transition(S, 1)]).

%% Returns the initial state of the 'anderson1' model as a binary pattern of 32 bits
initial() -> ?state(1, 0, 0, 0, 0, 0, 0).

length() -> 32.