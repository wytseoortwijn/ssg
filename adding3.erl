-module(adding3).
-export([initial/0, next/1, length/0]).

%% The definition of an 'adding3' state as a binary string of 22 bits
-define(state(C, X1, X2, P1, P2), <<C:10, X1:10, X2:10, P1:2, P2:2>>).

%% The definitions of the state names that are used in the adding1 model
-define(Q, 0).
-define(R, 1).
-define(S, 2).

%% The transition relation function, used internally to find the next state of some state given a process index
trans(?state(C, _, X2, ?Q, P2), 0) when C < 300 -> ?state(C, C, X2, ?R, P2);
trans(?state(C, X1, _, P1, ?Q), 1) when C < 300 -> ?state(C, X1, C, P1, ?R);
trans(?state(C, X1, X2, ?R, P2), 0) -> ?state(C, (X1 + C), X2, ?S, P2);
trans(?state(C, X1, X2, P1, ?R), 1) -> ?state(C, X1, (X2 + C), P1, ?S);
trans(?state(_, X1, X2, ?S, P2), 0) -> ?state(X1, X1, X2, ?Q, P2);
trans(?state(_, X1, X2, P1, ?S), 1) -> ?state(X2, X1, X2, P1, ?Q);
trans(_, _) -> none.

%% Given a state, this function returns a list containing all states accessible from the given state
next(S) -> [X || X <- [trans(S, 0), trans(S, 1)], X /= none].

%% Yields the initial state of the 'adding1' model
initial() -> ?state(1, 0, 0, ?Q, ?Q).

%% Denotes the length of the state vector, which is 22 bits in this case
length() -> 22.