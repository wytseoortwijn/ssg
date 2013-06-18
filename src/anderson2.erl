-module(anderson2).
-export([next/1, initial/0, size/0]).

%% Thus function checks if a given bit is pointing to the logical '1'
slot(1, _, _, 0) -> true;
slot(_, 1, _, 1) -> true;
slot(_, _, 1, _) -> true;
slot(_, _, _, _) -> false.

%% Given two bits, this function swaps one of these bits with a new value
swap(_, S2, S3, 0, V) -> <<V:1, S2:1, S3:1>>;
swap(S1, _, S3, 1, V) -> <<S1:1, V:1, S3:1>>;
swap(S1, S2, _, 2, V) -> <<S1:1, S2:1, V:1>>.

%% The transition relation function, used internally to find the next state of some state given a process index
transition(<<S1:1, S2:1, S3:1, N:8, _:8, 0:3, M2:8, P2:3, M3:8, P3:3>>, 0) -> 
	<<S1:1, S2:1, S3:1, (N + 1):8, N:8, 1:3, M2:8, P2:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, _:8, 0:3, M3:8, P3:3>>, 1) -> 
	<<S1:1, S2:1, S3:1, (N + 1):8, M1:8, P1:3, N:8, 1:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, P2:3, _:8, 0:3>>, 2) -> 
	<<S1:1, S2:1, S3:1, (N + 1):8, M1:8, P1:3, M2:8, P2:3, N:8, 1:3>>;

transition(<<S1:1, S2:1, S3:1, N:8, 2:8, 1:3, M2:8, P2:3, M3:8, P3:3>>, 0) -> 
	<<S1:1, S2:1, S3:1, ((256 + N - 3) rem 256):8, 2:8, 2:3, M2:8, P2:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, 2:8, 1:3, M3:8, P3:3>>, 1) -> 
	<<S1:1, S2:1, S3:1, ((256 + N - 3) rem 256):8, M1:8, P1:3, 2:8, 2:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, P2:3, 2:8, 1:3>>, 2) -> 
	<<S1:1, S2:1, S3:1, ((256 + N - 3) rem 256):8, M1:8, P1:3, M2:8, P2:3, 2:8, 2:3>>;

transition(<<S1:1, S2:1, S3:1, N:8, M1:8, 1:3, M2:8, P2:3, M3:8, P3:3>>, 0) -> 
	<<S1:1, S2:1, S3:1, N:8, (M1 rem 3):8, 2:3, M2:8, P2:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, 1:3, M3:8, P3:3>>, 1) -> 
	<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, (M2 rem 3):8, 2:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, P2:3, M3:8, 1:3>>, 2) -> 
	<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, P2:3, (M3 rem 3):8, 2:3>>;

transition(<<S1:1, S2:1, S3:1, N:8, M1:8, 2:3, M2:8, P2:3, M3:8, P3:3>>, 0) -> 
	case slot(S1, S2, S3, M1) of true -> <<S1:1, S2:1, S3:1, N:8, M1:8, 3:3, M2:8, P2:3, M3:8, P3:3>>; _ -> none end;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, 2:3, M3:8, P3:3>>, 1) -> 
	case slot(S1, S2, S3, M2) of true -> <<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, 3:3, M3:8, P3:3>>; _ -> none end;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, P2:3, M3:8, 2:3>>, 2) -> 
	case slot(S1, S2, S3, M3) of true -> <<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, P2:3, M3:8, 3:3>>; _ -> none end;

transition(<<S1:1, S2:1, S3:1, N:8, M1:8, 3:3, M2:8, P2:3, M3:8, P3:3>>, 0) -> 
	<<A:1, B:1, C:1>> = swap(S1, S2, S3, M1, 0), <<A:1, B:1, C:1, N:8, M1:8, 4:3, M2:8, P2:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, 3:3, M3:8, P3:3>>, 1) -> 
	<<A:1, B:1, C:1>> = swap(S1, S2, S3, M2, 0), <<A:1, B:1, C:1, N:8, M1:8, P1:3, M2:8, 4:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, P2:3, M3:8, 3:3>>, 2) -> 
	<<A:1, B:1, C:1>> = swap(S1, S2, S3, M3, 0), <<A:1, B:1, C:1, N:8, M1:8, P1:3, M2:8, P2:3, M3:8, 4:3>>;

transition(<<S1:1, S2:1, S3:1, N:8, M1:8, 4:3, M2:8, P2:3, M3:8, P3:3>>, 0) -> 
	<<A:1, B:1, C:1>> = swap(S1, S2, S3, (M1 + 1) rem 3, 1), <<A:1, B:1, C:1, N:8, M1:8, 0:3, M2:8, P2:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, 4:3, M3:8, P3:3>>, 1) -> 
	<<A:1, B:1, C:1>> = swap(S1, S2, S3, (M2 + 1) rem 3, 1), <<A:1, B:1, C:1, N:8, M1:8, P1:3, M2:8, 0:3, M3:8, P3:3>>;
transition(<<S1:1, S2:1, S3:1, N:8, M1:8, P1:3, M2:8, P2:3, M3:8, 4:3>>, 2) -> 
	<<A:1, B:1, C:1>> = swap(S1, S2, S3, (M3 + 1) rem 3, 1), <<A:1, B:1, C:1, N:8, M1:8, P1:3, M2:8, P2:3, M3:8, 0:3>>;

transition(_, _) -> none.

%% Given a state, this function returns a list containing the next possible state from the given state
next(S) -> lists:filter(fun(E) -> E /= none end, [transition(S, 0), transition(S, 1), transition(S, 2)]).

%% Returns the initial state of the 'anderson2' model as a binary string of 48 bits
initial() -> <<1:1, 0:1, 0:1, 0:8, 0:8, 0:3, 0:8, 0:3, 0:8, 0:3>>.

size() -> 48.