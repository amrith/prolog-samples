%%
%% rules_based.pl - a rules based solution to the three towers problem.
%%
%% -amrith
%%

:- use_module(library(lists)).

%% showmoves/2
%%
%% Show the moves that got us here. showmoves is recursive.
%%
showmoves(_, []) :-
    format('Done.\n').

showmoves(N, [H|T]) :-
    format('State[~d]: ', N),
    format('Left ~w, Center ~w, Right ~w\n', H),
    N1 is N + 1,
    showmoves(N1, T).

%% done/3
%%
%% Are we done? We are if the left and right are empty, and right is
%% something (which we don't care about.
done([], [], _).

%% towers/1
%%
%% the main entry point.
towers(N) :-
    findall(X, between(1, N, X), L),
    reverse(L, LeftList),
    towers(LeftList, [], [], [], State),
    %% comment the line below if you want to see alternate solutions.
    !,
    showmoves(1, State).

%% towers/5
%%
%% There are seven rules. The first one is a check for completion, and
%% the other six are the six possible moves that one could make.
%%
towers(Left, Center, Right, StateIn, StateOut) :-
    %% format('towers-check, Left ~w, Center ~w, Right ~w\n',
    %%        [Left, Center, Right]),
    done(Left, Center, Right),
    StateOut = StateIn.

%% The six possible moves follow. Each has the same structure, we
%% attempt a move, we check for cycles, and we recurse.
towers(Left, Center, Right, StateIn, StateOut) :-
    %% format('towers-lc, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),

    %% try a move
    move(Left, Center, LeftOut, CenterOut),

    %% check for cycles
    State = [LeftOut, CenterOut, Right],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    %% recurse
    towers(LeftOut, CenterOut, Right, X, StateOut).

towers(Left, Center, Right, StateIn, StateOut) :-
    %% format('towers-lr, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Left, Right, LeftOut, RightOut),

    State = [LeftOut, Center, RightOut],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(LeftOut, Center, RightOut, X, StateOut).

towers(Left, Center, Right, StateIn, StateOut) :-
    %% format('towers-cr, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Center, Right, CenterOut, RightOut),

    State = [Left, CenterOut, RightOut],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(Left, CenterOut, RightOut, X, StateOut).

towers(Left, Center, Right, StateIn, StateOut) :-
    %% format('towers-cl, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Center, Left, CenterOut, LeftOut),

    State = [LeftOut, CenterOut, Right],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(LeftOut, CenterOut, Right, X, StateOut).

towers(Left, Center, Right, StateIn, StateOut) :-
    %% format('towers-rc, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Right, Center, RightOut, CenterOut),

    State = [Left, CenterOut, RightOut],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(Left, CenterOut, RightOut, X, StateOut).

towers(Left, Center, Right, StateIn, StateOut) :-
    %% format('towers-rl, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Right, Left, RightOut, LeftOut),

    State = [LeftOut, Center, RightOut],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(LeftOut, Center, RightOut, X, StateOut).

%% move/4
%%
%% Attempt a move from From, to To. Enforce the rule about disc sizes
%% here.
%%
move(From, To, FromOut, ToOut) :-
    %% there needs to be something on From
    last(From, FromTop),

    %% The To should either be empty, or the top disc is larger than
    %% the one we want to move.
    (
	length(To, N), N = 0;
	last(To, ToTop), ToTop > FromTop
    ),

    %% make it so.
    delete(From, FromTop, FromOut),
    append(To, [FromTop], ToOut).
