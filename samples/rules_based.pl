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
    towers(LeftList, [], [], 0, _, [], State),
    %% comment the line below if you want to see alternate solutions.
    !,
    showmoves(1, State).

%% towers/5
%%
%% There are seven rules. The first one is a check for completion, and
%% the other six are the six possible moves that one could make.
%%
towers(Left, Center, Right, _, _, StateIn, StateOut) :-
    %% format('towers-check, Left ~w, Center ~w, Right ~w\n',
    %%        [Left, Center, Right]),
    done(Left, Center, Right),
    StateOut = StateIn.

%% The six possible moves follow. Each has the same structure, we
%% attempt a move, we check for cycles, and we recurse.
towers(Left, Center, Right, LIn, LOut, StateIn, StateOut) :-
    %% format('towers-lc, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),

    %% try a move. The caller provided the last disc moved in LIn,
    %% send that to move and if successful, Y is the disc that was
    %% moved. Send that to the recursive call to towers.
    move(Left, Center, LeftOut, CenterOut, LIn, Y),

    %% check for cycles
    State = [LeftOut, CenterOut, Right],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    %% recurse. Y is the disc we just moved, returned by move.
    towers(LeftOut, CenterOut, Right, Y, LOut, X, StateOut).

towers(Left, Center, Right, LIn, LOut, StateIn, StateOut) :-
    %% format('towers-lr, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Left, Right, LeftOut, RightOut, LIn, Y),

    State = [LeftOut, Center, RightOut],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(LeftOut, Center, RightOut, Y, LOut, X, StateOut).

towers(Left, Center, Right, LIn, LOut, StateIn, StateOut) :-
    %% format('towers-cr, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Center, Right, CenterOut, RightOut, LIn, Y),

    State = [Left, CenterOut, RightOut],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(Left, CenterOut, RightOut, Y, LOut, X, StateOut).

towers(Left, Center, Right, LIn, LOut, StateIn, StateOut) :-
    %% format('towers-cl, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Center, Left, CenterOut, LeftOut, LIn, Y),

    State = [LeftOut, CenterOut, Right],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(LeftOut, CenterOut, Right, Y, LOut, X, StateOut).

towers(Left, Center, Right, LIn, LOut, StateIn, StateOut) :-
    %% format('towers-rc, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Right, Center, RightOut, CenterOut, LIn, Y),

    State = [Left, CenterOut, RightOut],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(Left, CenterOut, RightOut, Y, LOut, X, StateOut).

towers(Left, Center, Right, LIn, LOut, StateIn, StateOut) :-
    %% format('towers-rl, Left ~w, Center ~w, Right ~w\n',
    %% 	   [Left, Center, Right]),
    move(Right, Left, RightOut, LeftOut, LIn, Y),

    State = [LeftOut, Center, RightOut],
    \+ member(State, StateIn),
    append(StateIn, [State], X),

    towers(LeftOut, Center, RightOut, Y, LOut, X, StateOut).

%% move/4
%%
%% Attempt a move from From, to To. Enforce the rule about disc sizes
%% here.
%%
%% LIn, and LOut are used to make sure that two consecutive moves don't
%% move the same disc. That is clearly not a good use of time. The caller
%% sends in the last disc moved (LIn) and we pick a move only if the
%% FromTop is not the same as LIn. If a move is made, the disc moved is
%% returned as LOut.
move(From, To, FromOut, ToOut, LIn, LOut) :-
    %% there needs to be something on From
    last(From, FromTop),

    %% don't move the same disc you moved in the last move
    FromTop \= LIn,

    %% The To should either be empty, or the top disc is larger than
    %% the one we want to move.
    (
	length(To, N), N = 0;
	last(To, ToTop), ToTop > FromTop
    ),

    %% make it so.
    LOut = FromTop,
    delete(From, FromTop, FromOut),
    append(To, [FromTop], ToOut).
