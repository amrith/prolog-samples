%%
%% recursive.pl - a recursive solver of the three towers problem.
%%
%% -amrith
%%

%% move/5
%%
%% There are two move rules, one for a single disc, and one for
%% multiple discs.
%%
%% move a single disc from A to B, where the third place is C.
move(1, A, B, _, Ct, Steps) :-
    Steps is Ct + 1,
    write(Steps), write(': '), write('Move one disc from '), write(A),
    write(' to '), write(B), nl.

%% move multiple discs from A to B, where the third place is C. This
%% is a recursive rule.
move(N, A, B, C, Ct, Steps) :-
    N > 1,
    M is N - 1,
    move(M, A, C, B, Ct, N1),
    move(1, A, B, C, N1, N2),
    move(M, C, B, A, N2, Steps).

%% towers/1
%%
%% The main entry point. Invoke like towers(5).
towers(N) :-
    move(N, left, right, middle, 0, Steps),
    write('That took a total of '), write(Steps), write(' steps.'), nl.

