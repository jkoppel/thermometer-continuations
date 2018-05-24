/* solve(N, L) holds when the list L holds a valid placement for N queens */
solve(N, L) :- queens(N, 0, [], L).

/* queens(N, I, L, Res) holds when the ML function
   (enum_nqueens N I L) would return Res. */
queens(N, N, L, L).
queens(N, I, L, Res) :-
  I < N,
  choose_okay_in_range(0, N, C, L),
  I1 is I+1,
  queens(N, I1, [C|L], Res).

/* choose_okay_in_range(I, N, C) takes input I and N, and holds for each
   C such that (I <= C < N) and okay(1, C, L) */
choose_okay_in_range(I, N, I, L) :- I < N, okay(1, I, L).
choose_okay_in_range(I, N, C, L) :- I < N, I1 is I+1, choose_okay_in_range(I1, N, C, L).

/* okay(I, C, L) holds when (okay I C L) is true in the ML version */
okay(_, _, []).
okay(I, C, [X|XS]) :- C =\= X, (C-X) =\= I, (X-C) =\= I, I1 is I+1, okay(I1, C, XS).

/* aggregate_all(count, Q, Y) holds when Y is the
   number of solutions of Q.
   count(N, Count) holds when Count is the number of
   valid placements of N queens. */
count(N, Count) :-
    findall(L, solve(N, L), Solutions),
    length(Solutions, Count).
