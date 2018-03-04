/* solve(N, L) holds when the list L holds a valid placement for N queens */
solve(N, L) :- queens(N, 0, [], L).

/* queens(N, I, L, Res) holds when the ML function
   (enum_nqueens N I L) would return Res. */
queens(N, N, L, L).
queens(N, I, L, Res) :-
  I < N,
  choose_in_range(0, N, C),
  okay(1, C, L),
  I1 is I+1,
  queens(N, I1, [C|L], Res).

/* choose_in_range(I, N, C) takes input I and N, and holds for each
   C such that (I <= C < N); it corresponds to a combination of
   direct-style 'choose' and 'range' in the ML version*/
choose_in_range(I, N, I) :- I < N.
choose_in_range(I, N, C) :- I < N, I1 is I+1, choose_in_range(I1, N, C).

/* okay(I, C, L) holds when (okay I C L) is true in the ML version */
okay(_, _, []).
okay(I, C, [X|XS]) :- C =\= X, (C-X) =\= I, (X-C) =\= I, I1 is I+1, okay(I1, C, XS).

/* aggregate_all(count, Q, Y) holds when Y is the
   number of solutions of Q.
   count(N, Count) holds when Count is the number of
   valid placements of N queens. */
count(N, Count) :- aggregate_all(count, solve(N, L), Count).
