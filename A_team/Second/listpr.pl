insert(X, List, BiggerList) :-
   delete(X, BiggerList, List).

permutation([], []).
permutation([X|L], P) :-
   permutation(L, L1),
   insert(X, L1, P).

permutation2([], []).
permutation2(L, [X|P]) :-
   delete(X, L, L1),
   permutation2(L1, P).

sublist([], _).
sublist(S, L) :-
   append(_, L1, L),
   append(S, _, L1),
   S \= [].

gensubset([], []).
gensubset([X|S], [X|L]) :-
   gensubset(S, L).
gensubset(S, [_|L]) :-
   gensubset(S, L).

mylength1([], 0).
mylength1([_|L], N) :-
   mylength1(L, M),
   N is M+1.

mylength2(L, N) :-
   mylength2(L, 0, N).

mylength2([], N, N).
mylength2([_|L], M, N) :-
   M1 is M+1,
   mylength2(L, M1, N).

mylength3([], 0).
mylength3([_|L], N) :-
   N > 0,
   M is N-1,
   mylength3(L, M).

mylength(L, N) :-
   var(N),
   mylength2(L, N).
mylength(L, N) :-
   nonvar(N),
   mylength3(L, N).

mylength4([], 0).
mylength4([_|L], 1+N) :-
   mylength4(L, N).

mylength5(L, N) :-
   mylength4(L, Expr),
   N is Expr.

make_matrix(M, N, Matrix) :-
   length(Matrix, M),
   make_lines(N, Matrix).

make_lines(_, []).
make_lines(N, [Line|Matrix]) :-
   length(Line, N),
   make_lines(N, Matrix).

make_square_matrix(N, Matrix) :-
   make_matrix(N, N, Matrix).

get_ith(1, [X|_], X).
get_ith(I, [_|L], X) :-
   I > 1,
   I1 is I-1,
   get_ith(I1, L, X).

del_ith(1, [X|L], X, L).
del_ith(I, [Y|L], X, [Y|L1]) :-
   I > 1,
   I1 is I-1,
   del_ith(I1, L, X, L1).

get_ijth(I, J, Matrix, X) :-
   get_ith(I, Matrix, Line),
   get_ith(J, Line, X).

del_first([], [], []).
del_first([[X|L]|R], [X|RX], [L|RL]) :-
   del_first(R, RX, RL).

del_ith_col(_, [], [], []).
del_ith_col(I, [L|R], [X|RX], [L1|RL]) :-
   del_ith(I, L, X, L1),
   del_ith_col(I, R, RX, RL).

matr_transp(M, []) :-
   empty_lists(M).
matr_transp(M, [C|TM]) :-
   del_first(M, C, RM),
   matr_transp(RM, TM).

empty_lists([]).
empty_lists([[]|M]) :-
   empty_lists(M).

/*diagd([[X]], [X]).
diagd(Matrix, [X|Diag]) :-
   del_first(Matrix, [X|_], [_|RMatrix]),
   diagd(RMatrix, Diag).

diagu(Matrix, DiagU) :-
   reverse(Matrix, RevMatrix),
   diagd(RevMatrix, DiagD),
   reverse(DiagD, DiagU).*/

matr_mult(M1, M2, M3) :-
   matr_transp(M2, RM2),
   matr_transp_mult(M1, RM2, M3).

matr_transp_mult([], _, []).
matr_transp_mult([R1|M1], M2, [R3|M3]) :-
   matr_transp_mult1(R1, M2, R3),
   matr_transp_mult(M1, M2, M3).

matr_transp_mult1(_, [], []).
matr_transp_mult1(R1, [R2|M2], [X|R3]) :-
   inn_prod(R1, R2, X),
   matr_transp_mult1(R1, M2, R3).

inn_prod([], [], 0).
inn_prod([X1|R1], [X2|R2], IP1) :-
   inn_prod(R1, R2, IP2),
   IP1 is IP2 + X1 * X2.

between(LBound, RBound, LBound) :-
    LBound =< RBound. 
between(LBound, RBound, Result) :-
    LBound < RBound,
    NextLBound is LBound + 1,
    between(NextLBound, RBound, Result).

allbetween(LBound, RBound, []) :-
   LBound > RBound.
allbetween(LBound, RBound, [LBound|Rest]) :-
   LBound =< RBound,
   NextLBound is LBound+1,
   allbetween(NextLBound, RBound, Rest).

alternative_between(LBound, RBound, Result) :-
   allbetween(LBound, RBound, List),
   member(Result, List).

alternative_allbetween(LBound, RBound, List) :-
   findall(Result, between(LBound, RBound, Result), List).