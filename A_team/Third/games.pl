/* Inner product between two matrices */
inn_prod([], [], 0).
inn_prod([X1|R1], [X2|R2], IP1) :-
   inn_prod(R1, R2, IP2),
   IP1 is IP2 + X1 * X2.

/* Creates a list from 1 to N */
do_list(N, L):- 
    N > 0,
    findall(Num, between(1, N, Num), L).

/* Finds all the numbers between LBound and RBound */
between(LBound, RBound, LBound) :-
    LBound =< RBound. 
between(LBound, RBound, Result) :-
    LBound < RBound,
    NextLBound is LBound + 1,
    between(NextLBound, RBound, Result).

/* Finds the sum of a given list */
sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

/* Function to "Refill" our bucket with K chips */
refill(X, T, K, T_, TStable):-

    /* We can't play a game more times than the the number of the available chips */
    T1 is T - X,
    T1 >= 0, 

    /* We can't let our bucket T be overflowed with more chips than T */
    proper_refill(T1, K, TStable, T_).

/* Function to proper "Refill" our bucket with K or less, chips */
proper_refill(T1, K , TStable, T_):-
    
    K > 0, 
    TNew is T1 + K,
    TNew =< TStable,
    T_ = TNew.

proper_refill(T1, K , TStable, T_):-

    K > 0,
    TNew is T1 + K,
    TNew > TStable,
    K_ is K - 1,
    proper_refill(T1, K_, TStable, T_).

/* Function to play all the possible combinations of games for each variable */
/* If a game has negative pleasure, then play it only one time*/
member2(X , _, P):-
    P < 0,
    X = 1.

member2(X, Chips, P):-
    P >= 0,
    member(X, Chips).

/* This function will return all the possible,legal games that can be played */
solution(Chips, T, K, Variables, P, Pleasure):-
    solution(Chips, T, K, Variables, P, [], Pleasure, T, Pleasure).


solution(_, _, K, [], P, Sol_, Pleasure, TStable, _):-

    /* We must play ALL the chips in a combination of games. Eitherwise, our solution is not even the optimal */

    length(Sol_, M),
    sum_list(Sol_,Sum),
    Sum =< TStable + (M-1)*K,
    inn_prod(Sol_, Pleasure, P).

solution(Chips, T, K, [X|X_], P, SolAcc, Pleasure, TStable, [P1|P2]):-

    /* X_i variable will be played Chips_j times| i = {1,2,...,L} , j={1,2,...,T} */
    member2(X, Chips, P1),

    /* Refill the bucket and check if the constraints are valid */
    refill(X, T, K, T1, TStable),

    /* Create the matrix with the possible games that can be played */
    append(SolAcc, [X], Sol_),

    /* Try all the possible combinations */
    solution(Chips, T1, K, X_, P, Sol_, Pleasure, TStable, P2).


games(Ps, T, K, Gs, P):-

    /* L has the number of the games we have to play */
    length(Ps, L),

    /* Create a list with the variables x_1, x_2, ... , x_L */
    length(Gs2, L),

    /* Create a list with the possible values that our variables can take*/
    do_list(T, C),

    /* Find all the possible solutions acccording to the constraints */
    findall( p(Gs2,P2), solution(C, T, K, Gs2, P2, Ps), M),

    /* Found the max of a given list */
    findmax(M, MaxA),
    P = MaxA,

    /* Return recursively all the solutions, one by one */
    member(p(Gs,P), M).


/* A function to seperate the 2 sublists from the findall list */
/* I don't need it anymore, but i'll let it to exist peacefully for future implementations */
seperator(L, Times, Pleasure):-
    seperator(L, Times, Pleasure, [], []).

seperator([], Times, Pleasure, TimesAcc, PleasureAcc):-
    Times = TimesAcc,
    Pleasure = PleasureAcc.

seperator([T1|T2], Times, Pleasure, TimesAcc, PleasureAcc):-

    times_pleasure(T1, T, P),
    append(TimesAcc, [T], Times_),
    append(PleasureAcc, P, Pleasure_),
    seperator(T2, Times, Pleasure, Times_, Pleasure_).

times_pleasure([T|P], T, P).

/* Returns the biggest element from a given list */
maxlist2( [M], M).
maxlist2( [X,Y | L], M):-
    maxlist2( [Y|L], M1), max(X,M1,M).

/* Returns the biggest element from a double list */

findmax( [p(_,A)], A).
findmax( [p(_,A1)|L], MaxA) :-
    findmax(L, MaxL),
    max2(A1, MaxL, MaxA).

max2(X,Y,X) :- X >= Y.
max2(X,Y,Y) :- X < Y.

/* Returns 2 same indexed lists , which each pleasure it's matching with his time's list */
maxPT(P, T, FinalP, FinalT, Max):-
    maxPT(P, T, FinalP, FinalT, Max, [], []).

maxPT([], [], FinalP, FinalT, _, AccP, AccT):-
    FinalP = AccP, 
    FinalT = AccT.

maxPT([P1|P2], [T1|T2], FinalP, FinalT, Max, AccP, AccT):-

    check(P1, Max, AccP, AccT, T1, AccP_, AccT_),
    maxPT(P2, T2, FinalP, FinalT, Max, AccP_, AccT_).


/* Checks if 2 items are the same. If they are the same, then append the items in another list */

check(P, Max, [], [], _, AccP_, AccT_):-
    P \= Max, !,
    AccP_ = [],
    AccT_ = [].

check(P, Max, AccP, AccT, _, AccP, AccT):-
    P \= Max, !.

check(P, Max, AccP, AccT, T, AccP_, AccT_):-

    member(P, [Max]),
    append(AccP, [P], AccP_),
    append(AccT, [T], AccT_).