/* Load the ic library from ECLiPSe */
:- lib(ic).

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

    writeln('In refill:'),
    writeln('X IN REFILL IS:'),
    writeln(X),
    writeln('And T in refill is:'),
    writeln(T),
    /* We can't play a game more times than the the number of the available chips */
    T1 is T - X,
    T1 >= 0, 
    writeln('T1 is:'),
    writeln(T1),

    /* We can't let our bucket T be overflowed with more chips than T */
    T2 is T1 + K,
    writeln('T2 is:'),
    writeln(T2),
    T2 =< TStable,
    writeln('New T is:'),
    writeln(T2),

    /* If these constraints are valid, then continue the process */
    T_ = T2.


/* This function will return all the possible,legal games that can be played */

solution(Chips, T, K, Variables, P, Pleasure):-
    writeln('ti ston poutso???'),

    writeln(Chips),
    writeln(T),
    writeln(K),
    writeln(Variables),
    writeln(Pleasure),
    solution(Chips, T, K, Variables, P, [], Pleasure, T).


solution(_, _, K, [], P, Sol_, Pleasure, TStable):-

    /* We must play ALL the chips in a combination of games. Eitherwise, our solution is not even the optimal */
    
    writeln('Sol2'),
    writeln('One solution is:'),
    writeln(Sol_),
    length(Sol_, M),
    sum_list(Sol_,Sum),
    writeln('Sum is:'),
    writeln(Sum),
    writeln('Optimal is:'),
    writeln(TStable + (M-1)*K),
    Sum =:= TStable + (M-1)*K,
    writeln('Sum equals to opt_sum'),

    /*Sol = Sol_,*/
    inn_prod(Sol_, Pleasure, P).

solution(Chips, T, K, [X|X_], P, SolAcc, Pleasure, TStable):-

    writeln('Sol1'),

    /* X_i variable will be played Chips_j times| i = {1,2,...,L} , j={1,2,...,T} */
    delete(X, Chips, _),
    writeln('X is:'),
    writeln(X),
    writeln('T is:'),
    writeln(T),

    /* Refill the bucket and check if the constraints are valid */
    refill(X, T, K, T1, TStable),
    writeln('T1 is:'),
    writeln(T1),

    /* Create the matrix with the possible games that can be played */
    append(SolAcc, [X], Sol_),
    writeln(Sol_),

    /* Try all the possible combinations */
    solution(Chips, T1, K, X_, P, Sol_, Pleasure, TStable).


games(Ps, T, K, Gs, P):-

    /* L has the number of the games we have to play */
    length(Ps, L),

    /* Create a list with the variables x_1, x_2, ... , x_L */
    length(Gs2, L),

    /* Create a list with the possible values that our variables can take*/
    do_list(T, C),

    /*findall( [Sol], solution(C, T, K, Gs, P, Sol, Ps), L ).*/

    findall( p(Gs2,P2), solution(C, T, K, Gs2, P2, Ps), M),

    /* Found the max of a given list */
    findmax(M, MaxA),
    P = MaxA,
    writeln('Max is:'),
    writeln(P),

    /* Return recursively all the solutions, one by one */
    member(p(Gs,P), M).


    /*findall( [Gs,P2] , solution(Ps2, T2, K2, Gs, P2), L),

    seperator(L, Times, Pleasure),

    maxlist2(Pleasure, M),

    maxPT(Pleasure, Times, FinalPleasure, FinalTimes, M),

    delete(Gs2,FinalTimes, _),

    P2 = M,
    
    writeln(FinalPleasure),
    writeln(FinalTimes).
    /*Find_Max_Pleasure(L, Ps2),
    writeln(L).*/



/* A function to seperate the 2 sublists from the findall list */
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