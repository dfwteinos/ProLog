/* Load the ic library from ECLiPSe */
:- lib(ic).

/* Inner product between two matrices */
inn_prod([], [], 0).
inn_prod([X1|R1], [X2|R2], IP1) :-
   inn_prod(R1, R2, IP2),
   IP1 is IP2 + X1 * X2.

/* Second constraint */
secondCR(Gs,T,K):-
    
    reverse(Gs,GsR),
    writeln(GsR),
    sec_con(GsR,GsR,T,K),
    reverse(GsR,GsR_),
    Gs = GsR_.

sec_con([],[],_,_).
sec_con(Gs_,[_|Tail], T, K):-
    
    length(Gs_,M),
    Sum is T+(M-1)*K,
    writeln(Sum),
    sum(Gs_) #=<(T+(M-1)*K),
    sec_con(Tail,Tail, T, K).

/* Third constraint */
thirdC(Gs,T,K):-
    third_con(Gs,Gs,T,K).

third_con([],[],_,_).
third_con(Gs_,[_|Tail], T, K):-

    length(Gs_,M),
    sum(Gs_) + K #>T + (M-1) * K,
    sec_con(Tail,Tail, T, K).

solution(Ps, T, K, GsR, P):-

    /* L has the length of the const Ps_1, Ps_2, ... , Ps_n */    
    length(Ps,L),                   

    /* Create a list with the variables x_1, x_2, ... , x_n */
    length(GsR_, L),

    /* First constraint, games(==variables) can be played from 1 up to T times. */
    GsR_ :: 1..T,

    /* Second constraint x_1 + ... + x_n <= T + (i-1)K */
    /* We can't play a game more times than the available chips everytime */
    secondCR(GsR_,T,K),

    /* Third constraint x_1 + ... + x_n > T + (i-1)K */
    /* We can't exceed each time the max number of T chips */
    thirdC(GsR_,T,K),

    /* We must apply our constraint in linear order */
    GsR = GsR_,

    /* Search for all the solutions, based on the upper constraints */
    search(GsR, 0, input_order, indomain, complete, []),

    /* We must find the pleasure for each combination of games */
    inn_prod(GsR, Ps, P).


games(Ps, T, K, Gs, P):-

    /* Find all the games according to our constraints */
    findall( p(Gs2,P2) , solution(Ps, T, K, Gs2, P2), L),

    /* Found the max of a given list */
    findmax(L, MaxA),
    P = MaxA,

    /* Return recursively all the solutions, one by one */
    member(p(Gs,P), L).

/* A function to seperate the 2 sublists from the findall list */
seperator(L, Times, Pleasure):-
    seperator(L, Times, Pleasure, [], []).

seperator([], Times, Pleasure, TimesAcc, PleasureAcc):-
    Times = TimesAcc,
    Pleasure = PleasureAcc.

seperator([T1|T2], Times, Pleasure, TimesAcc, PleasureAcc):-

    writeln(T1),
    times_pleasure(T1, T, P),
    append(TimesAcc, [T], Times_),
    append(PleasureAcc, P, Pleasure_),
    seperator(T2, Times, Pleasure, Times_, Pleasure_).

times_pleasure([T|P], T, P).

/* Returns the biggest element from a given list */
maxlist2( [M], M).
maxlist2( [X,Y | L], M):-
    maxlist2( [Y|L], M1), max(X,M1,M).

/* Returns 2 same indexed lists , which each pleasure it's matching with his time's list */
maxPT(P, T, FinalP, FinalT, Max):-
    maxPT(P, T, FinalP, FinalT, Max, [], []).

maxPT([], [], FinalP, FinalT, _, AccP, AccT):-
    FinalP = AccP, 
    FinalT = AccT.

maxPT([P1|P2], [T1|T2], FinalP, FinalT, Max, AccP, AccT):-

    writeln(P1),
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

/* Returns the biggest element from a double list */

findmax( [p(_,A)], A).
findmax( [p(_,A1)|L], MaxA) :-
    findmax(L, MaxL),
    max2(A1, MaxL, MaxA).

max2(X,Y,X) :- X >= Y.
max2(X,Y,Y) :- X < Y.