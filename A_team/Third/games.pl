/* Load the ic library from ECLiPSe */
:- lib(ic).

/* Inner product between two matrices */
inn_prod([], [], 0).
inn_prod([X1|R1], [X2|R2], IP1) :-
   inn_prod(R1, R2, IP2),
   IP1 is IP2 + X1 * X2.

/* Third constraint of our program. We can't exceed the bucket T */


chipsT(Gs, K):-

    reverse(Gs,Gs),
    no_exceed(Gs_),
    Gs = Gs_.

/* We make sure that each combination of terms are respecting our constraint */

no_exceed([H|T],Gs)


games(Ps, T, K, Gs, P):-

    /* L has the length of the const Ps_1, Ps_2, ... , Ps_L */    
    length(Ps,L),                   

    /* Create a list with the variables x_1, x_2, ... , x_L */
    length( Gs, L),

    /* First constraint, variables must be equal to the sum of total chips */
    sum(Gs) #= T + (L-1) * K,

    /* Second constraint, we can't play a single game more times than our max chips (T) */    
    Gs :: 1..T,

    /* Third constraint, we can't overfill the bucket T with chips > T. */
    chipsT(Gs,K),

    /* Search for all the solutions, based on the upper constraints */
    search(Gs, 0, input_order, indomain, complete, []),

    inn_prod(Gs, Ps, P).


games2(Ps2, T2, K2, Gs2, P2):-

    findall( [Gs,P2] , games(Ps2, T2, K2, Gs, P2), L),

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