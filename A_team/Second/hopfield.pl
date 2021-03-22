/* Function to transpose a given matrix M (1xN) to M_T (Nx1) */

matr_transpose(M,M_T):- 
    matr_transpose(M, M_T, []).

matr_transpose([], M_T, AccMatr):-
    M_T = AccMatr.

matr_transpose([H|T], M_T, AccMatr):-

    append(AccMatr, [[H]], F),
    matr_transpose(T, M_T, F).

/* Given 1 number N , creates an NxN Identity matrix (All values are 0 , except the main diagonal) */

identity_matr(M, N, I, Value):-

    identity_matr(M, N, I , [], Value).

identity_matr(_, -1, I , AccMatr, _):-

    reverse(AccMatr, I).

identity_matr(M, N, I, AccMatr, Value):-

    N =\= -1,
    zao(M, N, Zeros, Value),
    append(AccMatr, [Zeros], F),
    N1 is N-1, 
    identity_matr(M, N1, I, F, Value).

/* Creates a matrix ( length's equal to M ) which all the values are zero, except the N-ith index, which it's value == Value */

/*Zeros and One*/

zao(M,N,Zeros,Value):-         
    
    M > N,
    N >= 0,
    zao(M,N,Zeros,[],Value).

zao(0, _ , Zeros, AccMatr, _):-

    Zeros = AccMatr.

zao(M, 0, Zeros, AccMatr,Value):-

    M =\= 0,
    append(AccMatr, [Value], F),
    M1 is M-1,
    zao(M1,-1,Zeros,F,Value).   

zao(M,N,Zeros,AccMatr,Value):-

    M =\= 0,
    N =\= 0,
    N1 is N-1,
    M1 is M-1,
    append(AccMatr, [0], F),    
    zao(M1,N1,Zeros,F,Value).

/*================================*/

/* Function to transpose bigger matrices (and not vectors), NxM to MxN */

matr_transp(M, []) :-
   empty_lists(M).
matr_transp(M, [C|TM]) :-
   del_first(M, C, RM),
   matr_transp(RM, TM).

empty_lists([]).
empty_lists([[]|M]) :-
   empty_lists(M).


/* Function to multiply matrices */

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

/*Function to compute the length of an inner list from a list of lists */

inner_length([H|_], Length):-

    H \=[],
    length(H,Length).



list_addition(L1,L2,L3):-
    list_addition(L1,L2,L3,[]).
    
list_addition([],[],L3,AccList):-
    L3 = AccList.

list_addition([H1|T1], [H2|T2], L3, AccList):-

    H3 is H1 + H2,
    append(AccList, [H3], F),
    list_addition(T1,T2,L3,F).

/* Function which adds up 2 different matrices */


matr_add(M1,M2,M3):-

    /*
    If we want to check if the matrices have all the same rows and cols

    length(M1, L1),
    length(M2,L2),
    inner_length(M1,LL1),
    inner_length(M2,LL2),
    LL1 = LL2,
    L1 = L2,
    
    */
    matr_add(M1,M2,M3,[]).

matr_add([[]], M2, M2).

matr_add([],[], M3, AccMatr):-
    M3 = AccMatr.

matr_add([H1|T1], [H2|T2], M3, AccMatr):-

    H1 \= [],
    H2 \= [],

    list_addition(H1,H2,H3),

    append(AccMatr, [H3], F),

    matr_add(T1,T2,M3,F).

/* This function will take multiple matrices and will add them together, recursively */


mult_add([H|[]],W):-

    matr_add([[]],H,W).

mult_add([H|T],W):-

    T \=[],
    mult_add(T,L2),

    matr_add(H,L2,W).    

/* Function to compute the first term of the hopefield's formula */

hop_first(M,H1):-   
    hop_first(M,H1,[]).

hop_first([], H1, AccMatr):-
    H1 = AccMatr.

hop_first([H|T],H1,AccMatr):-

    matr_transpose(H,H_T),     
    /* Y^T */  
    matr_mult(H_T,[H], M),
    /* (Y^T) * (Y) */ 
    append(AccMatr, [M], F),

    hop_first(T, H1, F).

/* Function to compute the second term of the hopefield's formula */

hop_second(Matrix,H2):-

    length(Matrix,Value),
    inner_length(Matrix,N),
    N1 is N-1,
    ValueR is Value * (-1),
    identity_matr(N, N1, H2, ValueR).

/* Our main 'hopefield' function */

hopfield(M,W):-

    hop_first(M,H1),
    hop_second(M,H2),
    append(H1,[H2],H3),
    mult_add(H3,W).

