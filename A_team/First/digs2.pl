/* If our list ([[]]) contains 2 or more lists */
moreThanOne([ Head | Tail]) :- 
    Head \= [],
    Tail \= [].

/* Finds the last element from given list L */
lastElement(L,X):- append(_, [X], L).

/* Finds the first element from given list L */
firstElement(L,X):- append([X], _ , L).
/* Or => firstElement([X | _] , X).

/* Deletes the last element from given list L1 */
deleteLast(L1,L2):-  
    lastElement(L1,X),
    delete(X, L1, L2).

/* Search across a list of lists and deletes the element X if it's inside in some sublist */

deleteFromLists(_, [],L3):-
    writeln(L3).
deleteFromLists(X,L,L3):-
    delete(X,L,L3).

/* Deletes the first value (head) that is in a list */
deleteHead([_|T],L):-
    L = T.

/* Deletes all the heads of each given sublist. Then, a Final sublist is returned */

deleteAll([],[], L4, Final):-
    Final = L4.
deleteAll([_|T1],[H2|T2],L_,Final):-
    deleteHead(H2,L3),
    /*deleteFromLists(H1,H2,L3),*/
    append(L_, [L3], L4),
    /*L4 = Final,*/
    /*Final = L4,*/
    deleteAll(T1,T2,L4,Final).    

/* Check if a list is empty */
emptyList(L):- L = [].
/* Check if 2 lists are both empty */
isEmpty(L1,L2):- L1 = [] , L2 = [].

/* Pop the first item from L1, return the result into L2 and X takes the value from this item */
popFirst(L1,L2,X):-
    firstElement(L1,X),
    delete(X, L1, L2).

/* Checks if 2 lists have the same length */
lengthSimilarity([],[]). 
lengthSimilarity(L1 , L2):-
    deleteLast(L1, L1_Smaller),
    deleteLast(L2, L2_Smaller),
    lengthSimilarity(L1_Smaller, L2_Smaller).

/* Checks if a Matrix input is legitable(all list has to have the same length) */
isMatrix( [_ | []]).
isMatrix([H|T]):-
    firstElement(T,T1),
    lengthSimilarity(H,T1),
    isMatrix(T).

/* Accumulator Function. Creates each time the Diagonals */
createDiags([],[]).
createDiags([H|T],D):-
    firstElement(H,X),
    createDiags(T,D2),
    append(D2,[X],D).

/* Remove Head */
removeHead([_|T],L):-
    L=T.

/* Reverse the lists that are inside a bigger list */

reverseLists([],[]).
reverseLists([H|T],L):-     
    reverse(H,H_),
    reverseLists(T,L2),
    append(L2,[H_],L).


/* Function to compute the Up diagonals */
/* M = The first Matrix, DU is going to be final matrix, L is going to be the auxilliary list and D is our accumulator for diagonals */

diagsUP(M, DU, [[]|T], Diags):-
    writeln('D1'),
    M \=[],
    diagsUP(M, DU, T, Diags).

diagsUP(M,DU, [[]], Diags):-
    writeln('D2'),
    M \= [],
    diagsUP(M,DU,[],Diags).

diagsUP([], DU, [[]], Diags):-
    writeln('D3'),
    DU = Diags.

/* For: [[a,m,d] , [d,e,f]] */

diagsUP([], DU, L , Diags):-

    writeln('D4'),
    writeln(L),
    L \= [[]],
    createDiags(L,D),
    reverse(D,DR),
    deleteAll(DR, L, [], F),
    append(Diags, [DR], DU_),
    diagsUP([], DU, F, DU_).

diagsUP([],DU,[H|T],Diags):-

    writeln('D5'),
    H = [], T \= [],    
    /*removeHead(L,D_),*/
    createDiags(T,D),
    reverse(D,DR),
    deleteAll(DR,T, [], F),
    append(Diags, [DR], DU_),
    diagsUP([], DU, F, DU_).

diagsUP(M,DU,L,Diags):-

    M \= [],
    writeln('D6'),
    writeln(M),
    popFirst(M,M2,SubList),

    append(L,[SubList],L2),

    createDiags(L2,D),
    reverse(D,DR),

    deleteAll(DR,L2,[],F),  
    writeln(F),

    append(Diags,[DR],DU_),
    diagsUP(M2,DU,F,DU_).

/* Our main "function" */
diags(M,DU,DD):-
    /*moreThanOne(M),    
    isMatrix(M).*/
    diagsUP(M,DU,[],[]).
    /*reverse(M,DDR),
    diagsUP(DDR,DD_,[],[]),
    reverseLists(DD_,DD__),
    reverse(DD__,DD).*/

rand_matr(0, _, []).
rand_matr(K, N, [R|M]) :-
   K > 0,
   rand_row(N, R),
   K1 is K - 1,
   rand_matr(K1, N, M).

rand_row(0, []).
rand_row(N, [X|R]) :-
   N > 0,
   X is random mod 10,
   N1 is N - 1,
   rand_row(N1, R). 