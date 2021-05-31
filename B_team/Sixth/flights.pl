/* Include flight_data.pl program */ 
:- [flight_data]. 

/* Load the ic library from ECLiPSe */
:- lib(ic).

/* Load the branch and bround library from EXLiPSe */
:- lib(branch_and_bound).

run :- member(I,[1, 2]),
    write('I = '), writeln(I),
    flights(I, Pairings, Cost),
    write('Pairings = '), writeln(Pairings),
    write('Cost = '), writeln(Cost), nl, fail.


/* Creates a list from 1 to N */
do_list(N, L):-
    findall(Num, between(1, N, Num), L).

/* Finds all the numbers between LBound and RBound */
between(LBound, RBound, LBound) :-
    LBound =< RBound.

between(LBound, RBound, Result) :-
    NextLBound is LBound + 1, 
    NextLBound =< RBound, 
    between(NextLBound, RBound, Result).


/* Checks if element E is inside at list L */

checkIfExists(E , L, VarList, VarList) :-
    nonmember(E, L).

checkIfExists(E, L, VarList, NewVarList) :-

    member(E, L),
    append(VarList, [L], NewVarList).

/* If Flight it's inside at Pairs_i, append Pairs_i list */
/* HP = HeadPair , TP = TailPair */
flightList(Flight, Pairs, CombList) :-
    flightList(Flight, Pairs, CombList, []).

flightList(_, [], CombList, VarList) :-
    CombList = VarList.

flightList(Flight, [HP | TP], CombList, VarList) :-

    /* Checks if Flight is inside at [HP] list */
    checkIfExists(Flight, HP, VarList, NewVarList),

    flightList(Flight, TP, CombList, NewVarList).

/* This function will demand all the possible combinations for each flight, to has value from 0 to 1 */
applyBinaryConstraint(_, []).
applyBinaryConstraint(Vars, [I1 | I2]) :-

    get_ith(I1, Vars, Var),
    Var #:: [0,1],
    applyBinaryConstraint(Vars, I2).

/* This function will demand that all the combinations must have sum == 1 */
applySumConstraint(Vars, Indexes) :-
    applySumConstraint(Vars, Indexes, []).

applySumConstraint(Vars, [], VarAcc) :-
    sum(VarAcc) #= 1.
applySumConstraint(Vars, [I1 | I2], VarAcc) :-

    get_ith(I1, Vars, Var),
    append(VarAcc, [Var], VarAcc_),
    applySumConstraint(Vars, I2, VarAcc_).

/* Get the index of current item in a list */
indexOf([Element|_], Element, 0).
indexOf([_|Tail], Element, Index):-
    indexOf(Tail, Element, Index1), 
    Index is Index1+1.  

/* Get the i-th element from a list */
get_ith(0, [X|_], X).
get_ith(I, [_|L], X) :-
    I > 0,
    I1 is I-1,
    get_ith(I1, L, X).

/* Get the indexes of specific elements, from a given list */
getIndexes(Pairs, CombList, Indexes):-
    getIndexes(Pairs, CombList, Indexes, []).

getIndexes(_, [], IndexesAcc, IndexesAcc).
getIndexes(Pairs, [HC | TC], Indexes, IndexesAcc) :-

    /* Get index of current element */
    indexOf(Pairs, HC, Index),

    /* Append this index to Index Accumulator List */
    append(IndexesAcc, [Index], IndexesAcc_),

    /* Repeat until there are no items left */
    getIndexes(Pairs, TC, Indexes, IndexesAcc_).


/* Get the elements of specific indexes, from a given list */
getVariables(Vars, Indexes, Variables) :-  
    getVariables(Vars, Indexes, Variables, []).

getVariables(_, [], VariablesAcc, VariablesAcc).
getVariables(Vars, [HI | TI], Variables, VariablesAcc) :-

    /* Get the element of current index */
    get_ith(HI, Vars, Var),

    /* Append this element to Var Accumulator List */
    append(VariablesAcc, [Var], VariablesAcc_),

    /* Repeat until there are no items left */
    getVariables(Pairs, TI, Variables, VariablesAcc_).

/* Function to create Cost constraint */
cost_constr(Vars, Costs, Cost) :-
    cost_constr(Vars, Costs, Cost, 0).

cost_constr([], [], Cost, CostAcc) :-
    Cost #= CostAcc.
cost_constr([V1 | V2], [C1 | C2], Cost, CostAcc) :-

    CostAcc_ #= CostAcc + V1 * C1,
    cost_constr(V2, C2, Cost, CostAcc_).

/* Is Positive Function */
isPositive(Var, PosVarsAcc, PosVarsAcc_, _) :-
    Var #= 0,
    PosVarsAcc_ = PosVarsAcc.

isPositive(Var, PosVarsAcc, PosVarsAcc_, Index) :-
    Var #= 1, 
    append(PosVarsAcc, [Index], PosVarsAcc_).

/* Get the positive variables ( > 0) */
getPositiveVariables(Vars, PosVarsIndexes) :-
    getPositiveVariables(Vars, PosVarsIndexes, [], 0).

getPositiveVariables([], PosVarsIndexesAcc, PosVarsIndexesAcc, _).
getPositiveVariables([V1 | V2], PosVarsIndexes, PosVarsIndexesAcc, Index) :-

    isPositive(V1, PosVarsIndexesAcc, PosVarsIndexesAcc_, Index),
    Index_ is Index + 1,
    getPositiveVariables(V2, PosVarsIndexes, PosVarsIndexesAcc_, Index_).


/* This function is going to apply our desired constraints */
/* HF = HeadFlight, TF = TailFlight */
applyConstraints([], Pairs, Vars, PairLength, Costs) :-

    writeln("Applying cost constraint"),
    cost_constr(Vars, Costs, Cost), 

    /* Get only the variables that are equal to 1 */
    getPositiveVariables(Vars, PosVarsIndexes),
    write("Pos vars are: "), writeln(PosVarsIndexes), !,

    /* Search for all the solutions, based on the upper constraints */
    bb_min(search(Vars, 0, first_fail, indomain, complete, []),
        Cost, bb_options{solutions:all}),

    write("Cost is : "), writeln(Cost).


applyConstraints([HF | TF], Pairs, Vars, PairLength, Costs) :-

    HF \= [],
    write('HF IS: '), writeln(HF),
    /* For each flight, create a list with all the combinations that has this flight */
    flightList(HF, Pairs, CombList),
    write("Comb list is: ") ,writeln(CombList),

    /* Find the index of each item in the list */
    getIndexes(Pairs, CombList, Indexes),
    write("Indexes are: "), writeln(Indexes),

    /* Get the i-th elements from Variable list 
    getVariables(Vars, Indexes, Variables),
    write("vars are: "), writeln(Variables), */

    /* Each combination can take 0 or 1 as value */
    applyBinaryConstraint(Vars, Indexes),
    /* The sum of the expression in the bigger list, must be 1 */
    applySumConstraint(Vars, Indexes),

    applyConstraints(TF, Pairs, Vars, PairLength, Costs).

/* Function to create Variables */
createVars(Pairs, Vars, PairLength) :-  

    /* Retrireve the Length M of pairs */    
    length(Pairs, PairLength),

    /* Create x_M vars */
    length(Vars, PairLength).

/* The function that will do all the work */
flights(I, Pairings, Cost)  :-   

    writeln("ti ston poutso pali1"),
    /* Flights has the number of flights */
    /* Pairs has the lists of possible pairings */
    /* C has the list of each cost for each pair */
    get_flight_data(I, Flights, Pairs, C),

    /*writeln("ti ston poutso pali2"),*/
    /*Create a list from 1 to Flights */
    do_list(Flights, NumFlights),

    writeln("ti ston poutso pali3"), 
    /* Create Variables, fromo 1 to M */ 
    createVars(Pairs, Vars, PairLength),

    writeln("ti ston poutso pali4"),
    /* Apply constraints */
    applyConstraints(NumFlights, Pairs, Vars, PairLength, C),

    writeln("ti ston poutso pali5").