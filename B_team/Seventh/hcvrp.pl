/* Include hcvrp_data.pl program */ 
:- [hcvrp_data]. 

/* Load the ic library from ECLiPSe */
:- lib(ic).

/* Load the ic_global library from ECLiPSe */
:- lib(ic_global).

/* Load the branch and bround library from ECLiPSe */
:- lib(branch_and_bound).

/* The (hcvrp/6) accusation */ 
hcvrp(NCl, NVe, _, _, _, _) :-

    /*Retrieve the number of Clients */
    clients(CL),
    /*Retrieve the numer of Vehicles */
    vehicles(VE),

    /* Retrieve the first N clients */ 
    getItems(CL, NCl, NClients),
    /* Add a dummy client in the start of List */
    addDummyClient(NClients, NClientsT),

    /* Retrieve the first N capacities of vehicles */
    getItems(VE, NVe, Capacities),

    /* Create the distance matrix among the clients */
    distances(NClientsT, Distances),
    /* Create the flatten-distances matrix */
    flatten(Distances, FLDistances),

    /* Create the demands matrix within every client */
    demands(NClientsT, Demands, []),

    /* Create the list of Variables */
    matrix(NVe, NCl, Variables, []),
    /* Create the flatten-variables list  */
    flatten(Variables, FLVariables),

    /* Declare the range of variables */
    /* From 0 up to NClients */
    varsDomain(FLVariables, NCl),

    /* First Constraint */
    /* Each client must be server by exactly 1 truck */
    uniqueTruck(FLVariables, NCl),

    /* Second Constraint */
    /* Each client that will be served from each truck, must be not overcome the truck's total capacity */
    capacityLimit(Variables, Capacities, Demands), 

    /* Third Constraint */
    /* All Zeros must be after the non-zero values */
    zeros(Variables), 

    /* Cost of our Solution */
    /* Compute the Cost, that's will be the total distance our trucks have traveled */
    trucksDistance(Distances, Variables, Cost, []),

    writeln(Cost),
    writeln(Demands),
    writeln(FLVariables),
    writeln(Capacities),
    writeln(Distances).


/* Get the first N items from a given list L */
getItems(L, N, Items) :-
    getItems(L, N, Items, []).

getItems(_, 0, ItemsAcc, ItemsAcc).
getItems([L1 | L2], N, Items, ItemsAcc) :-
    append(ItemsAcc, [L1] , ItemsAcc_),
    N_ is N - 1,
    getItems(L2, N_, Items, ItemsAcc_).

/* Add a virtual client in the start of list */
addDummyClient([NC1 | NC2], NClients) :-
    NCD = c(0, 0, 0),
    append([], [NCD], NClients_),
    append(NClients_, [NC1], NClients__),
    append(NClients__, NC2, NClients).

/* Create the distance matrix */
/* Using Euclidean distance */
distances(Clients, Distances) :-
    distances(Clients, Clients, Distances, []).

distances([], _,DistancesAcc, DistancesAcc).
distances([CL1 | CL2], Clients, Distances, DistancesAcc) :-
    /* Create row for each Client */
    distanceRow(CL1, Clients, Row, []),

    append(DistancesAcc, [Row], DistancesAcc_),
    distances(CL2, Clients, Distances, DistancesAcc_).

/* Create row for the distance's matrix */
distanceRow(_, [], RowAcc, RowAcc).
distanceRow(CL, [CL1 | CL2], Row, RowAcc) :-

    /* Retrieve the cordinates for client */
    cordinates(CL, Cords0),
    /* Retrieve the cordinates for client 1 */
    cordinates(CL1, Cords1),
    /*Compute the Euclidean Distance between these 2 clients */
    euclidean(Cords0, Cords1, Distance),

    append(RowAcc, [Distance], RowAcc_),
    distanceRow(CL, CL2, Row, RowAcc_).


/* Retrieve the (X , Y) cordinates from a client */
cordinates(Client, Cords) :-
    c(_, X, Y) = Client,
    append([], [X, Y], Cords).

/* Computes the euclidean distance between 2 cordinates (X1, Y1) , (X2, Y2) */
euclidean([X1 , Y1], [X2 , Y2], Distance) :-
    
    Ddec is sqrt( (X2 - X1)^2 + (Y2 - Y1)^2 ),
    Distance is integer(round(Ddec * 1000)).

/* Retrieve the N-first demands of the item, for every client */
demands([], DemandsAcc, DemandsAcc).
demands([CL1 | CL2], Demands, DemandsAcc) :-
    c(Demand, _, _) = CL1,
    append(DemandsAcc, [Demand], DemandsAcc_),
    demands(CL2, Demands, DemandsAcc_).

/* Create N x M matrix, in 2D array, or (list[lists]) */
matrix(0, _, VarsAcc, VarsAcc).
matrix(N, M, Variables, VarsAcc) :-
    N > 0,
    length(Row, M),
    append(VarsAcc, [Row], VarsAcc_),
    N_ is N - 1,
    matrix(N_, M, Variables, VarsAcc_).

/* Declare the domain of the N x M variables */
varsDomain(FLVariables, NCl) :-
    FLVariables #:: 0..NCl.

/* Unique Truck constraint */
/* Thats means, that each client must be server by exactly 1 truck */
uniqueTruck(_, 0).
uniqueTruck(Clients, NCl) :-

    occurrences(NCl, Clients, 1),
    NCl_ is NCl - 1,
    uniqueTruck(Clients, NCl_).

/* Capacity Truck Limit constraint */
/* Clients must not overcome the total truck capacity limit */
capacityLimit([], [], _).
capacityLimit([TruckVars1 | TruckVars2], [C1 | C2], Demands) :-

    capacityTruckLimit(TruckVars1, Demands, [], C1), 
    capacityLimit(TruckVars2, C2, Demands).

/* Capacity Truck Limit for EACH individual truck and its variables */ 
capacityTruckLimit([], _, DemandsVarsAcc, C) :-
    sum(DemandsVarsAcc) #< C.
capacityTruckLimit([V1 | V2], Demands, DemVarsAcc, C) :-
    element(V1, Demands, D),
    append(DemVarsAcc, [D], DemandsVarsAcc_),
    capacityTruckLimit(V2, Demands, DemandsVarsAcc_, C). 


/* Third Constraint */
zeros([]).
zeros([ListVar1 | ListVar2]) :- 
    
    followByZeros(ListVar1),
    zeros(ListVar2).

/* All 0 must be in order */
followByZeros([_|[]]).
followByZeros([V1 | V2]) :-

    V2 \= [],
    getHead(V2, Head),    
    V1 #= 0 => Head #= 0,
    followByZeros(V2).

/* Returns the head of a given list L */
getHead([L1 | _], L1).

/* Cost accusation */
/* We'll compute for each possible solution, the total distance each truck went through */
trucksDistance(_, [], Cost, CostList) :-
    Cost #= sum(CostList).
trucksDistance(Distances, [Vars1 | Vars2], Cost, CostList) :-

    Vars1 \= [],
    writeln(Vars1),
    computeDistance(Distances, Vars1, TruckCost, [], 0),
    append(CostList, [TruckCost], CostList_),
    trucksDistance(Distances, Vars2, Cost, CostList_).

/* Computes the cost of transpotations for each truck */
computeDistance(_, [], TruckCost, TruckCostAcc, _) :-
    TruckCost #= sum(TruckCostAcc).
computeDistance(Distances, [V1 | V2], TruckCost, TruckCostAcc, Point) :-

    V1 \= [],
    write("V1 is: "),writeln(V1),
    write("Point is: "),writeln(Point),
    get_ith(Point, Distances, Dist_i),
    write("Dist is :"),writeln(Dist_i),
    element(V1, Dist_i, C1),
    append(TruckCostAcc, [C1], TruckCostAcc_),
    computeDistance(Distances, V2, TruckCost, TruckCostAcc_, V1).

/* Get the i-th element from a list */
get_ith(0, [X|_], X).
get_ith(I, [_|L], X) :-
    I > 0,
    I1 is I-1,
    get_ith(I1, L, X).