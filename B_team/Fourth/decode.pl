/* Include diags.pl program */
:- [diags].

/* Load the ic library from ECLiPSe */
:- lib(ic).

/* Function to create a matrix size of R * C */

matrix(R, C, Matrix):-

    matrix(R, C, Matrix, []).
    
matrix(R, _, Matrix, AccMatr):-

    R = 0,
    Matrix = AccMatr.

matrix(R, C, Matrix, AccMatr):-

    /* Repeat until we exhaust all rows */
    R > 0,

    /* Create a row with C variables */
    length(Row, C),

    /* Append the row into the matrix */
    append(AccMatr, [Row], Matr),

    /* Reduce rows by 1 */
    R_ is R - 1,

    /* Call until rows are 0 */
    matrix(R_, C, Matrix, Matr).

/* Rows & diagonals pixels constraint */
rows_diags_constr([], []).
rows_diags_constr([RH|RL], [MH|ML]):-

    /* Sum of each row must be equal to each element of the Rows list */
    sum(MH) #= RH,

    rows_constr(RL, ML).


/* Cols pixels constraint */

cols_constr([], _).
cols_constr([CH|CL], Matrix):-

    each_head(Matrix, Heads, RedMatr),

    /* Apply constraints */
    sum(Heads) #= CH,

    cols_constr(CL, RedMatr).

/* Returns the head of each list in the Matrix */
/* Also, returns the reduced matrix */
each_head(Matrix, Heads, RedMatr):-
    each_head(Matrix, Heads, [], RedMatr, []).

each_head([], Heads, AccHeads, RedMatr, AccRedMatr):-
    
    Heads = AccHeads, 
    RedMatr = AccRedMatr.

each_head([MH|MT], Heads, AccHeads, RedMatr, AccRedMatr):-

    head(MH, Head, Reduced),

    /* The list with the heads of each list */
    append(AccHeads, [Head], AccHeads_),

    /* The list with the reduced matrices */
    append(AccRedMatr, [Reduced], AccRedMatr_),

    /* Repeat until we have no more rows */
    each_head(MT, Heads, AccHeads_, RedMatr, AccRedMatr_).

/* Returns the head of a list and the list without the first item */
head([H|L], H, L).

/* Decode Program */
decode(Rows, Cols, DD, DU):-

    /* RL has the length of the Rows list */
    length(Rows, RL),

    /* CL has the length of the Columns list */
    length(Cols, CL),

    /* Create a RL * CL matrix */
    matrix(RL, CL, Matrix),

    /* declare the values that the variables can take */
    values(Matrix),
    /* Row's constraint */
    rows_diags_constr(Rows, Matrix),

    /* Col's constraint */
    cols_constr(Cols, Matrix),

    /* Create the DUP & DDOWN variables */
    diags(Matrix, DUP, DDOWN),

    /* Diags Down constraint */
    rows_diags_constr(DD, DDOWN),

    /* Diags Up constraint */
    rows_diags_constr(DU, DUP), 

    search(Matrix, 0, input_order, indomain, complete, []),

    write_out(Matrix).


/* Function to write the pixels */

write_out([]).
write_out([MH|MT]):-

    writeln("\n"),
    write_row(MH),
    write_out(MT).

/* write each row out */

write_row([]).

write_row([H|L]):-

    H > 0, 
    write("* "),
    write_row(L).

write_row([H|L]):-

    H = 0,
    write('. '),
    write_row(L).

values([]).
values([M|L]):-
    M :: 0..1,
    values(L).