/* Assigment 3 */
/* Group: Hannah Adams, Paul Freeman, Marcos Zavala */


/* Exercise 1 */

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).


/* Part 1 A */
schedule(X,Y,Z) :- enroll(X,A), where(A,Y), when(A,Z).	

/* Part 1 B */
usage(P,T) :- where(A,P), when(A,T).	

/* Part 1 C */
conflict(Y,X ):- when(Y,A), when(X,A), where(Y,B), where(X,B) ,X\=Y.

/* Part 1 D */
meet(X,Y) :- enroll(X,A), enroll(Y,A), X\=Y.
meet(X,Y) :- enroll(X,A), enroll(Y,B), when(A,C), inc(C, C1), when(B,C1), where(A,D), where(B,D), X\=Y.
meet(X,Y) :- enroll(X,A), enroll(Y,B), when(A,C), dec(C, C1), when(B,C1), where(A,D), where(B,D), X\=Y.

inc(X, X1) :- X1 is X+1.
dec(X, X1) :- X1 is X -1.

/* Exercise 2 */
/* Part 2 A */
rdups([], []).
rdups([H | T], T1) :- member(H, T), rdups(T, T1).
rdups([H | T], [H | T1]) :- not(member(H, T)), rdups(T, T1).

/* Part 2 B */
flatten(List, Flattened):- flatten(List, [], Flattened).
flatten([], Flattened, Flattened).
flatten([Item|Tail], L, Flattened):- flatten(Item, L1, Flattened), flatten(Tail, L, L1).
flatten(Item, Flattened, [Item|Flattened]):- \+ is_list(Item).

/*Part 2 C */
project(A, B, C):- project0(A, B, C, 1).
project0(_, [], [], _).
project0([], _, [], _).
project0([X|_], [L],  [L], C):- X = C.
project0([X|_], [_], [], C):- X\=C.
project0([X|L1], [Y|L2],[Y|L] ,C):- X= C, C1 is 1 + C, project0(L1, L2, L, C1).
project0([X|L1], [_|L2],L ,C):- X\= C, C1 is 1 + C, project0([X|L1], L2, L, C1).

