%deslocada_de([a,b,c,d,e,f,g],3,X).
insere_final([], E, E) :- ! .

insere_final([H|T], E, [H|X]) :-
    insere_final(T, E, X).

separa_elem_final([A], [], A) :- !.

separa_elem_final( [H|T], [H|L1], X) :-
	separa_elem_final(T, L1, X).

desloc_dir(L, 0, L) :- !.

desloc_dir(L, N, X) :-
	N1 is N +1,
	separa_elem_final(L, L1, X1),
	desloc_dir([X1|L1], N1, X).

desloc_esq(L, 0, L) :- !.
desloc_esq([H|T], N, X) :-
	N1 is N -1,
	insere_final(T, [H], X1),
	desloc_esq(X1, N1, X).

deslocada_de(L, N, X) :-
	N > 0,
	desloc_esq(L,N,X).

deslocada_de(L, N, X) :-
	N < 0,
	desloc_dir(L,N,X).

deslocada_de(L, 0, L) :- !.	




