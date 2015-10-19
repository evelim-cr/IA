pessoas(0, []) :- !.
pessoas(N, [(_Homem,_Cor,_Bebida,_Cigarro,_Animal)|T]) :-
	N1 is N-1,
	pessoas(N1, T).

pessoa(1, [H|_], H) :- !.
pessoa(N, [_|T], R) :-
	N1 is N-1,
	pessoa(N1, T, R).

dica1([(ingles,vermelha,_,_,_)|_]).
dica1([_|T]) :- dica1(T).

dica2([(sueco,_,_,_,cachorro)|_]).
dica2([_|T]) :- dica2(T).

dica3([(dinamarques,_,cha,_,_)|_]).
dica3([_|T]) :- dica3(T).

dica4([(_,verde,_,_,_),(_,branca,_,_,_)|_]).
dica4([_|T]) :- dica4(T).

dica5([(_,verde,cafe,_,_)|_]).
dica5([_|T]) :- dica5(T).

dica6([(_,_,_,pallmall,passaro)|_]).
dica6([_|T]) :- dica6(T).

dica7([(_,amarelo,_,dunhill,_)|_]).
dica7([_|T]) :- dica7(T).

dica8(Pessoas) :-
	pessoa(3, Pessoas, (_,_,leite,_,_)).

dica9(Pessoas) :-
	pessoa(1, Pessoas, (noruegues,_,_,_,_)).

dica10([(_,_,_,blends,_),(_,_,_,_,gato)|_]).
dica10([(_,_,_,_,gato),(_,_,_,blends,_)|_]).
dica10([_|T]) :- dica10(T).

dica11([(_,_,_,dunhill,_),(_,_,_,_,cavalo)|_]).
dica11([(_,_,_,_,cavalo),(_,_,_,dunhill,_)|_]).
dica11([_|T]) :- dica11(T).

dica12([(_,_,cerveja,blueMaster,_)|_]).
dica12([_|T]) :- dica12(T).

dica13([(alemao,_,_,prince,_)|_]).
dica13([_|T]) :- dica13(T).

dica14([(noruegues,_,_,_,_),(_,azul,_,_,_)|_]).
dica14([(_,azul,_,_,_),(noruegues,_,_,_,_)|_]).
dica14([_|T]) :- dica14(T).

dica15([(_,_,_,blends,_),(_,_,agua,_,_)|_]).
dica15([(_,_,agua,_,_),(_,_,_,blends,_)|_]).
dica15([_|T]) :- dica15(T).

pergunta([(_,_,_,_,peixe)|_]).
pergunta([_|T]) :- pergunta(T).

solucao(Pessoas) :-
	pessoas(5, Pessoas),
	dica1(Pessoas),
	dica2(Pessoas),
	dica3(Pessoas),
	dica4(Pessoas),
	dica5(Pessoas),
	dica6(Pessoas),
	dica7(Pessoas),
	dica8(Pessoas),
	dica9(Pessoas),
	dica10(Pessoas),
	dica11(Pessoas),
	dica12(Pessoas),
	dica13(Pessoas),
	dica14(Pessoas),
	dica15(Pessoas),
	pergunta(Pessoas).