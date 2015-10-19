moeda_diferente(Moedas, Posicao, Peso) :-
	primeiraPesagem(Moedas, Pesagem1),
	segundaPesagem(Moedas, [Pesagem1], Pesagem2),
	terceiraPesagem(Moedas, [Pesagem1, Pesagem2], Pesagem3),
	descobreMoeda([Pesagem1, Pesagem2, Pesagem3], Posicao, Peso).


%
% soma(L1, L2, S1, S2)
%
%  L1: Uma lista qualquer.
%  L2: Outra lista qualquer.
%  S1: Somatório dos elementos de L1.
%  S2: Somatório dos elementos de L2.
%
% Soma os elementos de L1 e coloca os resultados em S1. E também
% soma os elementos de L2, retornando em S2.
%
soma([], [], 0, 0) :- ! .

soma([V1|T1], [V2|T2], S1, S2) :-
	soma(T1, T2, NovoS1, NovoS2),
	S1 is NovoS1+V1,
	S2 is NovoS2+V2.


%
% compara(S1, S2, R)
%
%  S1: Primeiro número.
%  S2: Segundo número.
%  R: Resultado da comparação (maior, igual ou menor).
%
% Compara S1 com S2 e devolve maior, igual ou menor em R.
%
compara(S1, S2, maior) :- S1 > S2.
compara(S1, S2, igual) :- S1 =:= S2.
compara(S1, S2, menor) :- S1 < S2.


%
% pesa(L1, L2, R)
%
%  L1: Lista de moedas 1.
%  L2: Lista de moedas 2.
%  R: Resultado da pesagem (maior, igual ou menor).
%
% Compara os pesos das moedas em L1 com as moedas de L2.
% Devolve maior, igual ou menor em R se L1 for mais pesada,
% igual ou menos pesada do que L2, respectivamente.
%
pesa(L1, L2, R) :-
	soma(L1, L2, S1, S2),
	compara(S1, S2, R).


%
% primeiraPesagem(Moedas, Pesagem1)
%
%  Moedas: Lista das moedas.
%  Pesagem1: Resultado da primeira pesagem.
%
% Realiza a primeira pesagem e retorna o resultado em Pesagem1.
%
primeiraPesagem([M1,M2,M3,M4,M5,M6,M7,M8,_,_,_,_], Pesagem1) :-
	pesa([M1,M2,M3,M4], [M5, M6, M7, M8], Pesagem1).


%
% segundaPesagem(Moedas, L, Pesagem2)
%
%  Moedas: Lista das moedas.
%  L: Lista contendo apenas o resultado da primeira pesagem.
%  Pesagem2: Resultado da segunda pesagem.
%
% Decide quais moedas utilizar na segunda pesagem, efetua a pesagem e
% devolve o resultado em Pesagem2.
%
segundaPesagem([_,_,M3,M4,M5,M6,M7,M8,_,_,_,_], [maior], Pesagem2) :-
	pesa([M3,M5,M6], [M4,M7,M8], Pesagem2).

segundaPesagem([_,_,_,_,_,M6,M7,M8,_,M10,M11,M12], [igual], Pesagem2) :-
	pesa([M6,M7,M8], [M10,M11,M12], Pesagem2).

segundaPesagem([_,_,M3,M4,M5,M6,M7,M8,_,_,_,_], [menor], Pesagem2) :-
	pesa([M3,M5,M6], [M4,M7,M8], Pesagem2).


%
% terceiraPesagem(Moedas, L, Pesagem3)
%
%  Moedas: Lista das moedas.
%  L: Lista dos resultados das 2 pesagens anteriores.
%  Pesagem3: Resultado da terceira pesagem.
%
% A partir das duas primeiras pesagens, decide quais moedas pesar e
% efetua a pesagem devolvendo o resultado em Pesagem3.
%
terceiraPesagem([_,_,_,_,_,_,M7,M8,_,_,_,_], [maior, maior], Pesagem3) :-
	pesa([M7], [M8], Pesagem3).

terceiraPesagem([M1,M2,_,_,_,_,_,_,_,_,_,_], [maior, igual], Pesagem3) :-
	pesa([M1], [M2], Pesagem3).

terceiraPesagem([_,_,_,_,M5,M6,_,_,_,_,_,_], [maior, menor], Pesagem3) :-
	pesa([M5], [M6], Pesagem3).


terceiraPesagem([_,_,_,_,_,_,_,_,_,M10,M11,_], [igual, maior], Pesagem3) :-
	pesa([M10], [M11], Pesagem3).

terceiraPesagem([_,_,_,_,M5,_,_,_,M9,_,_,_], [igual, igual], Pesagem3) :-
	pesa([M5], [M9], Pesagem3).

terceiraPesagem([_,_,_,_,_,_,_,_,_,M10,M11,_], [igual, menor], Pesagem3) :-
	pesa([M10], [M11], Pesagem3).


terceiraPesagem([_,_,_,_,M5,M6,_,_,_,_,_,_], [menor, maior], Pesagem3) :-
	pesa([M5], [M6], Pesagem3).

terceiraPesagem([M1,M2,_,_,_,_,_,_,_,_,_,_], [menor, igual], Pesagem3) :-
	pesa([M1], [M2], Pesagem3).

terceiraPesagem([_,_,_,_,_,_,M7,M8,_,_,_,_], [menor, menor], Pesagem3) :-
	pesa([M7], [M8], Pesagem3).


%
% descobreMoeda(L, Posicao, Peso)
%
%  L: Lista dos resultados das 3 pesagens.
%  Posicao: Posição da moeda diferente.
%  Peso: Peso da moeda diferente ('Menos pesada' ou 'Mais pesada').
%
% Verifica os resultados das pesagens e descobre a moeda diferente.
%
descobreMoeda([maior,maior,maior], 8, 'Menos pesada').
descobreMoeda([maior,maior,igual], 3, 'Mais pesada').
descobreMoeda([maior,maior,menor], 7, 'Menos pesada').

descobreMoeda([maior,igual,maior], 1, 'Mais pesada').
descobreMoeda([maior,igual,menor], 2, 'Mais pesada').

descobreMoeda([maior,menor,maior], 6, 'Menos pesada').
descobreMoeda([maior,menor,igual], 4, 'Mais pesada').
descobreMoeda([maior,menor,menor], 5, 'Menos pesada').


descobreMoeda([igual,maior,maior], 11, 'Menos pesada').
descobreMoeda([igual,maior,igual], 12, 'Menos pesada').
descobreMoeda([igual,maior,menor], 10, 'Menos pesada').

descobreMoeda([igual,igual,maior], 9, 'Menos pesada').
descobreMoeda([igual,igual,menor], 9, 'Mais pesada').

descobreMoeda([igual,menor,maior], 10, 'Mais pesada').
descobreMoeda([igual,menor,igual], 12, 'Mais pesada').
descobreMoeda([igual,menor,menor], 11, 'Mais pesada').


descobreMoeda([menor,maior,maior], 5, 'Mais pesada').
descobreMoeda([menor,maior,igual], 4, 'Menos pesada').
descobreMoeda([menor,maior,menor], 6, 'Mais pesada').

descobreMoeda([menor,igual,maior], 2, 'Menos pesada').
descobreMoeda([menor,igual,menor], 1, 'Menos pesada').

descobreMoeda([menor,menor,maior], 7, 'Mais pesada').
descobreMoeda([menor,menor,igual], 3, 'Menos pesada').
descobreMoeda([menor,menor,menor], 8, 'Mais pesada').



% ===================================================================

%
% test_all
%
% Serve para testar o predicado moeda_diferente.
%
test_all :-
    moeda_diferente([1,2,2,2,2,2,2,2,2,2,2,2], Posicao, Peso),
    write(Posicao), nl, write(Peso), nl,

    moeda_diferente([2,1,2,2,2,2,2,2,2,2,2,2], Posicao2, Peso2),
    write(Posicao2), nl, write(Peso2), nl,

    moeda_diferente([2,2,1,2,2,2,2,2,2,2,2,2], Posicao3, Peso3),
    write(Posicao3), nl, write(Peso3), nl,

    moeda_diferente([2,2,2,1,2,2,2,2,2,2,2,2], Posicao4, Peso4),
    write(Posicao4), nl, write(Peso4), nl,

    moeda_diferente([2,2,2,2,1,2,2,2,2,2,2,2], Posicao5, Peso5),
    write(Posicao5), nl, write(Peso5), nl,

    moeda_diferente([2,2,2,2,2,1,2,2,2,2,2,2], Posicao6, Peso6),
    write(Posicao6), nl, write(Peso6),  nl,

    moeda_diferente([2,2,2,2,2,2,1,2,2,2,2,2], Posicao7, Peso7),
    write(Posicao7), nl, write(Peso7), nl,

    moeda_diferente([2,2,2,2,2,2,2,1,2,2,2,2], Posicao8, Peso8),
    write(Posicao8), nl, write(Peso8), nl,

    moeda_diferente([2,2,2,2,2,2,2,2,1,2,2,2], Posicao9, Peso9),
    write(Posicao9), nl, write(Peso9), nl,

    moeda_diferente([2,2,2,2,2,2,2,2,2,1,2,2], Posicao10, Peso10),
    write(Posicao10), nl, write(Peso10), nl,

    moeda_diferente([2,2,2,2,2,2,2,2,2,2,1,2], Posicao11, Peso11),
    write(Posicao11), nl, write(Peso11), nl,

    moeda_diferente([2,2,2,2,2,2,2,2,2,2,2,1], Posicao12, Peso12),
    write(Posicao12), nl, write(Peso12), nl,

    moeda_diferente([3,2,2,2,2,2,2,2,2,2,2,2], Posicao13, Peso13),
    write(Posicao13), nl, write(Peso13), nl,

    moeda_diferente([2,3,2,2,2,2,2,2,2,2,2,2], Posicao14, Peso14),
    write(Posicao14), nl, write(Peso14), nl,

    moeda_diferente([2,2,3,2,2,2,2,2,2,2,2,2], Posicao15, Peso15),
    write(Posicao15), nl, write(Peso15), nl,

    moeda_diferente([2,2,2,3,2,2,2,2,2,2,2,2], Posicao16, Peso16),
    write(Posicao16), nl, write(Peso16), nl,

    moeda_diferente([2,2,2,2,3,2,2,2,2,2,2,2], Posicao17, Peso17),
    write(Posicao17), nl, write(Peso17), nl,

    moeda_diferente([2,2,2,2,2,3,2,2,2,2,2,2], Posicao18, Peso18),
    write(Posicao18), nl, write(Peso18), nl,

    moeda_diferente([2,2,2,2,2,2,3,2,2,2,2,2], Posicao19, Peso19),
    write(Posicao19), nl, write(Peso19), nl,

    moeda_diferente([2,2,2,2,2,2,2,3,2,2,2,2], Posicao20, Peso20),
    write(Posicao20), nl, write(Peso20), nl,

    moeda_diferente([2,2,2,2,2,2,2,2,3,2,2,2], Posicao21, Peso21),
    write(Posicao21), nl, write(Peso21), nl,

    moeda_diferente([2,2,2,2,2,2,2,2,2,3,2,2], Posicao22, Peso22),
    write(Posicao22), nl, write(Peso22), nl,

    moeda_diferente([2,2,2,2,2,2,2,2,2,2,3,2], Posicao23, Peso23),
    write(Posicao23), nl, write(Peso23), nl,

    moeda_diferente([2,2,2,2,2,2,2,2,2,2,2,3], Posicao24, Peso24),
    write(Posicao24), nl, write(Peso24).