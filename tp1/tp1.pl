% TODO:
% [] naturalidade
% [y] listar filhos
% [y] listar pais
% [y] listar tios
% [y] listar sobrinhos
% [y] listar avos
% [y] listar netos
% [y] listar bisavos
% [y] listar bisnetos
% [] listar primos
% [] listar casados
% [] listar nascidos num ano
% [] listar mortos num ano
% [] listar naturais de um sitio
% [] listar pessoas com uma certa idade
% [] listar filhos de casamento
% [] solucoes
% [] relacao entre duas pessoas
% [] invariantes do filho (info repetida, mais que 2 pais)
% [] invariantes da naturalidade (nascimento e morte, mais que uma naturalidade)
% [] casado com mais que uma pessoa? acho que nao se aplica se for a base de filhos
% [] por comentarios direito
% [] clauses
% [y] remover repetidos

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% TRABALHO PRÁTICO: EXERCÍCIO 01    2014/15

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

:- op( 900,xfy,'::' ).
:- dynamic filho/2.
:- dynamic pai/2.
:- dynamic irmao/2.
:- dynamic avo/2.
:- dynamic neto/2.
:- dynamic tio/2.
:- dynamic primo/2.
:- dynamic casado/2.

r :- consult('tp1.pl').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Arvore geneologica exemplo

% pais de joao
filho(joao_silva, carlos_silva).
filho(joao_silva, maria_silva).

% pais de jose
filho(jose_silva, carlos_silva).
filho(jose_silva, maria_silva).

% pais de carlos, avos paternos de jose e de joao
filho(carlos_silva, luis_silva).
filho(carlos_silva, joaquina_ferreira).

% pais de maria, avos maternos de jose e de joao
filho(maria_silva, pedro_moreira).
filho(maria_silva, eugenia_antunes).

% pais de antonio, irmao de carlos, tio de joao e jose
filho(antonia_soares, luis_silva).
filho(antonia_soares, joaquina_ferreira).

% flora soares, prima de joao e jose, filha de antonia e joao soares (tios de joao e jose)
filho(flora_soares, antonia_soares).
filho(flora_soares, joao_soares).

%
casado(antonia_soares,joao_soares).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pai: Pai,Filho -> {V,F}

filho(F, P) :- clause(pai(P,F), true) .

pai(P,F) :- filho(F, P).
pai(P,F) :- clause(irmao(M,F), true), clause(pai(P,M),true).
pai(P,F) :- clause(irmao(F,M), true), clause(pai(P,M),true).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado irmao: M,N -> {V,F}

irmao(M,N) :- pai(P, M), pai(P, N) , M \== N.
irmao(M,N) :- clause(irmao(N, M), true) .

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado avo: Avo,Neto -> {V,F}

avo(A,N) :- filho(N, X) , pai(A, X).
avo(A,N) :- irmao(N,I), clause(avo(A,I), true).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado neto: Neto,Avo -> {V,F}

neto(N, A) :- avo(A, N).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado tio: Tio,Sobrinho -> {V,F}

tio(T, S) :- pai(P, S), irmao(T, P).
tio( T1,S ) :- avo(A, S), filho(T2, A), pai(P, S), P \== T2, clause(casado( T1, T2) , true).
tio( T1,S ) :- avo(A, S), filho(T2, A), pai(P, S), P \== T2, clause(casado( T2, T1) , true).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado sobrinho: Sobrinho,Tio -> {V,F}

sobrinho(S, T) :- tio(T, S).


primo(X, Y) :- pai(P1, X), pai(P2, Y), irmao(P1, P2).

bisavo(BA, BN) :- avo(A, BN), pai(BA, A).

bisneto(BN, BA) :- bisavo(BA, BN).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: X,Y -> {V,F}

descendente(X,Y) :- filho(X,Y); filho(X,Z) , descendente(Z,Y).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendenteGrau: Descendente,Ascendente,Grau -> {V,F}
descendenteGrau(D,A,1) :- filho(D, A).
descendenteGrau(D,A,G) :- filho(D, F), descendenteGrau(F, A, Z), G is Z + 1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendentesGrau: Ascendente,Grau,Resultado -> {V,F}
descendentesGrau(A,G,R) :- solucoes(P, descendenteGrau(P, A, G), R).

descendenteAteGrau(D,A,N) :- descendenteGrau(D,A,G), G =< N.

descendentesAteGrau(A,G,R) :- solucoes(P, descendenteAteGrau(P,A,G), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado grau: X,Y,N -> {V,F}

grau(X,Y,1) :- filho(X,Y).
grau(X,Y, N) :- filho(X,Z) , grau(Z, Y, G), N is G+1.



+filho( F,P ) :: ( solucoes( X, (filho(F, X)), S),
                    comprimento(S, N),
                    N=<2
                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarfilhos: P,S -> {V,F}
listarfilhos( P,S ) :- solucoes( F,filho(F,P), S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarpais: F,S -> {V,F}
listarpais( F,S ) :- solucoes( P,pai(F,P), S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listartios: SOB,S -> {V,F}
listartios( SOB,S ) :- solucoes( T,tio(T,SOB),S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarsobrinhos: T,S -> {V,F}
listarsobrinhos( T,S ) :- solucoes( SOB,sobrinho(SOB,T),S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaravos: N,S -> {V,F}
listaravos( N,S ) :- solucoes( A,avo(A,N),S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarnetos: A,S -> {V,F}
listarnetos( A,S ) :- solucoes( A,neto(N,A),S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarbisavos: BN,S -> {V,F}
listarbisavos( BN,S ) :- solucoes( A,avo(A,BN),S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarbisnetos: BA,S -> {V,F}
listarbisnetos( BA,S ) :- solucoes( BA,bisneto(N,BA),S ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado removerelemento: [X | L], Y, [A | B] -> {V,F}
removerelemento( [],_,[] ).
removerelemento( [X|L],X,NL ) :- removerelemento( L,X,NL ).
removerelemento( [X|L],Y,[X|NL] ) :- X \== Y, removerelemento( L,Y,NL ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado removerelementorep: [X | L], [A | B] -> {V,F}
removerelementorep( [],[] ).
removerelementorep( [X|L],[X|NL] ) :- removerelemento( L,X,TL ), removerelementorep( TL,NL ).
	

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: [X | L], N -> {V,F}

comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.



solucoes(A, T, S) :- findall(A, T, S).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

insere(T) :- assert(T).
insere(T) :- retract(T), !, fail.

teste([]).
teste( [R|LR]) :-
        R,
        teste(LR).

evolucao(T) :- solucoes( I, +T::I, S),
                  insere(T),
                  teste(S).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido  [i]

+filho( F,P ) :: (solucoes( (F,P),(filho( F,P )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).
