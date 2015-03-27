% TODO:
% [] naturalidade
% [] listar filhos
% [] listar pais
% [] listar tios
% [] listar sobrinhos
% [] listar avos
% [] listar netos
% [] listar bisavos
% [] listar bisnetos
% [] listar primos
% [] listar casados
% [] listar nascidos num ano
% [] listar mortos num ano
% [] listar naturais de um sitio
% [] listar pessoas com uma certa idade
% [] solucoes
% [] relacao entre duas pessoas
% [] invariantes do filho (info repetida, mais que 2 pais)
% [] invariantes da naturalidade (nascimento e morte, mais que uma naturalidade)
% [] casado com mais que uma pessoa? acho que nao se aplica se for a base de filhos
% [] por comentarios direito
% [] clauses

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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pai: Pai,Filho -> {V,F}

filho(F, P) :- clause(pai(P,F), true) .

pai(P,F) :- filho(F, P).
pai(P,F) :- clause(irmao(M,F), true), clause(pai(P,M),true).
pai(P,F) :- clause(irmao(F,M), true), clause(pai(P,M),true).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado irmao: M,N -> {V,F}

irmao(M,N) :- pai(P, M), pai(P, N).
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

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado sobrinho: Sobrinho,Tio -> {V,F}

sobrinho(S, T) :- tio(T, S).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado casado: M,N -> {V,F}

casado(M, N) :- filho(F, M), filho(F, N).


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
