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

r :- consult('pratico.pl').

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
% Extensao do predicado pai: P,F -> {V,F}

pai(P,F) :- filho(F, P).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado irmao: M,N -> {V,F}

irmao(M,N) :- pai(P, M), pai(P, N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado avo: A,N -> {V,F}

avo(A,N) :- filho(N, X) , pai(A, X).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado neto: N,A -> {V,F}

neto(N, A) :- avo(A, N).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: X,Y -> {V,F}

descendente(X,Y) :- filho(X,Y); filho(X,Z) , descendente(Z,Y).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado grau: X,Y,N -> {V,F}

grau(X,Y,1) :- filho(X,Y).
grau(X,Y, N) :- filho(X,Z) , grau(Z, Y, G), N is G+1.



+filho( F,P ) :: ( findall( X, (filho(F, X)), S),
                    comprimento(S, N),
                    N=<2
                  ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: [X | L], N -> {V,F}

comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

insere(T) :- assert(T).
insere(T) :- retract(T), !, fail.

teste([]).
teste( [R|LR]) :-
        R,
        teste(LR).

evolucao(T) :- findall( I, +T::I, S),
                  insere(T),
                  teste(S).


% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido  [i]

+filho( F,P ) :: (findall( (F,P),(filho( F,P )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).
