% TODO:
% y - feito
% x - feito e testado
% [] naturalidade
% [x] listar filhos
% [x] listar pais
% [x] listar tios
% [x] listar sobrinhos
% [x] listar avos
% [x] listar netos
% [y] listar bisavos
% [y] listar bisnetos
% [] listar primos
% [] listar casados
% [] listar nascidos num ano
% [] listar mortos num ano
% [] listar naturais de um sitio
% [] listar pessoas com uma certa idade
% [] listar filhos de casamento
% [x] solucoes
% [] relacao entre duas pessoas
% [x] invariantes do filho (info repetida, mais que 2 pais)
% [] invariantes do pai (impedir que inserir pai something de cabo dos invariados do filho)
% [] invariantes da naturalidade (nascimento e morte, mais que uma naturalidade)
% [x] invariante casado com mais que uma pessoa, informacao repetida e casado(A, B)/casado(B,A)
% [] por comentarios direito
% [] clauses
% [x] remover repetidos

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
:- dynamic naturalidade/3.

r :-
    consult('tp1.pl').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Arvore geneologica exemplo
%
%                       ricardo
%                          |
%                         jose + maria
%                           /     \
%      manuel + margarida  /       \
%             |           /         \
%            ana  + jorge         carolina + luis
%               /    \                     |
%            joao   carlos               carla
%
%

% ana e jorge: pais de joao e carlos
filho(joao, jorge).
filho(joao, ana).
filho(carlos, jorge).
filho(carlos, ana).

% casamento entre ana e jorge
casado(ana, jorge).

% manuel e margarida: pais de ana, avos maternos de joao e carlos
filho(ana, manuel).
filho(ana, margarida).
casado(manuel, margarida).

% jose e maria: pais de jorge, avos paternos de joao e carlos
filho(jorge, jose).
filho(jorge, maria).
casado(jose, maria).

% carolina: irma de jorge, tia de joao e carlos
filho(carolina, jose).
filho(carolina, maria).

% luis, marido de carolina, tio de joao e carlos
% e sua filha carla, prima de joao e carlos
filho(carla, luis).
filho(carla, carolina).
casado(carolina, luis).

% ricardo, pai de jose, avo de jorge e carolina, bisavo de joao, carlos e carla
filho(jose,ricardo).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Testes exemplo

t1 :-
    pai(ana, joao), pai(ana, carlos).

t2 :-
    irmao(joao, carlos).

t3 :-
    avo(manuel, carlos).

t4 :-
    avo(margarida, joao).

t5 :-
    tio(carolina, joao).

t6 :-
    tio(luis, carlos).

t7 :-
    nao( tio(jorge, joao) ).

t8 :-
    avo(jose, carlos).

t9 :-
    primo(carlos, carla).

t10 :-
    irmao(jorge, carolina).

t11 :-
    nao( irmao(jorge,luis) ).

t12 :-
    neto(carla, maria).

t13 :-
    sobrinho(joao, luis).

t14 :-
    sobrinho(joao, carolina).

t15 :-
    pai(ricardo,jose).

t16 :-
    nao( avo(ricardo, ana) ).

teste_relacoes(L) :-
    test_all( [t1, t2, t3, t4, t5, t6, t7,
                t8, t9, t10, t11, t12, t13,
                t14, t15, t16], L ).

tl1 :-
    listarPais(joao, L), contemTodos(L, [ana, jorge]).

tl2 :-
    listarFilhos(jorge, L), contemTodos(L, [carlos, joao]).

tl3 :-
    listarAvos(joao, A), contemTodos(A, [jose, maria, manuel, margarida]).

tl4 :-
    listarNetos(maria, L), contemTodos(L, [joao, carlos, carla]).

tl5 :-
    listarTios(joao, L), contemTodos(L, [carolina, luis]).

tl6 :-
    listarSobrinhos(luis, L), contemTodos(L, [joao, carlos]).

tl7 :-
    listarNetos(ricardo, L), contemTodos(L, [jorge, carolina]).

tl6 :-
    listarBisnetos(ricardo, L), contemTodos(L, [carlos, carla, joao]).

tl7 :-
    listarBisavos(carlos, L), contemTodos(L, [ricardo]).

teste_listar(L) :-
    test_all( [tl1, tl2, tl3, tl4, tl5, tl6, tl7], L ).


ti1 :-
    nao( evolucao(filho(joao,x)) ).

ti2 :-
    nao( evolucao(filho(joao,jorge)) ).

ti3 :-
    nao( evolucao(ana,x) ).

ti4 :-
    nao( evolucao(x,ana) ).

ti5 :-
    nao( evolucao(ana,jorge) ).

ti6 :-
    nao( evolucao(jorge, ana) ).

teste_invariantes(L) :-
    test_all( [ti1, ti2, ti3, ti4, ti5, ti6], L).

teste_total(L) :-
    teste_listar(SL1),
    teste_invariantes(SL2),
    teste_relacoes(SL3),
    L = [SL1, SL2, SL3].

% funcoes auxiliares de teste
contem(H, [H|T]).
contem(X, [H|T]) :-
    contem(X, T).

contemTodos([], _).
contemTodos([H|T], L) :-
    contem(H, L), contemTodos(T, L).

test_all([], []).
test_all([H|T], L) :-
    H, test_all(T, L).

test_all([H|T], L) :-
    test_all(T, NL), L = [H|NL].

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pai: Pai,Filho -> {V,F}

filho(F, P) :-
    clause(pai(P,F), true) .

pai(P,F) :-
    filho(F, P).
pai(P,F) :-
    clause(irmao(M,F), true), clause(pai(P,M),true).
pai(P,F) :-
    clause(irmao(F,M), true), clause(pai(P,M),true).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado irmao: M,N -> {V,F}

irmao(M,N) :-
    pai(P, M), pai(P, N) , M \== N.
irmao(M,N) :-
    clause(irmao(N, M), true) .

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado avo: Avo,Neto -> {V,F}

avo(A,N) :-
    filho(N, X) , pai(A, X).
avo(A,N) :-
    irmao(N,I), clause(avo(A,I), true).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado neto: Neto,Avo -> {V,F}

neto(N, A) :-
    avo(A, N).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado tio: Tio,Sobrinho -> {V,F}

tio(T, S) :-
    pai(P, S), irmao(T, P).
tio( T1,S ) :-
    avo(A, S), filho(T2, A), nao( pai(T2, S) ), clause(casado( T1, T2) , true).
tio( T1,S ) :-
    avo(A, S), filho(T2, A), nao( pai(T2, S) ), clause(casado( T2, T1) , true).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado sobrinho: Sobrinho,Tio -> {V,F}

sobrinho(S, T) :-
    tio(T, S).


primo(X, Y) :-
    pai(P1, X), pai(P2, Y), irmao(P1, P2).

bisavo(BA, BN) :-
    avo(A, BN), pai(BA, A).

bisneto(BN, BA) :-
    bisavo(BA, BN).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: X,Y -> {V,F}

descendente(X,Y) :-
    filho(X,Y); filho(X,Z) , descendente(Z,Y).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendenteGrau: Descendente,Ascendente,Grau -> {V,F}
descendenteGrau(D,A,1) :-
    filho(D, A).
descendenteGrau(D,A,G) :-
    filho(D, F), descendenteGrau(F, A, Z), G is Z + 1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendentesGrau: Ascendente,Grau,Resultado -> {V,F}
descendentesGrau(A,G,R) :-
    solucoes(P, descendenteGrau(P, A, G), R).

descendenteAteGrau(D,A,N) :-
    descendenteGrau(D,A,G), G =< N.

descendentesAteGrau(A,G,R) :-
    solucoes(P, descendenteAteGrau(P,A,G), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado grau: X,Y,N -> {V,F}

grau(X,Y,1) :-
    filho(X,Y).
grau(X,Y, N) :-
    filho(X,Z) , grau(Z, Y, G), N is G+1.



+filho( F,P ) :: ( solucoes( X, (filho(F, X)), S),
                    comprimento(S, N),
                    N=<2
                  ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarFilhos: P,S -> {V,F}
listarFilhos( P,S ) :-
    solucoes( F,filho(F,P),NL ), removerRepetidos(NL, S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarPais: F,S -> {V,F}
listarPais( F,S ) :-
    solucoes( P,pai(P,F),NL ), removerRepetidos(NL, S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarTios: SOB,S -> {V,F}
listarTios( SOB,S ) :-
    solucoes( T,tio(T,SOB),NL ), removerRepetidos(NL, S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarSobrinhos: T,S -> {V,F}
listarSobrinhos( T,S ) :-
    solucoes( SOB,sobrinho(SOB,T),NL ), removerRepetidos(NL, S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarAvos: N,S -> {V,F}
listarAvos( N,S ) :-
    solucoes( A,avo(A,N),NL ), removerRepetidos(NL, S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarNetos: A,S -> {V,F}
listarNetos( A,S ) :-
    solucoes( N,neto(N,A),NL ), removerRepetidos(NL, S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarBisavos: BN,S -> {V,F}
listarBisavos( BN,S ) :-
    solucoes( A,avo(A,BN),NL ), removerRepetidos(NL, S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarBisnetos: BA,S -> {V,F}
listarBisnetos( BA,S ) :-
    solucoes( BA,bisneto(N,BA),NL ), removerRepetidos(NL, S).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado removerElemento: [X | L], Y, [A | B] -> {V,F}
removerElemento( [],_,[] ).
removerElemento( [X|L],X,NL ) :-
    removerElemento( L,X,NL ).
removerElemento( [X|L],Y,[X|NL] ) :-
    X \== Y, removerElemento( L,Y,NL ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado removerRepetidos: [X | L], [A | B] -> {V,F}
removerRepetidos( [],[] ).
removerRepetidos( [X|L],[X|NL] ) :-
    removerElemento( L,X,TL ), removerRepetidos( TL,NL ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: [X | L], N -> {V,F}

comprimento( [],0 ).
comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.


nao(T) :-
    T, !, fail.

nao(_).


solucoes(A, T, S) :-
    T,
    assert( tmp(A) ),
    fail.

solucoes(A, T, S) :-
    obter([], S).

obter(X,S) :-
    retract( tmp(A) ),
    !,
    obter([A|X], S).

obter(S,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

insere(T) :-
    assert(T).
insere(T) :-
    retract(T), !, fail.

teste([]).
teste( [R|LR]) :-
        R,
        teste(LR).

evolucao(T) :-
    solucoes( I, +T::I, S),
                  insere(T),
                  teste(S).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido  [i]

+filho( F,P ) :: (solucoes( (F,P),(filho( F,P )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

casado(X,Y) :- clause(casado(Y,X), true).

% so estar casado com uma pessoa
+casado( X,Y ) :: ( solucoes( Z,casado(X,Z),S ),
                    comprimento( S,N ),
                    N =< 1
                  ).

+casado( X,Y ) :: ( solucoes(Z, casado(Y,Z), S),
                    comprimento( S,N ),
                    N =< 1
                  ).

% nao inserir conhecimento repetido:
% se esta casado(X,Y) nao inserir
% casado(X,Y) nem casado(Y,X)
% mas isto nao fica coberto pelo caso de so estar casado com uma pessoa?
+casado( X,Y ) :: ( solucoes( (X,Y), casado(X,Y), S),
                    comprimento( S,N ),
                    N == 1
                  ).

+casado( X,Y ) :: ( solucoes( (X,Y), casado(Y,X), S),
                    comprimento( S,N ),
                    N == 1
                  ).



% isto serve para testar, mas no fim e para remover
% o rui usou isto, disponibilizou no grupo, e deles
whynot( T , ER) :-
    solucoes(I,+T::I,S),
    assert(T),
    why_not_teste(S, ER),
    retract(T).

why_not_teste([],ok).
why_not_teste([I|L], ER) :- I, why_not_teste(L, ER).
why_not_teste([I|L], ER) :- head(L,ER).

head([H|L], H).
