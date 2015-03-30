% TODO:
% y - feito
% x - feito e testado
% [x] naturalidade
% [x] idade
% [x] listar filhos
% [x] listar pais
% [x] listar tios
% [x] listar sobrinhos
% [x] listar avos
% [x] listar netos
% [x] listar bisavos
% [x] listar bisnetos
% [x] listar primos
% [x] listar casados
% [x] listar nascidos num ano
% [x] listar mortos num ano
% [x] listar naturais de um sitio
% [x] listar pessoas com uma certa idade
% [x] listar filhos de casamento
% [x] solucoes
% [x] relacao entre duas pessoas
% [x] invariantes do filho (info repetida, mais que 2 pais)
% [x] invariantes do pai (impedir que inserir pai something de cabo dos invariantes do filho)
% [x] invariantes da naturalidade (nascimento e morte, mais que uma naturalidade)
% [x] invariante casado com mais que uma pessoa, informacao repetida e casado(A, B)/casado(B,A)
% [x] remover repetidos
% [] por comentarios direito
%
% predicados:
%   [x] remocao (tem de verificar predicados de remocao)
%   [x] ascendente
%   [x] ascendenteGrau
%   [x] ascendenteAteGrau
%   [x] novo naturalidade
%   [x] dataMorte
%
% Invariantes:
% [] nao colocar conhecimento repetido em:
%   [] ascendente
%   [] descendente
%   [] descendenteGrau
%   [] descendenteAteGrau
%   [] ascendenteGrau
%   [] ascendenteAteGrau
% [y] so pode ter 2 pais
% [y] so pode ter 4 avos
% [y] so pode ter 8 bisavos
% [] so ter uma relacao com um individuo (exceto primos e casados)
% [] nao ter relacionamento com ele proprio
% [] data nascimento < data morte
% [] uma so naturalidade
% [] filho entre nascimento e morte dos pais:
%   [] invariante quando se adiciona o filho
%   [] invariante quando se adiciona o pai
%   [] invariante quando se adiciona a naturalidade do filho
%   [] invariante quando se adiciona a naturalidade do pai
%   [] invariante quando se adiciona a dataMorte do pai
%   [] invariante quando se adiciona a dataMorte do filho

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
:- dynamic sobrinho/2.
:- dynamic bisavo/2.
:- dynamic bisneto/2.
:- dynamic descendente/2.
:- dynamic ascendente/2.
:- dynamic descendenteGrau/3.
:- dynamic descendenteAteGrau/3.
:- dynamic naturalidade/3.
:- dynamic dataMorte/2.

r :-
    consult('tp1.pl').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Arvore genealogica exemplo
%
%                 alexandre
%                     |
%                  ricardo + sara
%                          |
%     manuel + margarida  jose + maria
%            |              /     \
%            |             /       \
%            |            /         \
%           ana  +  jorge         carolina + luis
%               / \                        |
%           joao   carlos                carla
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

% ricardo e sara, pais de jose, avos de jorge e carolina, bisavos de joao, carlos e carla
filho(jose,ricardo).
filho(jose, sara).
casado(ricardo, sara).

% alexandre, pai de ricardo, avo de jose, bisavo de jorge e carolina, trisavo de joao, carlos e carla
filho(ricardo, alexandre).


% naturalidades e mortes
naturalidade(ricardo,porto,1872).
dataMorte(ricardo, 1922).

naturalidade(jose,porto,1910).
dataMorte(jose,1982).

naturalidade(margarida,aveiro,1910).
dataMorte(margarida,2003).

naturalidade(maria,braga,1933).
dataMorte(maria,2003).

naturalidade(ana,braga,1960).
dataMorte(ana,2010).

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

teste_predicados(L) :-
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

tl8 :-
    listarPrimos(joao, L), contemTodos(L, [carla]).

tl9 :-
    listarPrimos(carla, L), contemTodos(L, [joao,carlos]).

tl10 :-
    listarCasados(carolina, L), contemTodos(L, [luis]).

tl11 :-
    listarNascidos(1910, L), contemTodos(L, [jose,margarida]).

tl12 :-
    listarMortos(2003, L), contemTodos(L, [margarida,maria]).

tl13 :-
    listarNaturais(porto, L), contemTodos(L, [ricardo,jose]).

tl14 :-
    listarComIdade(50, L), contemTodos(L, [ana,ricardo]).

tl15 :-
    listarFilhosDeCasamento(manuel,margarida, L), contemTodos(L, [ana, jorge]).

teste_listar(L) :-
    test_all( [tl1, tl2, tl3, tl4, tl5, tl6, tl7, tl8, tl9, tl10, tl11, tl12, tl13, tl14, tl15], L ).


ti1 :-
    nao( evolucao(filho(joao,x)) ).

ti2 :-
    nao( evolucao(filho(joao,jorge)) ).

ti3 :-
    nao( evolucao(pai(carla,luis)) ).

ti4 :-
    nao( evolucao(pai(miguel,ana)) ).

ti5 :-
    nao( evolucao(ana,x) ).

ti6 :-
    nao( evolucao(x,ana) ).

ti7 :-
    nao( evolucao(ana,jorge) ).

ti8 :-
    nao( evolucao(jorge, ana) ).

teste_invariantes(L) :-
    test_all( [ti1, ti2, ti3, ti4, ti5, ti6, ti7, ti8], L).


tr1 :-
    relacao(joao, carlos, irmao).

tr2 :-
    relacao(carlos, joao, irmao).

tr3 :-
    relacao(ana, carlos, pai).

tr4 :-
    relacao(joao, carla, primo).

tr5 :-
    relacao(manuel, joao, avo).

tr6 :-
    relacao(carlos, margarida, neto).

tr7 :-
    relacao(carlos, carla, primo).

tr8 :-
    relacao(carla, joao, primo).

tr9 :-
    relacao(luis, joao, tio).

tr10 :-
    relacao(carla, ana, sobrinho).

tr11 :-
    relacao(carolina, carlos, tio).

tr12 :-
    relacao(ricardo, jose, pai).

tr13 :-
    relacao(jorge, ricardo, neto).

tr14 :-
    relacao(ricardo, joao, bisavo).

tr15 :-
    relacao(carolina, alexandre, bisneto).

tr16 :-
    relacao(alexandre, carlos, ascendente\ de\ grau\ 4).

tr17 :-
    relacao(joao, alexandre, descendente\ de\ grau\ 4).

tr18 :-
    relacao(luis, alexandre, desconhecido).

tr19 :-
    relacao(ana, carolina, desconhecido).


teste_relacoes(L) :-
    test_all([tr1, tr2, tr3, tr4, tr5, tr6, tr7,
                tr8, tr9, tr10, tr11, tr12, tr13,
                tr14, tr15, tr16, tr17, tr18, tr19], L).


% Testes de informacao complementar ou incompleta

% Invariantes:
% - nao colocar conhecimento repetido (filho, pai, irmao, casado, tio, sobrinho, avo, neto, bisavo, bisneto, descendente, primo, ascendente)
% - so podem ter 2 pais, 4 avos, 8 bisavos
% - so se pode ter uma relacao com um individuo, exceto primos e casados
% - data nascimento < data morte
% - uma so naturalidade
% - filho entre nascimento e morte dos pais

tc1 :-
    nao( evolucao( filho(joao, jorge) ) ).

tc2 :-
    nao( evolucao( pai(jorge, carlos) ) ).

tc3 :-
    nao( evolucao( irmao(jorge, carolina) ) ).

tc4 :-
    nao( evolucao( casado(jose, maria) ) ).

tc5 :-
    nao( evolucao( tio(ana, carla) ) ).

tc6 :-
    nao( evolucao( sobrinho(joao, luis) ) ).

tc7 :-
    nao( evolucao( avo(alexandre, jose) ) ).

tc8 :-
    nao( evolucao( neto(joao, margarida) ) ).

tc9 :-
    nao( evolucao( bisavo(ricardo,carla) ) ).

tc10 :-
    nao( evolucao( bisneto(jorge,alexandre) ) ).

tc11 :-
    nao( evolucao( descendente(carlos, alexandre) ) ).

tc12 :-
    nao( evolucao( ascendente(alexandre, carla) ) ).

tc13 :-
    nao( evolucao( descendenteGrau(joao, alexandre, 4) ) ).

tc14 :-
    nao( evolucao( descendenteGrau(carlos, alexandre, 5) ) ).

tc15 :-
    nao( evolucao( ascendenteGrau(alexandre, carlos, 5) ) ).

tc15 :-
    nao( evolucao( descendente(jose,alexandre) ) ).

tc16 :-
    nao( evolucao( ascendente(maria, carlos) ) ).

tc17 :-
    nao( evolucao( primo(carla, carlos) ) ).

tc18 :-
    nao( evolucao( primo(carlos, carla) ) ).

tc19 :-
    nao( evolucao( pai(ficticio, joao) ) ).

tc20 :-
    nao( evolucao( filho(carla, ficticio) ) ).

tc21 :-
    nao( evolucao( avo(avo_ficticio, joao) ) ).

tc22 :-
    nao( evolucao( neto(carlos, avo_ficticio) ) ).

tc23 :-
    nao( evolucao( casado(jorge, mulher_ficticia) ) ).

tc24 :-
    evolucao( primo(p1, p2) ),
    evolucao( casado(p1, p2) ),
    remocao( primo(p1,p2) ),
    remocao( casado(p1,p2) ).

tc25 :-
    evolucao( filho(f, p) ),
    nao( evolucao( casado(f, p) ) ),
    remocao( filho(f,p) ).

tc26 :-
    evolucao(naturalidade(x, Braga, 2015) ),
    nao( evolucao(dataMorte(x, 2014) ) ),
    remocao( naturalidade( x,Braga,2015) ).

tc27 :-
    evolucao(naturalidade(x, Braga, 1980)),
    evolucao(dataMorte(x, 2014) ),
    evolucao(naturalidade(f,Braga,2015)),
    nao( evolucao(filho(f,x)) ),
    remocao(naturalidade(x,Braga,1980)),
    remocao(dataMorte(x,2014)),
    remocao(naturalidade(f,Braga,2015)).

tc28 :-
    evolucao(naturalidade(x, Braga, 1980)),
    evolucao(dataMorte(x, 2014) ),
    evolucao(naturalidade(f,Braga,2015)),
    nao( evolucao(pai(f,x)) ),
    remocao(naturalidade(x,Braga,1980)),
    remocao(dataMorte(x,2014)),
    remocao(naturalidade(f,Braga,2015)).

tc29 :-
    evolucao(naturalidade(x, Braga, 1980)),
    evolucao(dataMorte(x, 2014) ),
    evolucao(filho(f,x)),
    nao(evolucao(naturalidade(f,Braga,2015))),
    remocao(filho(f,x)),
    remocao(naturalidade(x,Braga,1980)),
    remocao(dataMorte(x,2014)).

tc30 :-
    evolucao(naturalidade(x, Braga, 1980)),
    evolucao(dataMorte(x, 2014) ),
    evolucao(pai(f,x)),
    nao(evolucao(naturalidade(f,Braga,2015))),
    remocao(pai(f,x)),
    remocao(naturalidade(x,Braga,1980)),
    remocao(dataMorte(x,2014)).

tc31 :-
    nao( remocao(pai(jorge,joao)) ).

tc32 :-
    evolucao(filho(a,b)),
    remocao(filho(a,b)).

teste_complementar(L) :-
    test_all([tc1, tc2, tc3, tc4, tc5, tc6, tc7,
                tc8, tc9, tc10, tc11, tc12, tc13], L).

testar(L) :-
    teste_predicados(SL1),
    teste_listar(SL2),
    teste_invariantes(SL3),
    teste_relacoes(SL4),
    teste_complementar(SL5),
    L = (predicados: SL1, listar: SL2, invariantes: SL3, relacoes: SL4, complementar: SL5).

t(L) :- testar(L).

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

pai(P,F) :-
    filho(F, P).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado irmao: M,N -> {V,F}

irmao(M,N) :-
    pai(P, M), pai(P, N) , M \== N.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado casado: X,Y -> {V,F}

casado(X,Y) :- clause(casado(Y,X), true).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado avo: Avo,Neto -> {V,F}

avo(A,N) :-
    filho(N, X) , pai(A, X).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado neto: Neto,Avo -> {V,F}

neto(N, A) :-
    avo(A, N).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado tio: Tio,Sobrinho -> {V,F}

tio( T,S ) :-
    pai(P, S), irmao(T, P).

tio( T1,S ) :-
    avo(A, S), filho(T2, A), nao( pai(T2, S) ), casado( T1, T2).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado sobrinho: Sobrinho,Tio -> {V,F}

sobrinho(S, T) :-
    tio(T, S).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado primo: Primo,Primo -> {V,F}

primo(X, Y) :-
    pai(P1, X), pai(P2, Y), irmao(P1, P2).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado bisavo: Bisavo,Bisneto -> {V,F}

bisavo(BA, BN) :-
    avo(A, BN), pai(BA, A).

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado bisneto: Bisneto,Bisavo -> {V,F}

bisneto(BN, BA) :-
    bisavo(BA, BN).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: X,Y -> {V,F}

descendente(X,Y) :-
    filho(X,Y); filho(X,Z) , descendente(Z,Y).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ascendente: X,Y -> {V,F}
ascendente(X,Y) :-
    descendente(Y,X).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendenteGrau: Descendente,Ascendente,Grau -> {V,F}
descendenteGrau(D,A,1) :-
    filho(D, A).
descendenteGrau(D,A,G) :-
    filho(D, F), descendenteGrau(F, A, Z), G is Z + 1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendenteGrau: Ascendente,Descendente,Grau -> {V,F}
ascendenteGrau(A,D,G) :-
    descendenteGrau(D,A,G).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendentesGrau: Ascendente,Grau,Resultado -> {V,F}
descendentesGrau(A,G,R) :-
    solucoes(P, descendenteGrau(P, A, G), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ascendentesGrau: Descendente,Grau,Resultado -> {V,F}
ascendentesGrau(D,G,R) :-
    solucoes(P, ascendenteGrau(P,D,G), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendenteAteGrau: Descendente,Ascendente,Grau -> {V,F}
descendenteAteGrau(D,A,N) :-
    descendenteGrau(D,A,G), G =< N.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendentesAteGrau: Ascendente,Grau,Resultado -> {V,F}

descendentesAteGrau(A,G,R) :-
    solucoes(P, descendenteAteGrau(P,A,G), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ascendenteAteGrau: Ascendente,Descendente,Grau -> {V,F}
ascendenteAteGrau(A,D,N) :-
    descendenteAteGrau(D,A,N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ascendentesAteGrau: Ascendente,Grau,Resultado -> {V,F}
ascendentesAteGrau(D,G,R) :-
    solucoes(P, ascendenteAteGrau(P,D,G), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado grau: X,Y,N -> {V,F}

grau(X,Y,1) :-
    filho(X,Y).
grau(X,Y, N) :-
    filho(X,Z) , grau(Z, Y, G), N is G+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado idade: P,R -> {V,F}

idade( P,R ) :-
	naturalidade( P,N,NSC,MRR ),
	R is MRR - NSC.

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
    solucoes( A,bisavo(A,BN),NL ), removerRepetidos(NL, S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarBisnetos: BA,S -> {V,F}
listarBisnetos( BA,S ) :-
    solucoes( BN,bisneto(BN,BA),NL ), removerRepetidos(NL, S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarPrimos: PR,S -> {V,F}

listarPrimos( P,S ) :-
    solucoes( PR,primo(P,PR),NL ), removerRepetidos( NL,S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarCasados: C,S -> {V,F}

listarCasados( C,S ) :-
    solucoes( C2,casado(C,C2),S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarNascidos: NSC,S -> {V,F}

listarNascidos( NSC,S ) :-
    solucoes( P,naturalidade(P,N,NSC,MRR),S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarMortos: MRR,S -> {V,F}

listarMortos( MRR,S ) :-
    solucoes( P,naturalidade(P,N,NSC,MRR),S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarNaturais: N,S -> {V,F}

listarNaturais( N,S ) :-
    solucoes( P,naturalidade(P,N,NSC,MRR),S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarComIdade: I,S -> {V,F}

listarComIdade( I,S ) :-
	solucoes( P,idade(P,I),S ).



relacao(A, B, filho) :-
    filho(A, B).

relacao(A, B, pai) :-
    pai(A, B).

relacao(A, B, irmao) :-
    irmao(A, B).

relacao(A, B, tio) :-
    tio(A,B).

relacao(A,B, sobrinho) :-
    sobrinho(A,B).

relacao(A, B, casado) :-
    casado(A,B).

relacao(A,B, avo) :-
    avo(A,B).

relacao(A,B, neto) :-
    neto(A,B).

relacao(A,B,bisavo) :-
    bisavo(A,B).

relacao(A,B, bisneto) :-
    bisneto(A,B).

relacao(A,B, primo) :-
    primo(A,B).

relacao(A,B,X) :-
    descendenteGrau(A,B,N),
    X = descendente\ de\ grau\ N.

relacao(A,B,X) :-
    descendenteGrau(B,A,N),
    X = ascendente\ de\ grau\ N.

relacao(A,B,desconhecido).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado selecionaComuns: [A|B],[X|L],Res -> {V,F}

selecionaComuns( [],[C|D],[] ).
selecionaComuns( [A|B],[C|D],Res ) :-
    nao( contem( A,[C|D] ) ),
    selecionaComuns( B,[C|D], Res).
selecionaComuns( [A|B],[C|D],Res ) :-
    selecionaComuns( B,[C|D], R),
    contem( A,[C|D] ),
    Res = [A|R].

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarFilhosDeCasamento: I,S -> {V,F}

listarFilhosDeCasamento( X,Y,S ) :-
    casado( X,Y ),
    listarFilhos( X,S1 ),
    listarFilhos( Y,S2 ),
    selecionaComuns( S1,S2,S ).

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

remocao(T) :-
    solucoes( C, -T::C, S),
    teste(S),
    retract(T).

% nao permitir que individuos tenham relacoes incongruentes

+filho( F,P ) :: relacao(F,P,desconhecido).
+pai( P,F ) :: relacao(P,F,desconhecido).
+irmao( M,N ) :: relacao(M,N,desconhecido).
+avo( A,N ) :: relacao(A,N,desconhecido).
+neto( N,A ) :: relacao(N,A,desconhecido).
+tio( T,S ) :: relacao(T,S,desconhecido).
+sobrinho( S,T ) :: relacao(S,t,desconhecido).
+primo( P1,P2 ) :: relacao(P1,P2,desconhecido).
+bisavo( BA,BN ) :: relacao(BA,BN,desconhecido).
+bisneto( BN,BA ) :: relacao(BN,BA,desconhecido).


% nao permitir a insercao de conhecimento repetido

+filho( F,P ) :: (solucoes( (F,P),(filho( F,P )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+pai( F,P ) :: (solucoes( (P,F),(pai( P,F )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+irmao( M,N ) :: (solucoes( (M,N),(irmao( M,N )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+irmao( M,N ) :: (solucoes( (N,M),(irmao( N,M )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+primo( P1,P2 ) :: (solucoes( (P1,P2),(primo( P1,P2 )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+primo( P1,P2 ) :: (solucoes( (P2,P1),(primo( P2,P1 )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+tio( T,SOB ) :: (solucoes( (T,SOB),(tio( T,SOB )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+sobrinho( SOB,T ) :: (solucoes( (SOB,T),(sobrinho( SOB,T )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+avo( A,N ) :: (solucoes( (A,N),(avo( A,N )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+neto( N,A ) :: (solucoes( (N,A),(neto( N,A )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+bisavo( BA,BN ) :: (solucoes( (BA,BN),(bisavo( BA,BN )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+bisneto( BN,BA ) :: (solucoes( (BN,BA),(bisneto( BN,BA )),S ),
                  comprimento( S,N ),
                  N == 1
                  ).

+casado( X,Y ) :: ( solucoes( Z,casado(X,Z),S ),
                    comprimento( S,N ),
                    N =< 1
                  ).

+casado( X,Y ) :: ( solucoes(Z, casado(Y,Z), S),
                    comprimento( S,N ),
                    N =< 1
                  ).

% nao permitir que um filho tenha mais que dois progenitores

+filho( F,P ) :: ( solucoes( X, (filho(F, X)), S),
                    comprimento(S, N),
                    N < 3
                  ).


% nao permitir ao adicionar pai que este seja pai de alguem do qual e descendente
+pai( P,F ) ::  ( solucoes( (P,F), (pai( P,F )), S),
                    nao( descendente(P,F) )
                ).

% nao permitir adicionar pai de um filho que ja tenha dois progenitores
+pai( P,F ) ::  ( solucoes( X, (filho(F, X)), S),
                    comprimento(S, N),
                    N < 3
                ).

% nao permitir adicionar avo de um neto que ja tenha quatro avos
+avo( A,N ) ::  ( solucoes( X, (neto(N, X)), S),
                    comprimento(S, N),
                    N < 5
                ).

% nao permitir adicionar bisavo de um bisneto que ja tenha oito bisavos
+bisavo( A,N ) ::  ( solucoes( X, (bisneto(N, X)), S),
                    comprimento(S, N),
                    N < 9
                ).

% nao e possivel ter nascimento depois de morte
+naturalidade( P,N,NSC,MRR ) :: ( solucoes( (P,N,NSC,MRR),naturalidade( P,N,NSC,MRR ), S ),
                                  NSC < MRR
                                ).

% nao e possivel alguem ter mais que uma naturalidade
+naturalidade( P,N,NSC,MRR ) :: ( solucoes( (NX,NSCX,MRRX),naturalidade( P,NX,NSCX,MRRX ), S ),
                                  comprimento(S,NL),
                                  NL < 2
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
