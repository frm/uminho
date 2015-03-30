

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


% naturalidades
naturalidade(ricardo,porto,1872,1922).

naturalidade(jose,porto,1910,1982).

naturalidade(margarida,aveiro,1910,2003).

naturalidade(maria,braga,1933,2003).

naturalidade(ana,braga,1960,2010).

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
    evolucao( primo(p1, p2) ),
    evolucao( casado(p1, p2) ),
    nao( evolucao(tio(p1,p2)) ),
    remocao( primo(p1,p2) ),
    remocao( casado(p1,p2) ).

tc27 :-
    nao( evolucao(naturalidade(x, Braga, 2015, 2014) ) ).

tc28 :-
    evolucao(pai(abc,def)),
    nao( evolucao(filho(def,abc)) ),
    remocao( pai(abc,def) ).

tc29 :-
    evolucao( avo(av,nt) ),
    nao( evolucao( tio(nt,av) ) ),
    remocao( avo(av,nt) ).

tc30 :-
    evolucao( pai(abc,def) ),
    nao( evolucao(casado(abc,def)) ),
    remocao( pai(abc,def) ).

tc31 :-
    evolucao( primo(abc,def) ),
    evolucao( casado(def,abc) ),
    remocao( primo(abc,def) ),
    remocao( casado(def,abc) ).

tc32 :-
    evolucao( primo(abc,def) ),
    evolucao( casado(abc,def) ),
    nao( evolucao(bisavo(def,abc)) ),
    remocao( primo(abc,def) ),
    remocao( casado(abc,def) ).

tc33 :-
    nao( evolucao( filho(f,f) ) ).

tc34 :-
    nao( evolucao( pai(f,f) ) ).

tc35 :-
    nao( evolucao( irmao(f,f) ) ).

tc36 :-
    nao( evolucao( tio(f,f) ) ).

tc37 :-
    nao( evolucao( sobrinho(f,f) ) ).

tc38 :-
    nao( evolucao( avo(f,f) ) ).

tc39 :-
    nao( evolucao( neto(f,f) ) ).

tc40 :-
    nao( evolucao( bisneto(f,f) ) ).

tc41 :-
    nao( evolucao( bisavo(f,f) ) ).

tc42 :-
    nao( evolucao( casado(f,f) ) ).

tc43 :-
    nao( evolucao( primo(f,f) ) ).

teste_complementar(L) :-
    test_all([tc1, tc2, tc3, tc4, tc5, tc6, tc7,
                tc8, tc9, tc10, tc11, tc12, tc13,
                tc14, tc15, tc16, tc17, tc18, tc19,
                tc20, tc21, tc22, tc23, tc24, tc25,
                tc26, tc27, tc28, tc29, tc30, tc31,
                tc32, tc33, tc34, tc35, tc36, tc37,
                tc38, tc39, tc40, tc41, tc42, tc43],
                L).

testar(L) :-
    teste_predicados(SL1),
    teste_listar(SL2),
    teste_invariantes(SL3),
    teste_relacoes(SL4),
    teste_complementar(SL5),
    L = (predicados: SL1, listar: SL2, invariantes: SL3, relacoes: SL4, complementar: SL5).

t(L) :- testar(L).

% funcoes auxiliares de teste

test_all([], []).
test_all([H|T], L) :-
    H, test_all(T, L).

test_all([H|T], L) :-
    test_all(T, NL), L = [H|NL].

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