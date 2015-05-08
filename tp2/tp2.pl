%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% TRABALHO PRÁTICO: EXERCÍCIO 02    2014/15

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic matricula/2.
:- dynamic marca/2.
:- dynamic excecao/1.
:- dynamic nulo/1.
:- dynamic '-'/1.

%conhecimento perfeito positivo
marca( "AA-AA-00",subaru ).

%conhecimento perfeito negativo
-marca( "AA-AA-01",bmw ).

%conhecimento desconhecido
marca( "AA-AA-02",mdesconhecida01 ).
excecao( marca( MTR,MRC ) ) :-
  marca( MTR,mdesconhecida01 ).

%conhecimento impreciso
excecao( marca( "AA-AA-03",subaru ) ).
excecao( marca( "AA-AA-03",suzuki ) ).

%conhecimento interdito
marca( "AA-AA-04",mproibida01 ).
excecao( marca( MTR,MRC ) ) :-
  marca( MTR,mproibida01 ).
nulo( mproibida01 ).

excecao( marca( "AA-AA-05",subaru ) ).

%tem de ser feito de uma forma geral

+marca( MTR,MRC ) :: ( solucoes( (MTR,Md),( marca( "AA-AA-04",Md ),nao( nulo(Md) ) ),S ),
                       comprimento( S,N ), N == 0
                     ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado marca: MTR,MRC -> {V,F,D}

-marca( MTR,MRC ) :-
    nao( marca( MTR,MRC ) ),
    nao( excecao( marca( MTR,MRC ) ) ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F, Desconhecido}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao, falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).


demoCada([], []).
demoCada([Q|T], R) :-
    demo(Q, X),
    demoCada(T, B),
    R = [X|B]. 


demoTodos([], verdadeiro).

demoTodos([Q|T], verdadeiro) :- 
    demo(Q, verdadeiro),
    demoTodos(T, verdadeiro).

demoTodos([Q|T], desconhecido) :-
    demo(Q, desconhecido),
    demoTodos(T, verdadeiro).

demoTodos([Q|T], desconhecido) :-
    nao( demo(Q, falso)),
    demoTodos(T, desconhecido).

demoTodos([Q|T], falso) :- 
    demo(Q, falso).

demoTodos([Q|T], falso) :-
    demoTodos(T, falso).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento: Termo -> {V,F}

evolucaoMais( [] ).
evolucaoMais( [X|L] ) :-
	evolucao( X ),
	evolucaoMais( L ).

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado teste: [R|LR] -> {V,F}

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado solucoes: A,T,S -> {V, F}

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: [X | L], N -> {V,F}

comprimento( [],0 ).

comprimento( [X|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.


% ----------------- INVARIANTES
%impossível adicionar conhecimento repetido
+marca( MTR,MRC ) :: (solucoes( MRC,(marca(MTR,MRC)),S),
                        comprimento(S,N),
                        N == 1
                     ).

+(-T) :: (solucoes( T,(-T),S),
            		comprimento(S,N),
            		N == 1
   	 	 ).

%CONHECIMENTO PERFEITO POSITIVO
%Não permitir adicionar quando se tem o conhecimento perfeito negativo oposto
+marca( MTR,MRC ) :: nao( -marca(MTR,MRC) ).

%->Depois de adicionado não se pode adicionar nada, e tem de se remover o conhecimento imperfeito

%CONHECIMENTO PERFEITO NEGATIVO
%Não permitir adicionar se houver o conhecimento positivo perfeito oposto.
+(-marca( MTR,MRC )) :: nao( marca(MTR,MRC) ).

%CONHECIMENTO DESCONHECIDO
%Não se pode adicionar se houver conhecimento perfeito positivo.


%CONHECIMENTO IMPERFEITO
%apenas deixar adicionar conhecimento positivo se este. Remover o conhecimento impreciso.
+marca( MTR,MRC) :: (solucoes( B,excecao(marca(MTR,B)),S),
                        verificaSePertence(S,MRC),
                        solucoes(excecao(marca(MTR,B)),excecao(marca(MTR,B)),S),
               			removeImprecisao(MTR)
                    ).

verificaSePertence([], MRC).
verificaSePertence( S,MRC ) :-
	verificaSePertenceAux( S,MRC ).

verificaSePertenceAux( [MRC],MRC ).
verificaSePertenceAux( [MRC|L],MRC ).
verificaSePertenceAux( [X|L],MRC ) :-
	verificaSePertence( L, MRC).

removeImprecisao([]) :-
	.


%impossível adicionar excecoes a conhecimento perfeito positivo
+excecao( marca(MTR,MRC) ) :: nao( marca(MTR,MRC) ).

%CONHECIMENTO NULO
%impossível adicionar conhecimento se tivermos conhecimento proibido associado a essa matricula
+marca( MTR,MRC) :: (solucoes( B,marca(MTR,B),S),
                        semNulos(S)
                    ).

semNulos( [] ).
semNulos( [X|L] ) :-
	nao( nulo( X ) ),
	semNulos( L ).



% [y] Não permitir conhecimento repetido
% [y] Não permitir adicionar se tivermos conhecimento proibido
% [y] Evolucao Perfeita
% [] Evolucao Imprecisa
% [] Evolucao Desconhecida
% [] Evolucao Proibida
% [] Não permitir adicionar nada quando temos conhecimento perfeito
% [] Permitir adicionar quando já temos conhecimento desconhecido (Como se faz?)
% [] Permitir adicionar quando já temos conhecimento impreciso (Apagar as excecoes?)