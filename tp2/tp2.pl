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



demoConj([], verdadeiro).

demoConj([Q|T], verdadeiro) :- 
    demo(Q, verdadeiro),
    demoConj(T, verdadeiro).

demoConj([Q|T], desconhecido) :-
    demo(Q, desconhecido),
    nao( demoConj(T, falso)).

demoConj([Q|T], desconhecido) :-
    nao( demo(Q, falso)),
    demoConj(T, desconhecido).

demoConj([Q|T], falso) :- 
    demo(Q, falso).

demoConj([Q|T], falso) :-
    demoConj(T, falso).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento: Termo -> {V,F}
evolucaoDesconhecido( marca(MTR,MRC) ) :-
	evolucao( marca(MTR,MRC) ),
	assert( (excecao( marca( MTRVAR,MRCVAR ) ) :-
  					marca( MTRVAR,MRC ))
		   ).

evolucaoInterdito( marca(MTR,MRC) ) :-
	evolucao( marca(MTR,MRC) ),
	assert( (excecao( marca( MTRVAR,MRCVAR ) ) :-
  					marca( MTRVAR,MRC ))
		  ),
	evolucao( nulo(MRC) ).

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

remove( Termo ) :-
	retract( Termo ).

removeTermos( [] ).
removeTermos( [X] ) :-
	retract(X).
removeTermos( [X|L] ) :-
	retract(X),
	removeTermos( L ).

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

%CONHECIMENTO PERFEITO POSITIVO
%Não permitir adicionar quando se tem o conhecimento perfeito negativo oposto
+marca( MTR,MRC ) :: nao( -marca(MTR,MRC) ).

%Não permitir adicionar quando já se tem o conhecimento perfeito positivo
+marca( MTR,MRC ) :: (	solucoes( B,(marca(MTR,B)),S),
            			comprimento(S,N),
            			N == 1
            		 ).

%->Depois de adicionado não se pode adicionar nada, e tem de se remover o conhecimento imperfeito




%CONHECIMENTO PERFEITO NEGATIVO
%Não permitir adicionar se houver o conhecimento positivo perfeito oposto.
+(-marca( MTR,MRC )) :: nao( marca(MTR,MRC) ).

%Não permitir adicionar conhecimento negativo repetido
+(-T) :: (solucoes( T,(-T),S),
            		comprimento(S,N),
            		N == 1
   	 	 ).




%CONHECIMENTO DESCONHECIDO
%deixar adicionar conhecimento positivo e remover o conhecimento desconhecido.
+marca( MTR,MRC) :: (solucoes( marca(MTR,B),marca(MTR,B),S),
                        seTemDesconhecidoRemove(S)
                    ).

seTemDesconhecidoRemove( [] ).
seTemDesconhecidoRemove( [ marca(MTR,MRC)] ) :-
	demo(marca(MTR,MRC),desconhecido),
	removeTermos( [marca(MTR,MRC),(excecao(marca(MTRVAR,MRCVAR)):-marca(MTRVAR,MRC))] ).

seTemDesconhecidoRemove( [X|L] ) :-
	demo(marca(MTR,MRC),desconhecido),
	removeTermos( [marca(MTR,MRC),(excecao(marca(MTRVAR,MRCVAR)):-marca(MTRVAR,MRC))] ).
	
seTemDesconhecidoRemove( [X|L] ) :-
	seTemDesconhecidoRemove( L ).



%CONHECIMENTO IMPERFEITO
%apenas deixar adicionar conhecimento positivo se este pertence ao conjunto de conhecimento impreciso e remover o conhecimento impreciso.
+marca( MTR,MRC) :: (solucoes( B,excecao(marca(MTR,B)),S),
                        verificaSePertence(S,MRC),
                        solucoes(excecao(marca(MTR,B)),excecao(marca(MTR,B)),S2),
               			removeTermos(S2)
                    ).

verificaSePertence([], MRC).
verificaSePertence( S,MRC ) :-
	verificaSePertenceAux( S,MRC ).

verificaSePertenceAux( [MRC],MRC ).
verificaSePertenceAux( [MRC|L],MRC ).
verificaSePertenceAux( [X|L],MRC ) :-
	verificaSePertence( L, MRC).


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



% INVARIANTES
% [y] impossível adicionar conhecimento repetido
% [y] impossível adicionar conhecimento perfeito positivo quando se tem o conhecimento perfeito negativo oposto ou conhecimento positivo
% [y] impossível adicionar conhecimento perfeito negativo quando se tem o conhecimento perfeito positivo oposto
% [y] impossível adicionar excecoes a conhecimento perfeito positivo
% [y] deixar adicionar conhecimento positivo se tivermos conhecimento impreciso, removendo o conhecimento impreciso.
% [y] impossível adicionar conhecimento se tivermos conhecimento proibido associado a essa matricula

% EVOLUCOES
% [y] evolucao Perfeita
% [] Evolucao Imprecisa
% [] Evolucao Desconhecida
% [] Evolucao Proibida
% [] Não permitir adicionar nada quando temos conhecimento perfeito
% [] Permitir adicionar quando já temos conhecimento desconhecido (Como se faz?)
% [] Permitir adicionar quando já temos conhecimento impreciso (Apagar as excecoes?)