palindrome([]).
palindrome([_]).

palindrome(X) :- reverse(X,R), R == X.

esqueleto(N, R, X) :- n(M), M =< N-1, N_ is N-M-1, esqueleto_aux(N_,R,1,L), X is [ M | L ].

esqueleto_aux(N,R,H,X) :- n(M), M =< N, N_ is N-M, esqueleto_aux(N_,R,N,L), X is [ M | L ].
esqueleto_aux(0,R,H,X) :- repeat(0,H,X).

repeat(_,K,[]) :- K =< 0, !.
repeat(Num,Times,[Num | L]) :- Times_ is Times-1, repeat(Num,Times_,L), !.

min(A,B,X) :- B =< A, X = B, !.
min(A,B,X) :- A =< B, X = A, !.

comp_debil(K,0,X) :- repeat(0,K,X), !.
comp_debil(0,_,[]).
comp_debil(1,X,[X]).
comp_debil(K,N,X) :- between(0,N,I), Max is N-I, Krec is K-1, (((K < 0; I > Krec*Max), !, fail); 1 = 1), comp_debil(Krec,I,Max,Comp_resto), X = [Max | Comp_resto].

comp_debil(K,0,_,X) :- repeat(0,K,X), !.
comp_debil(0,_,_,[]).
comp_debil(1,X,_,[X]).
comp_debil(K,N,Max_Ant,X) :- between(0,N,I), C is N-I, min(C,Max_Ant,M), Max is M-I, Krec is K-1, (((K < 0; I > Krec*Max), !, fail); 1 = 1), comp_debil(Krec,I,Max,Comp_resto), X = [Max | Comp_resto].
