palindrome([]).
palindrome([_]).

palindrome(X) :- reverse(X,R), R == X.

n(0).
n(N) :- n(M), N is M+1.

esqueleto(N, R, X) :- n(M), M =< N-1, N_ is N-M-1, esqueleto_aux(N_,R,1,L), X is [ M | L ].

esqueleto_aux(N,R,H,X) :- n(M), M =< N, N_ is N-M, esqueleto_aux(N_,R,N,L), X is [ M | L ].
esqueleto_aux(0,R,H,X) :- repeat(0,H,X).

repeat(_,0,[]).
repeat(Num,Times,[Num | L]) :- Times_ is Times-1, repeat(Num,Times_,L).

loop(TIMES) :- n(I), I =< TIMES,
write(I), nl,
I = TIMES, !, fail.
loop(TIMES) :- write('The End'). 
chao.
comp_fuertes(0,_,[]).
comp_fuertes(1,X,[X]).
comp_fuertes(N,N,X) :- repeat(1,N,X), !. 
comp_fuertes(K,N,X) :-
	n(I),
	NK is N-K,
	I =< NK,
	C is NK+1-I,
	R is N-C,
	KR is K-1,
	KR >= 1,
	comp_fuertes(KR,R,Comp_resto), X = [C | Comp_resto], fail.