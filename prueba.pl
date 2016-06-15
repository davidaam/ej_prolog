rep(N,L) :- N =:= 1, L = [1], !.
rep(N,[C | L]) :- between(1,N,C), Nsig is N-1, rep(Nsig, L).
