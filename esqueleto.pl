repeat(_,K,[]) :- K =< 0, !.
repeat(Num,Times,[Num | L]) :- Times_ is Times-1, repeat(Num,Times_,L), !.


esqueleto(1,_,[0]) :- !.

esqueleto(N,R,esq([[HijosRaiz] | Lista])) :-
	MaxHijos is min(N,R),
	between(1,MaxHijos,HijosRaiz),
	Restantes is N-1,
	esqueleto(Restantes,MaxHijos,HijosRaiz,Lista).

esqueleto(N,_,N,[L]) :- repeat(0,N,L), !.
esqueleto(N,R,HijosAnterior,[L1 | Resto]) :-
	constrLista(N,R,HijosAnterior,L1),
	sumlist(L1,TamSig),
	NSig is N-HijosAnterior,
	RSig is min(NSig,R),
	esqueleto(NSig, RSig, TamSig, Resto).


constrLista(N,R,TamLista,L) :-
	Max is min(N,R),
	constrLista(N,Max,1,N,TamLista,L).

constrLista(_,_,_,Restantes,0,[]) :- !.
constrLista(_,_,_,0,Tam,L) :- repeat(0,Tam,L), !.

constrLista(N,R,Min,Restantes,Tam,[Cabeza | L]) :-
	Max is min(R,Restantes),
	between(Min,Max,I),
	Cabeza is Max - I + Min,
	RestantesSig is Restantes - Cabeza,
	TamSig is Tam - 1,
	RSig is min(Max,Cabeza),
	constrLista(N, RSig, 0, RestantesSig, TamSig, L).

slice([],_,_,[]) :- !.
slice([Cabeza | Resto], 0, 0, []) :- !.
slice([Cabeza | Resto], 0, Tam, [Cabeza | L]):- T is Tam - 1, slice(Resto,0,T,L). 
slice([Cabeza | Resto], Inicio, Tam, L) :- I is Inicio-1, T is Tam-1,slice(Resto, I, T, L).

etiquetamiento([],_,_,[]).
etiquetamiento([[] | _],_,_,[]).
etiquetamiento([[] | Resto],Inicio,Tam,[]).
etiquetamiento([[0 | Cola] | Resto],Inicio,Tam,[arista(1,nodo(1,[])) | LAristas]) :- ISig is Inicio + 1, etiquetamiento([Cola | Resto],ISig,Tam,LAristas).
etiquetamiento([[Cab] | Lista], nodo(1,L)) :- etiquetamiento(Lista, 0, Cab, L).
etiquetamiento([[Cabeza | [CabSig | Cola]] | [LAristas | Resto]], Inicio, Tam, [arista(1,nodo(1,Arista)) | LAristas]) :-
	slice(LAristas, Inicio, Cabeza, MisAristas),
	etiquetamiento([MisAristas | Resto],Inicio, Cabeza, Arista),
	ISig is Inicio + Cabeza,
	etiquetamiento([Cola | [LAristas | Resto]],ISig,CabSig,LAristas).

describirEtiquetamiento(nodo(E,Aristas)) :- 
	write(0), write(": "), write(E), nl,
	describirEtiquetamiento(Aristas,[0],0).	

describirEtiquetamiento(nodo(E, Aristas), Padre, EtiquetaA) :-
	join_lista(Padre, ".", "", Padre_Str), write(Padre_Str), write(": "), write(E), write(", "), write(EtiquetaA), nl,
	describirEtiquetamiento(Aristas,Padre,0).

describirEtiquetamiento([arista(EtiquetaA, Nodo) | RestoAristas],Padre,N) :-
	append(Padre,[N],X),
	describirEtiquetamiento(Nodo, X, EtiquetaA),
	NSig is N+1,
	describirEtiquetamiento(RestoAristas,Padre,NSig).

describirEtiquetamiento([],_,_) :- !.

join_lista([Cab | Cola], Separador, Acc, Str) :-
	number_string(Cab,Str_Cab),
	string_concat(Str_Cab,Separador,X),
	string_concat(Acc,X,AccSig),
	join_lista(Cola, Separador, AccSig, Str).

join_lista([X | []], Separador, Acc, Str) :- number_string(X,Str_X), string_concat(Acc,Str_X,Str).

