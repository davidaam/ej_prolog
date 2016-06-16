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
slice([Cabeza | Resto], Inicio, Tam, L) :- I is Inicio-1, T is Tam-1, slice(Resto, I, T, L).

head([X | _],X).

etiquetamiento([[]],_,_,[]).
etiquetamiento([[Cab] | Lista], Arbol) :- etiquetamiento(Lista, 0, Cab, L), etiquetar(nodo(EtNodo,L),Arbol).

genAristasHojas(0,[]).
genAristasHojas(Tam,[arista(X,nodo(Y,[])) | Resto]) :- T is Tam - 1, genAristasHojas(T, Resto).
etiquetamiento([[0 | Resto]],Inicio,Tam,Aristas) :- genAristasHojas(Tam,Aristas), !.

etiquetamiento([[Cabeza | Cola] | [LAristas | Resto]], Inicio, Tam, [arista(EtArista,nodo(EtNodo,Arista)) | RestoAristas]) :-
	ISig is Inicio + Cabeza,
	slice(LAristas, Inicio, ISig, MisAristas),
	etiquetamiento([MisAristas | Resto],Inicio, Cabeza, Arista),
	((head(Cola,CabSig),
		etiquetamiento([Cola | [LAristas | Resto]],ISig,CabSig,RestoAristas)
	); RestoAristas = [], !).

% Caso base: Si un nodo no tiene subarboles, el tamaño de sus subarboles es 0
tamArbol([],0).

% El tamaño de un arbol es 1 + el tamaño de sus subarboles hijos
tamArbol(nodo(_, Aristas),N) :- tamArbol(Aristas,_N), N is _N+1.

% El tamaño de una lista de subarboles hijos es el tamaño del primer subarbol más
% el tamaño del resto de los subarboles en la lista.
tamArbol([ arista(_,Sub_Arbol) | Aristas2],N) :-
	tamArbol(Sub_Arbol,N1),
	tamArbol(Aristas2,N2), N is N1+N2.
	
generarL(0, []) :- !.
generarL(N, [N | Lista]):- Nsig is N-1, generarL(Nsig,Lista).
	
etiquetar(Arbol, ArbolN):-
    tamArbol(Arbol, N), 
    generarL(N, ListaN),
    delete(ListaN, N,ListaA),
    etiquetar(Arbol, ListaN,ListaA, ArbolN).

etiquetar([],_,ListaN,ListaA,ListaN,ListaA,[]).

etiquetar(nodo(EtNodo, [arista(EtArista,nodo(EtNodo2,Aristas2)) | Aristas]), ListaN,ListaA, nodo(EtNodo, [arista(EtArista,nodo(EtNodo2,Aristas2)) | Aristas])) :-
    tamArbol(nodo(EtNodo, [arista(EtArista,nodo(EtNodo2,Aristas2)) | Aristas]), N),
    between(1,N,EtNodo),
    ESig is EtNodo + 1,
    between(ESig,N,EtNodo2),
    EtArista is abs(EtNodo-EtNodo2),
    (
    	(
	    	member(EtArista,ListaA),
		    subtract(ListaN,[EtNodo,EtNodo2],ListaNSig),
		    delete(ListaA,EtArista,ListaASig),
		    etiquetar(Aristas2, EtNodo2, ListaNSig, ListaASig, ListaNR, ListaAR, Aristas2),
		    etiquetar(Aristas, EtNodo, ListaNR, ListaAR, ListaADef, ListaNDef, Aristas)
		); (fail, !)
	).
    
etiquetar([arista(EtArista,nodo(EtNodo,Aristas)) | Resto], EtPadre, ListaN, ListaA, ListaNDef, ListaADef, [arista(EtArista,nodo(EtNodo,Aristas)) | Resto]) :-
    length(ListaN,TamLN),
    between(1,TamLN,I),
    nth1(I,ListaN,EtNodo),
    EtArista is abs(EtPadre-EtNodo),
    (
    	(
	    	member(EtArista,ListaA),
		    delete(ListaN,EtNodo,ListaNSig),
		    delete(ListaA,EtArista,ListaASig),
		    etiquetar(Aristas, EtNodo, ListaNSig, ListaASig, ListaNR, ListaAR, Aristas),
		    etiquetar(Resto, EtPadre, ListaNR, ListaAR, ListaNDef, ListaADef, Resto)
		); (fail, !)
	).


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

