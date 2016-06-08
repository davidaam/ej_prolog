bienEtiquetado(nodo(Et_Nodo,Aristas)) :-
	tamArbol(nodo(Et_Nodo,Aristas),N),
	Et_Nodo =< N,
	bienEtiquetado_aux(N,[],[],Et_Nodo,Aristas).

bienEtiquetado_aux(_,_,_,_,[]).

bienEtiquetado_aux(N,Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, [Arista | L_Aristas]) :-
	bienEtiquetado_aux(N,Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, L_Aristas).
	bienEtiquetado_aux(N,Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, Arista),

bienEtiquetado_aux(N,Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, arista(Et_Arista,nodo(Et_Nodo2,Aristas2))) :-
	not(member(Et_Nodo2,Set_Et_Nodos)),
	not(member(Et_Arista,Set_Et_Aristas)),
	Et_Nodo <= N,
	Et_Arista < N,
	Dif is Et_Nodo1-Et_Nodo2,
	abs(Dif, Dif_Abs),
	Dif_Abs == Et_Arista,
	bienEtiquetado_aux(N,[Et_Nodo2 | Set_Et_Nodos], [Et_Arista | Set_Et_Aristas], Et_Nodo2, Aristas2).

tamArbol(nodo(_, Aristas),N) :- tamArbol(Aristas,_N), N is _N+1.
tamArbol([],0).
tamArbol([ arista(_,Sub_Arbol) | Aristas2],N) :-
	tamArbol(Sub_Arbol,N1),
	tamArbol(Aristas2,N2), N is N1+N2.