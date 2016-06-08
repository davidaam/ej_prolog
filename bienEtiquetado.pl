bienEtiquetado(nodo(Et_Nodo,Aristas)) :- bienEtiquetado_aux([],[],Et_Nodo,Aristas).

bienEtiquetado_aux(_,_,_,[]).

bienEtiquetado_aux(Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, [Arista | L_Aristas]) :-
	bienEtiquetado_aux(Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, Arista),
	bienEtiquetado_aux(Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, L_Aristas).

bienEtiquetado_aux(Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, arista(Et_Arista,nodo(Et_Nodo2,Aristas2))) :-
	Dif is Et_Nodo1-Et_Nodo2,
	abs(Dif, Dif_Abs),
	Dif_Abs == Et_Arista,
	bienEtiquetado_aux(Et_Nodo2,Aristas2).