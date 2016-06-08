bienEtiquetado(nodo(R,E)) :- bienEtiquetado_aux(R,E).
bienEtiquetado_aux(_,[]).
bienEtiquetado_aux(R,[A | E]) :- bienEtiquetado_aux(R,A), bienEtiquetado_aux(R,E).
bienEtiquetado_aux(R,arista(RA,nodo(R1,X))) :- RR is R-R1, RABS is abs(RR), RA == RABS , bienEtiquetado(nodo(R1,X)).