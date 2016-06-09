% Caso base: una hoja siempre está bien etiquetada (no tiene aristas)
bienEtiquetado_aux(_,_,_,_,[]).
% Caso base: Si un nodo no tiene subarboles, el tamaño de sus subarboles es 0
tamArbol([],0).

bienEtiquetado(nodo(Et_Nodo,Aristas)) :-
	tamArbol(nodo(Et_Nodo,Aristas),N),
	% Primero chequeo que la etiqueta del nodo raíz no sea mayor que N
	Et_Nodo =< N, 
	% Voy llevando el N, la etiqueta del nodo padre, y dos listas que representan
	% el conjunto de etiquetas de nodos y aristas ya utilizados
	bienEtiquetado_aux(N,[],[],Et_Nodo,Aristas). 

% Un árbol está bien etiquetado si para toda arista se cumple la condición de buen etiquetamiento
bienEtiquetado_aux(N,Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, [Arista | L_Aristas]) :-
	bienEtiquetado_aux(N,Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, L_Aristas),
	bienEtiquetado_aux(N,Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, Arista),

% Una arista cumple con la condición de buen etiquetamiento si la etiqueta de la arista es igual
% al valor absoluto de la diferencia entre la etiqueta del nodo padre y del nodo hijo, y si el
% subarbol asociado también está bien etiquetado
bienEtiquetado_aux(N,Set_Et_Nodos, Set_Et_Aristas, Et_Nodo1, arista(Et_Arista,nodo(Et_Nodo2,Aristas2))) :-
	not(member(Et_Nodo2,Set_Et_Nodos)), % La etiqueta del nodo no puede haber sido utilizada anteriormente
	not(member(Et_Arista,Set_Et_Aristas)), % La etiqueta de la arista no puede haber sido utilizada anteriormente
	% Las etiquetas de nodo van de 1 ... N
	Et_Nodo >= 1,
	Et_Nodo <= N,
	% Las etiquetas de arista van de 1 ... N-1
	Et_Arista >= 1,
	Et_Arista < N,
	% Chequeamos que se cumpla la condición de buen etiquetamiento
	Dif is Et_Nodo1-Et_Nodo2,
	abs(Dif, Dif_Abs),
	Dif_Abs == Et_Arista,
	% Chequeamos que el subarbol esté bien etiquetado
	bienEtiquetado_aux(N,[Et_Nodo2 | Set_Et_Nodos], [Et_Arista | Set_Et_Aristas], Et_Nodo2, Aristas2).

% El tamaño de un arbol es 1 + el tamaño de sus subarboles hijos
tamArbol(nodo(_, Aristas),N) :- tamArbol(Aristas,_N), N is _N+1.

% El tamaño de una lista de subarboles hijos es el tamaño del primer subarbol más
% el tamaño del resto de los subarboles en la lista.
tamArbol([ arista(_,Sub_Arbol) | Aristas2],N) :-
	tamArbol(Sub_Arbol,N1),
	tamArbol(Aristas2,N2), N is N1+N2.
