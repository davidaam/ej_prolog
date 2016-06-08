miembro(X, [X | _ ]).
miembro(X, [Y | Z]) :- miembro(X,Z).

concat([], R, R).
concat([A | B], R, [A | Z]) :- concat(B, R, Z).

eliminar(X, [], []).
eliminar(X, [X | Z], R) :- eliminar(X,Z,R).
eliminar(X, [Y | Z], [Y | R]) :- eliminar(X,Z,R).

invertir(L,R) :- invertir_aux(L,[],R).
invertir_aux([],ACC,ACC).
invertir_aux([X | Xs],ACC,R) :- invertir_aux(Xs, [X | ACC], R).

