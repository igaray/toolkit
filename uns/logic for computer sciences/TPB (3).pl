/*5 */
/* a) cera una lista con dos elementos */
crearLista(E1,E2,[E1,E2]).

/* b1) inserta un elemento E al comienzo de una lista. */
inserta_comienzo(Elem,[],[Elem]).
inserta_comienzo(Elem,[X|Xs],[Elem|L]):- inserta_comienzo(X,Xs,L).

/* b2) inserta un elemento E al final de una lista */
inserta_final(Elem,[],[Elem]).
inserta_final(Elem,[X|Xs],[X|L]):- inserta_final(Elem,Xs,L).

/* inserta un elemento en la segunda posición de una lista. */
inserta_segunda(Elem,[],[Elem]).
inserta_segunda(Elem,[X|Xs],[X|L]):- inserta_comienzo(Elem,Xs,L).


/* c) concatena dos listas */
concatena([],L2,L2).
concatena([L1|L1s],L2,[L1|L]):- concatena(L1s,L2,L).

/* d) busca un elemento en una lista */
busca(Elem,[Elem|Xs]).
busca(Elem,[X|Xs]):- busca(Elem,Xs).

/* e) invierte una lista */
invierte([],[]).
invierte([X|Xs],L):- invierte(Xs,Aux), inserta_final(X,Aux,L).

/* f1) borra una aparición de un elemento en una lista */
borra_una(Elem,[],[]).
borra_una(Elem,[Elem|Xs],Xs).
borra_una(Elem,[X|Xs],[X|L]):- Elem \= X, borra_una(Elem,Xs,L).

/* f2) borra todas las apariciones de un elemento en una lista. */
borra_todas(Elem,[],[]).
borra_todas(Elem,[Elem|Xs],L):- borra_todas(Elem,Xs,L).
borra_todas(Elem,[X|Xs],[X|L]):- Elem \= X, borra_todas(Elem,Xs,L).

/* g1) cambiar una sola aparición de un cierto elemento por otro */
cambia_una(Nuevo,Viejo,[],[]).
cambia_una(Nuevo,Viejo,[Viejo|Xs],[Nuevo|Xs]).
cambia_una(Nuevo,Viejo,[X|Xs],[X|L]):- Viejo \= X, cambia_una(Nuevo,Viejo,Xs,L).

/* g2) cambiar todas las apariciones de un cierto elemento por otro */
cambia_todas(Nuevo,Viejo,[],[]).
cambia_todas(Nuevo,Viejo,[Viejo|Xs],[Nuevo|L]):- cambia_todas(Nuevo,Viejo,Xs,L).
cambia_todas(Nuevo,Viejo,[X|Xs],[X|L]):- Viejo \= X, cambia_todas(Nuevo,Viejo,Xs,L).

/* genera una lista palindroma de longitud 2*N a partir de una lista de longitud N */
genera_palindroma([],[]).
genera_palindroma([X|Xs],[X|Z]):- genera_palindroma(Xs,Aux), inserta_final(X,Aux,Z).

/* desplaza una posición a la derecha a todos los elementos de una lista */
desplaza_derecha([],[]).
desplaza_derecha(L,L2):- quitar_ultimo(L,Auxiliar,Ultimo), inserta_comienzo(Ultimo,Auxiliar,L2).

/* predicado auxiliar: obtiene el último elemento de una lista y lo elimina de la lista. */
quitar_ultimo([X],[],X).
quitar_ultimo([X|Xs],[X|L],E):- quitar_ultimo(Xs,L,E).

/* desplaza una posición a la izquierda a todos los elementos de una lista. */
desplaza_izquierda([],[]).
desplaza_izquierda(L,Z):- quitar_primero(L,Auxiliar,Primero), inserta_final(Primero,Auxiliar,Z).

/* predicado auxiliar: obtiene el primer elemento de una lista y lo elimina de la lista. */
quitar_primero([X|Xs],Xs,X).

/* 6) ejercicios sobre conjuntos (listas de elementos) */
 /*a) determinar si un elemento es miembro de un conjunto */
miembro(X,[X|Xs]).
miembro(X,[C|Xs]):- X\=C, miembro(X,Xs).
 /*b) inserta un elemento en un conjunto */
inserta(X,[],[X]).
inserta(X,C,C):- miembro(X,C).
inserta(X,C,[X|C]):- not miembro(X,C).
 /*c) une dos conjuntos (el 'inserta' verifica que el elemento ya pertenezca al conjunto union */
unir([],C2,C2).
unir([X|Xs],C2,[X|C3]):- unir(Xs,C2,C3), not miembro(X,C2).
unir([X|Xs],C2,C3):- unir(Xs,C2,C3), miembro(X,C2).
 /* intersecta dos conjuntos */
intersecta([],C2,[]).
intersecta([X|Xs],C2,[X|C3]):- intersecta(Xs,C2,C3), miembro(X,C2).
intersecta([X|Xs],C2,C3):- intersecta(Xs,C2,C3), not miembro(X,C2).
 /* calcula la diferencia entre dos conjuntos */
diferencia([],X,X).
diferencia([X|Xs],Y,Z):- diferencia(Xs,Y,Aux), borra_una(X,Aux,Z).
 /* construye un conunto con los elementos de una lista */
conjunto([],[]).
conjunto([X|Xs],C):- conjunto(Xs,C), miembro(X,C).
conjunto([X|Xs],[X|C]):- conjunto(Xs,C), not miembro(X,C).

/* 7) calcula la distancia entre cualquier par de vertices de un grafo G=(V,A), donde V=ConjuntoVertices y A=ConjuntoArcos.
   Previamente defino relaciones (distancia entre vertices establecidos) donde fijo valores. */
distancia(v1,v2,1).
distancia(v1,v3,2).
distancia(v1,v3,2).
distancia(v1,v4,3).
distancia(v2,v3,2).
distancia(v3,v4,1).
distancia(X,Y,D):- distancia(Y,X,D).
/* Preguntar como defino los vertices */

/* determina el tipo de argumento que es recibido al predicado */
argumento(X,lista):- lista(X).
argumento(A,atomo):- atomo(A).

lista([]).
lista([X]):- objeto(X).
objeto(X,objeto).


/* 9) traductor de oraciones */

/* 10) a) ordena una lista de números */
ordena([],[]).
ordena([X|Xs],Z):- ordena(Xs,Aux), inserta_ordenado(X,Aux,Z).

inserta_ordenado(X,[],[X]).
isnerta_ordenado(X,[X|Ys],[X|Ys]).
inserta_ordenado(X,[Y|Ys],[X|[Y|Ys]]):- menor(X,Y).
inserta_ordenado(X,[Y|Ys],[Y|Z]):- noMenor(X,Y), inserta_ordenado(X,Ys,Z).

menor(X,Y):- X < Y.
noMenor(X,Y):- X > Y.


/* 13) Rotar una lista N posiciones a derecha y N posiciones a izquierda */

rotarNder(L,0,L).
rotarNder(L,s(X),Lr):- rotarNizq(L,X,Laux), desplaza_derecha(Laux,Lr).

rotarNizq(L,0,L).
rotarNizq(L,s(X),Lr):- rotarNizq(L,X,Laux), desplaza_izquierda(Laux,Lr).


/* 14) Determina si una lista Les predijo de otra */
prefijo([],L).
prefijo([X|Xs],[X|Ys]):- prefijo(Xs,Ys).

/* 15) Implementar el insertSort (HACER) */
/* 16) Implementar el QuicktSort (HACER) */

/* 17) Evalua una expresión aritmética */
evalua(E,E):- number(E).
evalua(E1+E2,V):- evalua(E1,V1), evalua(E2,V2), V is V1+V2.
evalua(E1*E2,V):- evalua(E1,V1), evalua(E2,V2), V is V1*V2.
evalua(E1-E2,V):- evalua(E1,V1), evalua(E2,V2), V is V1-V2.
evalua(E1/E2,V):- evalua(E1,V1), evalua(E2,V2), V is V1/V2.


/* 18) Reetorna el resultado de una expresion de operaciones entre conjuntos */
eval(X,X):- list(X).
eval(X+Y,Z):- eval(X,X1), eval(Y,Y1), unir(X1,Y1,Z).
eval(X-Y,Z):- eval(X,X1), eval(Y,Y1), diferencia(X1,Y1,Z).
eval(X*Y,Z):- eval(X,X1), eval(Y,Y1), intersecta(X1,Y1,Z).

/* 19) 


/* 20) Relaciona una lista de elementos con su imagen aplanada */
aplanar([],[]).
aplanar([X|Xs],[X|Z]):- not list(X), aplanar(Xs,Z).
aplanar([X|Xs],Z):- list(X), aplanar(X,Aux1), aplanar(Xs,Aux2), unir(Aux1,Aux2,Z).

/* 21) */


/* 22) */


/* Ejercicios opcionales (Pura ineficiencia) */
/* 1) Determinar que elemento y cuantas veces se repite mas en una lista (Pura ineficiencia) */
masVeces([X],X,1).
masVeces([X|Xs],X,C):- veces(X,Xs,C), masVeces(Xs,Y,C1), mayor(C,C1).
masVeces([X|Xs],Y,C):- veces(X,Xs,C1), masVeces(Xs,Y,C), mayor(C,C1).

veces(X,[],1).
veces(X,[X|Xs],Z):- X=Y, veces(X,Xs,C), Z is C+1.
veces(X,[Y|Xs],Z):- X\=Y, veces(X,Xs,Z).

mayor(X,Y):- X >= Y.