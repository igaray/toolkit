$ TPB E1

% From TAOP

natural_number(0).
natural_number(s(X)) :- natural_number(X).

% Se pueden generar todos los numeros naturales de la siguiente manera

generar(0).
generar(s(N)):-
    generar(N),
    natural(N).

% TPB E2

% 0 + X = X for all X
% (X + 1) + Y = (Z + 1) for all X,Y,Z such that X + Y = Z

addition(0,X,X) :- natural_number(X).
addition(s(X),Y,s(Z)) :- addition(X,Y,Z).

% TPB E3

% 0 <= X for all X
% X + 1 <= Y + 1 for all X,y scuh that X <= Y

less_or_equal(0,X) :- natural_number(X).
less_or_equal(s(X),s(Y)) :- less_or_equal(X,Y).

% 0 * X = 0 for all X 
% (X + 1) * Y = Z for all X,Y,Z such that Z = (X * Y) + Y

multiplication(0,X,0).
multiplication(s(X),Y,Z) :- multiplication(X,Y,XY), addition(XY,Y,Z).

% 0 ^ (X + 1) = 0
% (X + 1) ^ 0 = 1
% X ^ (N + 1) = Y for all N,X,Y such that Y = (X ^ N) * X

exp(s(X),0,0).
exp(0,s(X),s(0)).
exp(s(N),X,Y) :- exp(N,X,Z), multiplication(Z,X,Y).

% 0! = 1
% (X + 1)! = X! * (X + 1) for all X

factorial(0,s(0)).
factorial(s(X),Y) :- factorial(X,Z), multiplication(s(X),Z,Y). 

minimum(N1,N2,N1) :- less_or_equal(N1,N2).
minimum(N1,N2,N2) :- less_or_equal(N2,N1). 

ackermann(0,N,s(N)).
ackermann(s(M),0,Output) :- ackermann(M,s(0),Output).
ackermann(s(M),s(N),Output) :- ackermann(M,OutPut,Auxiliary).

% TPB E4

multiplicar(0,Y,0).
multiplicar(s(X),Y,Z) :-
    multiplicar(X,Y,P),
    suma(Y,P,Z).
                        
potencia(X,0,s(0)).
potencia(X,s(Y),Z) :-
    potencia(X,Y,Pot),
    multiplicar(X,Pot,Z).

% TPB E5

% Inciso a

crear(X,Y,[X,Y]).

% Inciso b-1

insertarPrincipio(X,[],[X]).
insertarPrincipio(X,Ys,[X|Ys]).

% Inciso b-2

insertarFinal(X,[],[X]).
insertarFinal(X,[L|Ls],[L|L2s]) :- insertarFinal(X,Ls,L2s).

% Inciso c

concatenar([],X,X).
concatenar([X|Xs],Ys,[X|Zs]) :- concatenar(Xs,Ys,Zs).

% Inciso d

pertenece(X,[X|Xs]).
pertenece(X,[Y|Xs]) :- pertenece(X,Xs).

% Inciso e

invertir([],[]).
invertir([X|Xs],Y) :-
    invertir(Xs,Z),
    concatenar(Z,X,Y).
                     
% Inciso f-1

borrar(X,[X],[]).
borrar(X,[X|Xs],Xs).
borrar(X,[Y|Xs],[Y|Zs]) :- borrar(X,Xs,Zs).

% Inciso f-2

borrarTodas(X,[X],[]).
borrarTodas(X,[X|Xs],Z) :- borrarTodas(X,Xs,Z).
borrarTodas(X,[Y|Xs],[Y|Zs]) :-borrarTodas(X,Xs,Zs).

% Inciso g-1

cambiar(X,Y,[X],[Y]).
cambiar(X,Y,[X|Xs],[Y|Xs]).
cambiar(X,Y,[Z|Xs],[Z|Ts]) :- cambiar(X,Y,Xs,Ts).

% Inciso g-2

cambiarTodas(X,Y,[X],[Y]).
cambiarTodas(X,Y,[X|Xs],[Y|Zs]) :- cambiarTodas(X,Y,Xs,Zs).
cambiarTodas(X,Y,[Z|Xs],[Z|Ts]) :- cambiarTodas(X,Y,Xs,Ts).

% Inciso h

iguales([],[]).
iguales([X|Xs],[X|Ys]) :- iguales(Xs,Ys).

palindroma([]).
palindroma(L) :-
    invertir(L,Linv),
    iguales(Linv,L).
                
% Inciso i

generar(L,L2n) :-
    invertir(L,Linv),
    concatenar(L,Linv,L2n).
                 
% Inciso k

desplazarIzq([],[]).
desplazarIzq([X|Xs],F) :-
    insertarFinal(X,Xs,F).

% Inciso j

desplazarDer([],[]).
desplazarDer(X,Y) :-
    desplazarIzq(Y,X).

% *****

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

a(X,Y,[X,Y|[]]).

b1(X,Ys,[X|Ys]).

b2(X,Ys,Zs) :- append(Ys,[X],Zs).

c([],Ys,Ys).
c([X|Xs],Ys,[X|Zs]) :- c(Xs,Ys,Zs).

d(X,[X|_]).
d(X,[Y|Ys]) :- X \= Y, d(X,Ys).

e(Xs,Ys) :- e(Xs,[],Ys).
e([X|Xs],Acc,Ys) :- e(Xs,[X|Acc],Ys).
e([],Ys,Ys).

f1(X,[X|Xs],Xs).
f1(X,[Y|Ys],[Y|Zs]) :- f1(X,Ys,Zs).

f2([],X,[]).
f2([X|Xs],X,Ys) :- f2(Xs,X,Ys).
f2([X|Xs],Z,[X|Ys]) :- X \= Z, f2(Xs,Z,Ys).

cambiar(x1,x2).
g1(X,[X|Xs],[Y|Xs]) :- cambiar(X,Y).
g1(X,[Y|Ys],[Y|Zs]) :- g1(X,Ys,Zs).

g2(X,[],[]).
g2(X,[X|Xs],[Z|Ys]) :- cambiar(X,Z), g2(X,Xs,Ys).
g2(Z,[X|Xs],[X|Ys]) :- X \= Z, g2(Z,Xs,Ys).

h(Xs) :- e(Xs,Ys), iguales(Xs,Ys).
iguales([],[]).
iguales([X|Xs],[Y|Ys]) :- X = Y, iguales(Xs,Ys).

i(Xs,Ys) :- e(Xs,Zs), c(Xs,Zs,Ys).

% TPB E6

% Inciso a.

nopertenece(X,[]).
nopertenece(X,[Y|Xs]) :-
    X \= Y,
    nopertenece(X,Xs).

conjValido([]).
conjValido([X|Xs]) :-
    nopertenece(X,Xs),
    conjValido(Xs).

% Inciso b.

pertenece(X,[X]).
pertenece(X,[X|Xs]).
pertenece(X,[Y|Xs]) :- pertenece(X,Xs).

% Inciso c.

incorporar(X,[],[X]).
incorporar(X,Ys,[X|Ys]).
incorporar(X,Ys,Ys) :- pertenece(X,Ys).

% Inciso d.

unir(Xs,[],Xs).
unir(Xs,[Y|Ys],Zs) :-
    pertenece(Y,Xs),
    unir(Xs,Ys,Zs).
unir(Xs,[Y|Ys],[Y|Zs]) :- unir(Xs,Ys,Zs).

% Inciso e.

intersectar(Xs,[],[]).
intersectar(Xs,[Y|Ys],[Y|Zs]) :-
    pertenece(Y,Xs),
    intersectar(Xs,Ys,Zs).
intersectar(Xs,[Y|Ys],Zs) :- intersectar(Xs,Ys,Zs).

% Inciso f.

diferencia([],Ys,[]).
diferencia([X|Xs],Ys,Zs) :-
    pertenece(X,Ys),
    diferencia(Xs,Ys,Zs).
diferencia([X|Xs],Ys,[X|Zs]) :- diferencia(Xs,Ys,Zs).

% Inciso g.

armar([],[]).
armar([X|Xs],Z) :-
    pertenece(X,Xs),
    armar(Xs,Z).
armar([X|Xs],[X|Zs]) :- armar(Xs,Zs).

% TPB E7

% Grafo dirigido con aristas rotuladas con las distancia entre los nodos que une

arista(a,b,10).
arista(a,c,13).
arista(b,g,15).
arista(b,d,26).
arista(c,d,16).
arista(d,e,22).
arista(d,h,3).
arista(e,f,35).
arista(h,f,31).
arista(g,e,7).

distancia(V1,V1,0).
distancia(V1,V2,Distancia) :-
    arista(V1,Vn,Dist1),
    distancia(Vn,V2,Dist2),
    Distancia is Dist1 + Dist2.

% TPB E9

buscar([],X,X).
buscar([[X,Y]|Ds],X,Y).
buscar([[T,J]|Ds],X,Y) :- buscar(Ds,X,Y).


traducir(Ds,[],[]).
traducir(Ds,[X|Xs],[Y|Ys]) :-
    buscar(Ds,X,Y),
    traducir(Ds,Xs,Ys).
traducir(Ds,[X|Xs],[Y|Ys]) :- traducir(Ds,Xs,Ys).

% TPB E10

% Inciso a

menor([X],X).
menor([X|Xs],X) :-
    menor(Xs,Y),
    X < Y.
menor([X|Xs],Y) :-
    menor(Xs,Y),
    Y < X.

eliminar(X,[X],[]).
eliminar(X,[X|Xs],Xs).
eliminar(X,[Y|Xs],[Y|Zs]) :- eliminar(X,Xs,Zs).

ordenar([],[]).
ordenar(L,[X|L2]) :-
    menor(L,X),
    eliminar(X,L,L3),
    ordenar(L3,L2).

% Inciso b

menorPalabras([X],X).
menorPalabras([X|Xs],X) :-
    menorPalabras(Xs,Y),
    compare(<,X,Y).
menorPalabras([X|Xs],Y) :-
    menorPalabras(Xs,Y),
    compare(<,Y,X).

eliminarPalabra(X,[X],[]).
eliminarPalabra(X,[X|L],L).
eliminarPalabra(X,[Y|Ys],[Y|Zs]) :- eliminar(X,Ys,Zs).

ordenarPalabras([],[]).
ordenarPalabras(L,[X|L2]) :-
    menorPalabras(L,X),
    eliminarPalabra(X,L,L3),
    ordenarPalabras(L3,L2).

% TPB E11

/*
Una solucion alternativa a la anterior seria cambiar el metodo de ordenamiento,
en el ejercicio anterior utilice el select-sort, ahora se podria utilizar el
quicksort o el insert-sort u otro
*/

% TPB E12

entrada('jamon',5).
entrada('melon con jamon', 15).
entrada('mayonesa de aves', 6).
minuta('papas fritas', 4).
minuta('milanesa',6).
minuta('rabas',9).
postre('flan',4).
postre('budin',7).
postre('helado',5).

suma(X,0,X).
suma(X,Y,Z) :- Z is X + Y.

menu(X,[Y,Z,T]) :- entrada(Y,P1),
                   minuta(Z,P2),
                   postre(T,P3),
                   suma(P1,P2,P),
                   suma(P,P3,W),
                   W < X.

% TPB E14

desplazarIzq([],[]).
desplazarIzq([X|Xs],F) :- insertarFinal(X,Xs,F).

desplazarDer([],[]).
desplazarDer(X,Y) :- desplazarIzq(Y,X).

rotarNDerecha(X,0,X).
rotarNDerecha(X,N,Z) :-
    desplazarDer(X,Y),
    M is N - 1,
    rotarNDerecha(Y,M,Z).

rotarNIzquierda(X,0,X).
rotarNIzquierda(X,N,Z) :-
    desplazarIzq(X,Y),
    M is N - 1,
    rotarNIzquierda(Y,M,Z).

% TPB E15

prefijo([],Ys).
prefijo([X|Xs],[X|Ys]) :- prefijo(Xs,Ys).

% TPB E16

% Inciso a

insertarOrdenado(X,[],[X]).
insertarOrdenado(X,[Y|Ys],[X,Y|Ys]) :- X =< Y.
insertarOrdenado(X,[Y|Ys],[Y|Zs]) :-
    X > Y,
    insertarOrdenado(X,Ys,Zs).

insert_sort([],[]).
insert_sort([X|Lista_Desordenada],ListaOrdenada):-
    insert_sort(Lista_Desordenada,Lista),
    insertarOrdenado(X,Lista,ListaOrdenada).

% Inciso b

menor([X],X).
menor([X|Xs],X):-
    menor(Xs,Y),
    X < Y.
menor([X|Xs],Y):-
    menor(Xs,Y),
    Y =< X.

eliminar(X,[X],[]).
eliminar(X,[X|Xs],Xs).
eliminar(X,[Y|Xs],[Y|Zs]) :- eliminar(X,Xs,Zs).

select_sort([],[]).
select_sort(L,[X|L2]) :-
    menor(L,X),
    eliminar(X,L,L3),
    select_sort(L3,L2).
                             
% Inciso c

partir([],P,[],[]).
partir([H|LIST],P,[H|L],R) :- H =< P, partir(LIST,P,L,R).
partir([H|LIST],P,L,[H|R]) :- partir(LIST,P,L,R).

quicksort([],[]).
quicksort([X|Xs],Z) :-
    partir(Xs,X,L1,L2),
    quicksort(L1,L1Ord),
    quicksort(L2,L2Ord),
    append(L1Ord,[X|L2Ord],Z).

% TPB E17

eval(E1/E2,V) :- eval(E1,V1),
                 eval(E2,V2),
                 V2 \= 0,
                 V is V1 / V2.

eval(E1*E2,V) :- eval(E1,V1),
                 eval(E2,V2),
                 V is V1 * V2.

eval(E1-E2,V) :- eval(E1,V1),
                 eval(E2,V2),
                 V is V1 - V2.

eval(E1+E2,V) :- eval(E1,V1),
                 eval(E2,V2),
                 V is V1 + V2.
eval(N,N):- integer(N).

% TPB E18

pertenece(X,[X]).
pertenece(X,[X|Xs]).
pertenece(X,[Y|Xs]) :- pertenece(X,Xs).

unir(Xs,[],Xs).
unir(Xs,[Y|Ys],Zs) :-
    pertenece(Y,Xs),
    unir(Xs,Ys,Zs).
unir(Xs,[Y|Ys],[Y|Zs]) :- unir(Xs,Ys,Zs).

nopertenece(X,[]).
nopertenece(X,[Y|Xs]) :-
    X \= Y,
    nopertenece(X,Xs).

conjValido([]).
conjValido([X|Xs]) :-
    nopertenece(X,Xs),
    conjValido(Xs).

intersectar(Xs,[],[]).
intersectar(Xs,[Y|Ys],[Y|Zs]) :-
    pertenece(Y,Xs),
    intersectar(Xs,Ys,Zs).
intersectar(Xs,[Y|Ys],Zs) :- intersectar(Xs,Ys,Zs).

diferencia([],Ys,[]).
diferencia([X|Xs],Ys,Zs) :-
    pertenece(X,Ys),
    diferencia(Xs,Ys,Zs).
diferencia([X|Xs],Ys,[X|Zs]) :- diferencia(Xs,Ys,Zs).

unionExclusiva(X,Y,Z) :-
    unir(X,Y,P),
    intersectar(X,Y,Q),
    diferencia(P,Q,Z).

eval(E1+E2,C) :- eval(E1,C1),
                 eval(E2,C2),
                 unir(C1,C2,C).
eval(E1*E2,C) :- eval(E1,C1),
                 eval(E2,C2),
                 intersectar(C1,C2,C).
eval(E1-E2,C) :- eval(E1,C1),
                 eval(E2,C2),
                 diferencia(C1,C2,C).
eval(E1/E2,C) :- eval(E1,C1),
                 eval(E2,C2),
                 unionExclusiva(C1,C2,C).
eval(C,C) :- conjValido(C).

% TPB E19

sobre('bloque1','bloque1').
sobre('bloque19','bloque19').
sobre('bloque3','bloque3').
sobre('bloque2','bloque2').
sobre('bloque1','bloque19').
sobre('bloque19','bloque3').
sobre('bloque3','bloque2').

encima(B1,B1).
encima(B1,B2) :-
    sobre(B1,BX),
    sobre(BX,B2).

% TPB E20

concatenar([],X,X).
concatenar([X|Xs],Ys,[X|Zs]) :- concatenar(Xs,Ys,Zs).

aplanar([],[]).
aplanar([X|Y],Z) :-
    aplanar(X,T1),
    aplanar(Y,T2),
    concatenar(T1,T2,Z).
aplanar([X|Y],[X|Z]) :-
    atom(X),
    aplanar(Y,Z).

% TPB E21

% Inciso a

crearArbolVacio(X,arbolBin(vacio,X,vacio)) :- atom(X).

% Inciso b

insertar(X,vacio,arbolBin(vacio,X,vacio)).
insertar(X,arbolBin(HI,R,HD),arbolBin(HI,R,HD)) :- X = R.
insertar(X,arbolBin(HI,R,HD),arbolBin(NHI,R,HD)) :-
    X < R,
    insertar(X,HI,NHI).
insertar(X,arbolBin(HI,R,HD),arbolBin(HI,R,NHD)) :-
    X > R,
    insertar(X,HD,NHD).
insertar(X,R,arbolBin(X,R,vacio)) :- X < R.
insertar(X,R,arbolBin(vacio,R,X)) :- X > R.

% Inciso c

% No funciona eliminarMin

eliminarMin(arbolBin(vacio,X,HD),X,HD).
eliminarMin(arbolBin(HI,R,HD),X,NA) :- eliminarMin(HI,X,NA)

eliminar(X,arbolBin(vacio,X,vacio),vacio).
eliminar(X,arbolBin(HI,R,HD),NA) :-
    X > R,
    eliminar(X,HD,NA).
eliminar(X,arbolBin(HI,R,HD),NA) :-
    X < R,
    eliminar(X,HI,NA).
eliminar(X,arbolBin(HI,X,HD),NA) :-
    HD \= vacio,
    eliminarMin(HD,Min,T),
    NA = arbolBin(HI,Min,T).
eliminar(X,arbolBin(HI,X,HD),NA) :-
    HD = vacio,
    NA = HI.
eliminar(X,arbolBin(HI,X,HD),NA) :- NA = HD.

% Inciso d

maximo(X,Y,X) :- X > Y.
maximo(X,Y,Y) :- X =< Y.

altura(vacio,-1).
altura(arbolBin(HI,R,HD),A) :-
    altura(HI,A1),
    altura(HD,A2),
    maximo(A1,A2,M),
    A is M + 1.
altura(X,0) :- integer(X).

% Inciso e

difierenALoSumoEnUno(X,Y) :- X is Y + 1.
difierenALoSumoEnUno(X,Y) :- X is Y - 1.
difierenALoSumoEnUno(X,Y) :- X = Y.

balanceado(vacio).
balanceado(arbolBin(HI,R,HD)) :-
    altura(HI,AI),
    altura(HD,AD),
    difierenALoSumoEnUno(AI,AD).

% TPB E22

% Inciso a

generarArbol(X,arbolBin(vacio,X,vacio)) :- atom(X).
generarArbol(E1+E2,NA) :- generarArbol(E1,A1),
                          generarArbol(E2,A2),
                          NA = arbolBin(A1,+,A2).
generarArbol(E1-E2,NA) :- generarArbol(E1,A1),
                          generarArbol(E2,A2),
                          NA = arbolBin(A1,-,A2).
generarArbol(E1*E2,NA) :- generarArbol(E1,A1),
                          generarArbol(E2,A2),
                          NA = arbolBin(A1,*,A2).
generarArbol(E1/E2,NA) :- generarArbol(E1,A1),
                          generarArbol(E2,A2),
                          NA = arbolBin(A1,/,A2).

% Inciso b

concatenar([],X,X).
concatenar([X|Xs],Ys,[X|Zs]) :- concatenar(Xs,Ys,Zs).

post(vacio,[]).
post(arbolBin(HI,R,HD),L) :- post(HI,LI),
                             post(HD,LD),
                             concatenar(LI,LD,LC),
                             concatenar(LC,[R],L).


% TPB E OPCIONAL

cantVeces(X,[],0).
cantVeces(X,[X|Xs],V):- cantVeces(X,Xs,W),
                        V is W+1.
cantVeces(X,[Y|Xs],V):- cantVeces(X,Xs,V).

masVeces([],_,0).
masVeces([X|Xs],X,V):- cantVeces(X,[X|Xs],V),
                       masVeces(Xs,X,W),
                       V > W.
masVeces([X|Xs],Y,W):- cantVeces(X,[X|Xs],V),
                       masVeces(Xs,Y,W),
                       V =< W.


