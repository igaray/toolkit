%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nn(0).
nn(s(X)):- nn(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 2%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sumar(0,X,X):- nn(X).
sumar(s(X),Y,s(Z)):- sumar(X,Y,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 3%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

producto(0,X,0):- nn(X).
producto(s(X),Y,Z):- producto(X,Y,Aux),
		     sumar(Aux,Y,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 4%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

potencia(X,0,s(0)).
potencia(X,s(Y),Z):- potencia(X,Y,Aux),
		     producto(X,Aux,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5a%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crear_lista(X,Y,[X,Y]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5b%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inserta_ppio(Elto,L,[Elto|L]).

inserta_fin(Elto,[],[Elto]).
inserta_fin(Elto,[X|Xs],[X|Lista]):- inserta_fin(Elto,Xs,Lista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

concatenar([],L,L).
concatenar([X|Xs],Ys,[X|L]):- concatenar(Xs,Ys,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5d%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buscar(X,[X|L]).
buscar(X,[Y|L]):- X\=Y, buscar(X,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5e%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invertir([],[]).
invertir([X|Xs],L):- invertir(Xs,Aux), inserta_fin(X,Aux,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5f%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

borrar_una(X,[],[]).
borrar_una(X,[X|Xs],Xs).
borrar_una(X,[Y|Ys],[Y|Zs]):- X\=Y, borrar_una(X,Ys,Zs).

borrar_todas(X,[],[]).
borrar_todas(X,[X|Xs],Zs):- borrar_todas(X,Xs,Zs).
borrar_todas(X,[Y|Ys],[Y|Zs]):- X\=Y, borrar_todas(X,Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5g%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cambiar_una(X,Y,[],[]).
cambiar_una(X,Y,[X|Xs],[Y|Xs]).
cambiar_una(X,Y,[Z|Zs],[Z|L]):- Z\=X, cambiar_una(X,Y,Zs,L).

cambiar_todas(X,Y,[],[]).
cambiar_todas(X,Y,[X|Xs],[Y|L]):- cambiar_todas(X,Y,Xs,L).
cambiar_todas(X,Y,[Z|Zs],[Z|L]):- Z\=X, cambiar_todas(X,Y,Zs,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5h%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

palindroma([],[]).
palindroma([X|Xs],[X|L]):- palindroma(Xs,Aux), inserta_fin(X,Aux,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5i%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ult_elemento([X],X,[]).
ult_elemento([X|Xs],Y,[X|L]):- Xs\=[], ult_elemento(Xs,Y,L).

desplaza_der([],[]).
desplaza_der(Lista,[UE|L_sin_UE]):- Lista\=[], ult_elemento(Lista,UE,L_sin_UE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 5j%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

desplaza_izq([],[]).
desplaza_izq([X|Xs],L):- inserta_fin(X,Xs,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 6a%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pertenece(X,[X|Xs]).
pertenece(X,[Y|Ys]):- X\=Y, pertenece(X,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 6b%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insertar(X,Y,[X|Y]):- not pertenece(X,Y).
insertar(X,Y,Y):- pertenece(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 6c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

union([],C1,C1).
union([X|C1],C2,C):- insertar(X,C2,Aux), union(C1,Aux,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 6d%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interseccion([],C,[]).
interseccion([X|C1],C2,[X|C]):- pertenece(X,C2), interseccion(C1,C2,C).
interseccion([X|C1],C2,C):- not pertenece(X,C2), interseccion(C1,C2,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 6e%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diferencia([],C,[]).
diferencia([X|C1],C2,[X|C]):- not pertenece(X,C2), diferencia(C1,C2,C).
diferencia([X|C1],C2,C):- pertenece(X,C2), diferencia(C1,C2,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 6f%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

armar_cjto([],[]).
armar_cjto([X|Xs],[X|C]):- not pertenece(X,Xs), armar_cjto(Xs,C).
armar_cjto([X|Xs],C):- pertenece(X,Xs), armar_cjto(Xs,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 7%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arco(a,b,1).
arco(b,a,1).

arco(a,c,2).
arco(c,a,2).

arco(a,d,3).
arco(d,a,3).

arco(d,e,1).
arco(e,d,1).

arco(f,e,5).
arco(e,f,5).

arco(f,c,8).
arco(c,f,8).

distancia(X,Y,D,Visitados):- insertar(X,Visitados,Visita),
			     buscar_camino(X,Y,D,Visita).

buscar_camino(X,X,0,Visitados).

buscar_camino(X,Y,D,Visitados):- arco(X,Z,N),
				 not pertenece(Z,Visitados),
				 insertar(Z,Visitados,Aux),
				 buscar_camino(Z,Y,N1,Aux),
				 D is N+N1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 9%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

traduccion(el,the).
traduccion(perro,dog).
traduccion(y,and).
traduccion(leon,lyon).

traducir([],[]).
traducir([X|Xs],[Y|Ys]):- traduccion(X,Y), traducir(Xs,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 12%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

regla1(X,Y):- concatenar([a],X,Aux),
	      concatenar(Aux,[c],Y).

regla2(X,Y):- concatenar([b],X,Aux),
	      concatenar(Aux,[c],Y).

cadena([b,c]).
cadena1([b,c]).
cadena(X):- regla1([Y|Sal],X), Y=a, cadena([Y|Sal]).
cadena(X):- regla1([Y|Sal],X), Y=b, cadena1([Y|Sal]).
cadena1(X):- regla2(Sal,X), cadena1(Sal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 13%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rotar_der(Lista,0,Lista).
rotar_der(Lista,N,Rsta):- N\=0,
			  desplaza_der(Lista,Aux),
			  N1 is N-1,
			  rotar_der(Aux,N1,Rsta).

rotar_izq(Lista,0,Lista).
rotar_izq(Lista,N,Rsta):- N\=0,
			  desplaza_izq(Lista,Aux),
			  N1 is N-1,
			  rotar_izq(Aux,N1,Rsta).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ejercicio 20%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lista([]).
lista([X|Xs]):- lista(Xs).

aplanar([],[]).

aplanar([X|Xs],[X|Ap]):- not lista(X),
			 aplanar(Xs,Ap).

aplanar([X|Xs],Ap):- lista(X),
		     aplanar(X,Aux),
		     aplanar(Xs,Aux1),
		     concatenar(Aux,Aux1,Ap).

par_impar(Lista, Rsta):- aplanar(Lista,Aux),
		   	 pares(Aux,LP),
		   	 impares(Aux,LI),
		   	 concatenar([LP],[LI],Rsta).

pares([],[]).
pares([X|Xs],[X|Ys]):- (0 is X mod 2), pares(Xs,Ys).
pares([X|Xs],Ys):- (1 is X mod 2), pares(Xs,Ys).

impares([],[]).
impares([X|Xs],[X|Ys]):- (1 is X mod 2), impares(Xs,Ys).
impares([X|Xs],Ys):- (0 is X mod 2), impares(Xs,Ys).
		   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Opcional 1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mas_v([X],L,X,Veces):- cuantas_veces(X,L,Veces). %% 
mas_v([X|Xs],L,Y,Veces):- cuantas_veces(X,L,Aux),%%
			  mas_v(Xs,L,Y,Veces),   %%
			  mayor(Veces,Aux).      %%
mas_v([X|Xs],L,X,Aux):- cuantas_veces(X,L,Aux),  %%
			mas_v(Xs,L,Y,Veces),     %%
			mayor(Aux,Veces).        %%   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mayor(X,Y):- X>=Y. %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cuantas_veces(X,[],0).                              %%
cuantas_veces(X,[X|Xs],V):- cuantas_veces(X,Xs,Aux),%%
			    V is Aux+1.             %%
cuantas_veces(X,[Y|Ys],V):- X\=Y,                   %%
			    cuantas_veces(X,Ys,V).  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mas_veces(L,E,N):- armar_cjto(L,C),
		   mas_v(C,L,E,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Insert sort%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ordenar([],[]).

ordenar([X|Xs],Ord):- ordenar(Xs,Aux),
		      insertar_ord(X,Aux,Ord).

insertar_ord(X,[],[X]).

insertar_ord(X,[Y|Ys],[X,Y|Ys]):- mayor(Y,X).

insertar_ord(X,[Y|Ys],[Y|Ord]):- not mayor(Y,X),
			         insertar_ord(X,Ys,Ord).

%%%%%%%%%%%%%%%%%%%%%%%%%%%mayor para palabras%%%%%%%%%%%%%%%%%%%%%%%%%
mayorp(X,Y):- name(X,XL),
	      name(Y,YL),
	      mayor_pal(XL,YL).

mayor_pal([X|Xs],[]).
mayor_pal([X|Xs],[Y|Ys]):- X>Y.
mayor_pal([X|Xs],[Y|Ys]):- X=Y,
			   mayor_pal(Xs,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%mayor para notacion s(n)%%%%%%%%%%%%%%%%%%%%%%%

mayor_s(s(X),0).
mayor_s(s(X),s(Y)):- mayor_s(X,Y).