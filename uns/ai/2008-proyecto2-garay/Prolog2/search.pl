%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                   Buscar                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% search(+Origen, +Destino, -Costo, -Camino)
%
% Calcula usando A* el camino óptimo (de menor Costo) para llegar desde el Origen al Destino a través de un
% cierto Camino, al resultado lo invierte porque se obtiene el Camino desde el nodo destino al actual y lo
% se busca es el paso inmediato para poder elegir a donde desplazarse cuando fuere posible. Además se elimina
% el primer elemento luego de invertir el camino porque esté será la celda actual.
% Este shell dinámicamente realiza un assert para definir la meta y luego de realizar la búsqueda efectua
% el correspondiente retract para que en próximas búsquedas no haya más de una meta.
% Por como se invoca a este shell, siempre encontrará un camino, ya que en los casos en que no exista uno,
% los predicados que lo consultan habrán fallado porque la posición destino no estaba en el mapa o bien,
% porque estaba en el mapa y era un obstáculo.

search([FOrigen,COrigen],[FDest,CDest],Costo,Camino):-
   assert(is_goal(agt_world_rep([FDest,CDest],_,_))),
   buscarAstar([nodo(agt_world_rep([FOrigen,COrigen],_,_),[],0,0)], [], Camino2, Costo),
   reverse(Camino2,Camino3),
   Camino3 = [_X|Camino],
   retract(is_goal(agt_world_rep([FDest,CDest],_,_))).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                       Busqueda heuristica A* sobre el mapa                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscarAstar(+ Open, + Closed, - Solucion)
%
% El predicado buscarAstar/3 realiza busqueda heuristica A* sobre el mapa generado por el agente
% Este algoritmo realiza control de visitados y si un nodo N que ya pertenece a
% Open o a Closed es generado a partir de un mejor camino entonces N es agregado nuevamente

buscarAstar(Open, _Closed, [Pos|Camino], Costo):-
   seleccionarA(Nodo, Open, _OpenSinNodo),
   Nodo = nodo(Celda,Camino,Costo,_Festimado),
   Celda = agt_world_rep(Pos,_Cont,_Ter),
   is_goal(Celda),!.

buscarAstar(Open, Closed, Solucion, Costo):-
   seleccionarA(Nodo, Open, OpenSinNodo),
   vecinos(Nodo,Vecinos),
   agregar_costosA(Vecinos,Nodo),
   mejorCaminoEnOpenOClosed(Vecinos,OpenSinNodo,Vecinos2,OpenSinNodo2), %si alguno de los vecinos ya pertenece a open y tiene mejor F entonces quitamos ese nodo de open.%%%%Si algun vecino ya pertenece a open pero no tiene mejor F entonces quitamos ese vecino de la lista de vecinos
   mejorCaminoEnOpenOClosed(Vecinos2,Closed,Vecinos3,Closed2),%si alguno de los vecinos ya pertenece a closed y tiene mejor F entonces quitamos ese nodo de closed.%%%%Si algun vecino ya pertenece a closed pero no tiene mejor F entonces quitamos ese vecino de la lista de vecinos
   agregar_fronteraA(Vecinos3, OpenSinNodo2, NewOpen),
   buscarAstar(NewOpen, [Nodo|Closed2], Solucion, Costo).


% vecinos(+ Nodo, -Vecinos)
% Dado un nodo, retorna todos los nodos vecinos del nodo pero descartando aquellos que son: obs.

vecinos(Nodo,Vecinos):-
   Nodo = nodo(N, Camino,_CostoCamN,_FestimadoN),
   N = agt_world_rep(PosN,_ContN,_TerN),
   findall(nodo(V,[PosN|Camino],_CostoCamV,_FestimadoV),
                (   adyacenteA(N,V),
                        V \= agt_world_rep(_,obs,-)
                ),
                Vecinos).


% mejorCaminoEnOpenOCLosed(+ Vecinos, +Open/Closed, -Vecinos2,- Open/Closed2)
% Si un nodo N que ya pertenece a Open es generado a partir de un "mejor camino",
% se reemplazara en Open el nodo N por este otro nodo N con "mejor camino".
% Si un nodo N que ya pertenece a Closed es generado a partir de un mejor camino,
% N es eliminado de Closed y agregado nuevamente a Open para que pueda ser
% considerado eventualmente.
% En este predicado solo de realizan dos cosas:
% quitamos los nodos vecinos que no deben ser considerados (ya que son repetidos y no tienen un mejor camino)
% quitamos de open o close los nodos que tiene peor camino que los nodos vecinos generados.
% no se los coloca de nuevo en Open ya que se hace despues en agregar frontera.

% caso base
mejorCaminoEnOpenOClosed([],Open_o_Closed,[],Open_o_Closed).

% si algun vecino ya pertenece a open/closed y tiene mejor F entonces quitamos ese nodo de open/closed.
mejorCaminoEnOpenOClosed([Vecino|RestoVecinos],Open_o_Closed,[Vecino|RestoVecinos2],Open_o_Closed3):-
   Vecino = nodo(V,_,_,_),
   member(nodo(V,_,_,_),Open_o_Closed),
   recuperarNodo(Vecino,Open_o_Closed,Nodo),
   mejorCamino(Vecino,Nodo),
   delete(Open_o_Closed,Nodo,Open_o_Closed2),
   mejorCaminoEnOpenOClosed(RestoVecinos,Open_o_Closed2,RestoVecinos2,Open_o_Closed3).

% Si algun vecino ya pertenece a open/closed pero no tiene mejor F entonces quitamos ese vecino de la lista de vecinos(para que no haya nodos repetidos)
mejorCaminoEnOpenOClosed([Vecino|RestoVecinos],Open_o_Closed,RestoVecinos2,Open_o_Closed3):-
   Vecino = nodo(V,_,_,_),
   member(nodo(V,_,_,_),Open_o_Closed),
   recuperarNodo(Vecino,Open_o_Closed, Nodo),
   not(mejorCamino(Vecino,Nodo)),
   mejorCaminoEnOpenOClosed(RestoVecinos,Open_o_Closed,RestoVecinos2,Open_o_Closed3).

%Si algun vecino no pertenece a open/closed entonces lo dejo en la lista de vecinos y sigo buscando
mejorCaminoEnOpenOClosed([Vecino|RestoVecinos],Open_o_Closed,[Vecino|RestoVecinos2],Open_o_Closed3):-
   Vecino = nodo(V,_,_,_),
   not(member(nodo(V,_,_,_),Open_o_Closed)),
   mejorCaminoEnOpenOClosed(RestoVecinos,Open_o_Closed,RestoVecinos2,Open_o_Closed3).


% Recupera de "open_o_closed" el nodo "Nodo" que es parecido a "Vecino" pero con distinto camino y costo.
% recuperarNodo(+ Vecino,+ Open_o_Closed,- Nodo)

recuperarNodo(nodo(V,_CamV,_CostV,_FestV),[nodo(N,CamN,CostN,FestN)|_Resto],nodo(N,CamN,CostN,FestN)):-
   N = V,!.

recuperarNodo(nodo(V,CamV,CostV,FestV),[nodo(N,_,_,_)|Resto],Nodo):-
   N \= V,
   recuperarNodo(nodo(V,CamV,CostV,FestV),Resto,Nodo).


% mejorCamino(+ Vecino,+ Nodo)
% este predicado es exitoso si Vecino tiene un camino con menor costo estimado que Nodo
% sabiendo que los dos nodos son el mismo nodo pero con distintos caminos y Festimado

mejorCamino(nodo(_,_,_,FestimadoV),nodo(_,_,_,FestimadoN)):-
   FestimadoV < FestimadoN.


%seleccionarA(-Nodo, +Open, -OpenSinNodo) selecciona el primer nodo de la frontera

seleccionarA(Nodo, [Nodo|RestoOpen], RestoOpen).


%agregar_fronteraA(+Vecinos, +OpenSinNodo, -NewOpen),
%Los vecinos se agregan a la frontera y se ordena segun f(n) para hacer busqueda A*

agregar_fronteraA(Vecinos, Frontera1, Frontera3):-
   append(Vecinos,Frontera1,Frontera2),
   quick_sort(Frontera2,Frontera3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               Quicksort                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% quick_sort(+ Frontera2,- Frontera3)
%
% Ordena la frontera segun el valor f(n) del nodo mediante el algoritmo de ordenamiento rapido

quick_sort([],[]).

quick_sort([Pivot|L],Res):-
    pivotear(Pivot,L,Lizq,Lder),
    quick_sort(Lizq,ResLizq), quick_sort(Lder,ResLder),
    append(ResLizq,[Pivot|ResLder],Res).

pivotear(_,[],[],[]).

pivotear(Pivot,[X|L],[X|Lizq],Lder):- menor(X,Pivot), pivotear(Pivot,L,Lizq,Lder).

pivotear(Pivot,[X|L],Lizq,[X|Lder]):- mayorIgual(X,Pivot), pivotear(Pivot,L,Lizq,Lder).

menor(nodo(_,_,_,F1),nodo(_,_,_,F2)):- F1 < F2.

mayorIgual(nodo(_,_,_,F1),nodo(_,_,_,F2)):- F1 >= F2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                Adyacente                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% adyacenteA(+ Celda1,- Celda2)
%
% Dada una celda del mapa, nos devuelve las adyacentes(una para cada punto cardinal)

adyacenteA(agt_world_rep([F1,C],_,_), agt_world_rep([F2,C],T2,Cont2)):- F2 is F1 - 1,agt_world_rep([F2,C],T2,Cont2).
adyacenteA(agt_world_rep([F,C1],_,_), agt_world_rep([F,C2],T2,Cont2)):- C2 is C1 + 1,agt_world_rep([F,C2],T2,Cont2).
adyacenteA(agt_world_rep([F1,C],_,_), agt_world_rep([F2,C],T2,Cont2)):- F2 is F1 + 1,agt_world_rep([F2,C],T2,Cont2).
adyacenteA(agt_world_rep([F,C1],_,_), agt_world_rep([F,C2],T2,Cont2)):- C2 is C1 - 1,agt_world_rep([F,C2],T2,Cont2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              Agregar Costos                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregar_costosA( +- Vecinos, +Nodo)
%
% Agrega a la lista de vecinos de un nodo, los costos g(n) de llegar a esos vecinos y agrega la estimacion
% del costo total f(n) = g(n)+f(n)

agregar_costosA([],_Nodo).

agregar_costosA([V|RestoV],nodo(N,CamN,CostoN,FestimadoN)):-
   V= nodo(MV,_CamV,CostoV,FestimadoV),
   costoA(MV,C),
   CostoV is C + CostoN,
   distManhattan(MV,H),                                                 % heuristica (distancia de Manhattan)
   FestimadoV is CostoV + H,
   agregar_costosA(RestoV,nodo(N,CamN,CostoN,FestimadoN)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  Costos                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% costoA(+ Nodo, - Costo)
%
% Retorna el costo de pasar por el nodo dado, teniendo en cuenta que pasar por una baldosa consume 1 barra
% de bateria, por alfombra consume 2, y si es home consume 1? CHEQUEAR

costoA(V,1):- V = agt_world_rep(_Pos,bal,_Cont),!.
costoA(V,1):- V = agt_world_rep(_Pos,home,_Cont),!.
costoA(V,2):- V = agt_world_rep(_Pos,alf,_Cont),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                         Distancia de Manhattan                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% disManhattan(+ Celda1, + Celda2, - Distancia)
%
% Retorna el valor de la distancia de Manhattan para dos celdas dadas del mapa

distManhattan(agt_world_rep([F,C],_,_),H):-
   is_goal(agt_world_rep([F2,C2],_,_)),
   DifFila is abs(F - F2),                          % averiguo diferencia absoluta entre filas.
   DifColumna is abs(C - C2),                       % averiguo diferencia absoluta entre columnas.
   H is DifFila + DifColumna.                       % distancia manhattan entre nodo actual y el nodo destino
   
