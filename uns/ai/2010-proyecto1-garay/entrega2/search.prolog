%--------------------------------------------------------------------------------------------------%
% node(Position, Path_to_Node, Action_List, Path_Cost, Last_Facing_Direction)
%
% La estructura node/5 mantiene un nodo del grafo de busqueda. 
% El 1er argumento es la posicion del mapa (el "estado" del nodo del grafo).
% El 2do argumento es el camino hacia la posicion desde donde se comenzo la busqueda, representado
% por una lista de posiciones. 
% El 3er argumento es la lista de acciones que el agente debe ejecutar desde la posicion donde se
% comenzo la busqueda para llegar a el. 
% El 4to argumento es el costo del camino, tomando en cuenta todas las acciones necesarias y el 
% terreno del camino recorrido.
% El 5to argumento es la ultima direccion en la cual queda mirando el agente, mantenida para poder
% calcular las acciones apropiadas para la lista de acciones.
                                                                                                                    
:- dynamic is_goal/1.

%--------------------------------------------------------------------------------------------------%
% multiple_search/4
% multiple_search(+Direction, +Position, +Destinations, -Paths)
%
% El 1er argumento es la direccion en la cual el agente esta mirando al comenzar la busqueda.
% El 2do argumento es la posicion desde la cual se comienza la busqueda.
% El 3er argumento es la lista de posiciones hacia las cuales se quieren calcular los caminos.
% El 4to argumento queda ligado a una lista de listas, donde cada elemento es el resultado de buscar
% un camino a uno de los destinos dados, respectivamente. 
% Cada solucion es una lista de tres elementos, conteniendo el camino hacia el destino, la secuencia
% de acciones necesarias para llegar a el representada como una lista, y el costo de ejecutar esas 
% acciones.
%
% multiple_search/4 nunca falla, si no hay un camino a ninguna posicion dada, devuelve la lista vacia.

multiple_search(        _,        _,                           [],                           []).
multiple_search(Direction, Position, [Destination | Destinations], [[Cost, Path, Plan] | Paths]) :-
    search(Direction, Position, Destination, Path, Plan, Cost),
    multiple_search(Direction, Position, Destinations, Paths).
multiple_search(Direction, Position, [_Destination | Destinations], Paths) :-
    multiple_search(Direction, Position, Destinations, Paths).

%--------------------------------------------------------------------------------------------------%
% search/6
% search(+Start_Direction, +Start_Position, +End_Position, -Path, -Actions,  -Cost)
%
% El 1er argumento es la posicion desde la cual se comienza la busqueda.
% El 2do argumento es la posicion a la cual se quiere llegar. 
% El 3er argumento queda ligado al camino solucion desde la posicion inicial a la meta. 
% El 4to argumento queda ligado al costo del camino.
%
% Si no hay camino, falla.

search(_Start_Direction, _Start_Position, End_Position, _Path, _Actions, _Cost) :-
    not(passable(End_Position)),
    !,
    fail. 

search(Start_Direction, Start_Position, End_Position, Path, Actions, Cost) :-
    assert(     is_goal(End_Position) ),
    astar_search(Start_Position, Start_Direction, Path, Actions, Cost),
    retractall( is_goal(_) ),
    !.

search(_Start_Direction, _Start_Position, _End_Position, _Path, _Actions, _Cost) :-
    retractall( is_goal(_) ),
    fail,
    !.
    
%--------------------------------------------------------------------------------------------------%
% astar_search/5 
% astar_search(+Start_Position, +Start_Direction, -Solution, -Actions, -Cost)
%
% El 1er argumento es la posicion desde donde se comienza la busqueda.
% El 2do argumento es la direccion en la cual el agente esta mirando al comenzar la busqueda.
% El 3er argumento queda ligado al camino solucion a la meta. 
% El 4to argumento queda ligado a la secuencia de acciones necesarias para llegar al destino.
% El 5to argumento queda ligado al costo del camino solucion. 
%
% Inicializamos la frontera con un nodo con la posicion inicial, un camino vacio hacia el nodo 
% inicial, una lista de acciones vacia para llegar hacia el nodo inicial, un costo nulo, y la 
% direccion inicial en la cual esta mirando el agente.

astar_search(Start_Position, Start_Direction, Solution, Actions, Cost) :-
    astar_search1([node(Start_Position, [], [], 0, Start_Direction)], [], Solution1, Actions1, Cost),
    reverse(Solution1, [Start_Position | Solution]),
    reverse(Actions1, Actions).

%--------------------------------------------------------------------------------------------------%
% astar_search/4
% astar_search(+Frontier, +Visited, -Path, -Actions, -Path_Cost)
%
% El 1er argumento es la frontera de busqueda. 
% El 2do argumento es la lista de nodos visitados.
% El 3er argumento es la solucion parcial.
% El 4to argumento es la secuencia de acciones necesarias para seguir el camino.
% El 5to argumento es el costo de ejecutar las acciones. 

astar_search1(Frontier, _Visited, [Position | Path], Actions, Path_Cost) :-
    astar_select(node(Position, Path, Actions, Path_Cost, _Last_Direction), Frontier, _Frontier1),
    is_goal(Position).

astar_search1(Frontier, Visited, Solution_Path, Solution_Actions, Cost) :-
    astar_select(Selected_Node, Frontier, Frontier1),
    neighbours(Selected_Node, Neighbours),
    add_paths(Neighbours, Selected_Node, Neighbours1),
    astar_add_to_frontier(Neighbours1, Frontier1, Frontier_New, Visited, Visited_New),
    astar_search1(Frontier_New, [Selected_Node | Visited_New], Solution_Path, Solution_Actions, Cost).

%--------------------------------------------------------------------------------------------------%
% astar_select/3
% astar_select(-Selected_Node, +Frontier, -New_Frontier)
%
% Selecciona un nodo de la frontera. 
%
% El 1er argumento queda ligado al nodo seleccionado de la frontera.
% El 2do argumento es la Frontera de la cual se elige el nodo. 
% El 3er argumento queda ligado a la frontera sin el nodo que fue seleccionado.

astar_select(Node, [Node | Frontier], Frontier).
                                                                                                                   
%--------------------------------------------------------------------------------------------------%
% neighbours/2
% neighbours(+Node, -Neighbours)
%
% Define a todos los vecinos validos del nodo dado.
% Los nodos validos son aquellos que corresponden a posicion con terreno pasable.
% 
% El 1er argumento es el nodo correspondiente a la posicion cuyos vecinos se calcularan.
% El 2do argumento es la lista de nodos correspondientes a posiciones vecinas a la del nodo dado.

neighbours(Node, Neighbours) :-
    Node = node(Position, Path, Actions, Path_Cost, Last_Direction),
    findall(
        node(Neighbour_Positions, [Position | Path], Actions, Path_Cost, Last_Direction),
        arc(Position, Neighbour_Positions),
        Neighbours
    ).

%--------------------------------------------------------------------------------------------------%
% add_paths/3
% add_paths(+Neighbours, +Frontier_Element, -New_Frontier_Elements)
%
% Prepara a los vecinos para ser agregados a la frontera, sumandole el costo del nodo actual y las 
% acciones necesarias para llegar al vecino al costo total del camino, y agregando la(s) acciones 
% necesarias para llegar al vecino.
% 
% El 1er argumento es la lista de nodos vecinos a preparar.
% El 2do argumento es el elemento de la frontera a partir del cual se generaron los nodos vecinos.
% El 3er argumento queda ligado a la lista de nodos vecinos preparados.

add_paths([], _Frontier_Element, []).
add_paths([node(Neighbour,        _,               _,             _,              _) | Rest_Of_Neighbours], 
           node( Position,     Path,     Action_List,     Path_Cost, Last_Direction), 
          [node(Neighbour, New_Path, New_Action_List, New_Path_Cost, Next_Direction) | Rest_Of_Frontier]) :-

    terrain_cost(Neighbour, Terrain_Cost),
    calculate_action(        Last_Direction, Position, Neighbour, Action        ),
    calculate_action_cost(   Last_Direction, Position, Neighbour, Action_Cost   ),
    calculate_next_direction(Last_Direction, Position, Neighbour, Next_Direction),
    
    append([Position], Path, New_Path),
    append(Action, Action_List, New_Action_List),
    New_Path_Cost is Path_Cost + Terrain_Cost + Action_Cost,
    add_paths(Rest_Of_Neighbours, node(Position, Path, Action_List, Path_Cost, Last_Direction), Rest_Of_Frontier).

%--------------------------------------------------------------------------------------------------%
calculate_action(n, [CR,C], [NR,C], [move_fwd])          :- CR > NR. 
calculate_action(D, [CR,C], [NR,C], [move_fwd, turn(n)]) :- D \= n, CR > NR. 
calculate_action(s, [CR,C], [NR,C], [move_fwd])          :- CR < NR. 
calculate_action(D, [CR,C], [NR,C], [move_fwd, turn(s)]) :- D \= s, CR < NR. 
calculate_action(e, [R,CC], [R,NC], [move_fwd])          :- CC < NC. 
calculate_action(D, [R,CC], [R,NC], [move_fwd, turn(e)]) :- D \= e, CC < NC. 
calculate_action(w, [R,CC], [R,NC], [move_fwd])          :- CC > NC. 
calculate_action(D, [R,CC], [R,NC], [move_fwd, turn(w)]) :- D \= w, CC > NC. 

%--------------------------------------------------------------------------------------------------%
calculate_action_cost(n, [CR,C], [NR,C], 1) :- CR > NR. 
calculate_action_cost(D, [CR,C], [NR,C], 2) :- D \= n, CR > NR. 
calculate_action_cost(s, [CR,C], [NR,C], 1) :- CR < NR. 
calculate_action_cost(D, [CR,C], [NR,C], 2) :- D \= s, CR < NR. 
calculate_action_cost(e, [R,CC], [R,NC], 1) :- CC < NC. 
calculate_action_cost(D, [R,CC], [R,NC], 2) :- D \= e, CC < NC. 
calculate_action_cost(w, [R,CC], [R,NC], 1) :- CC > NC. 
calculate_action_cost(D, [R,CC], [R,NC], 2) :- D \= w, CC > NC. 

%--------------------------------------------------------------------------------------------------%
calculate_next_direction(n, [CR,C], [NR,C], n) :- CR > NR. 
calculate_next_direction(D, [CR,C], [NR,C], n) :- D \= n, CR > NR. 
calculate_next_direction(s, [CR,C], [NR,C], s) :- CR < NR. 
calculate_next_direction(D, [CR,C], [NR,C], s) :- D \= s, CR < NR. 
calculate_next_direction(e, [R,CC], [R,NC], e) :- CC < NC. 
calculate_next_direction(D, [R,CC], [R,NC], e) :- D \= e, CC < NC. 
calculate_next_direction(w, [R,CC], [R,NC], w) :- CC > NC. 
calculate_next_direction(D, [R,CC], [R,NC], w) :- D \= w, CC > NC. 

%--------------------------------------------------------------------------------------------------%
% astar_add_to_frontier/5
% astar_add_to_frontier(Neighbours, Old_Frontier, New_Frontier, Old_Visited, New_Visited)

astar_add_to_frontier(Neighbours, Frontier_Old, Frontier_New, Visited_Old, Visited_New) :-
    Frontier1 = Frontier_Old,
    Visited1  = Visited_Old,
    checkall(Neighbours, Frontier1, Frontier2, Visited1, Visited2),
    sort_by_f(Frontier2, Frontier3),
    Frontier_New = Frontier3,
    Visited_New  = Visited2.

%--------------------------------------------------------------------------------------------------%
% checkall/5
% checkall(Neighbours, Old_Frontier, New_Frontier, Old_Visited, New_Visited)
% 
% Realiza un chequeo por cada vecino, y segun los resultados actualiza la Frontera y los Visitados.
%
% El 1er argumento es la lista de vecino a verificar.
% El 2do argumento es la frontera vieja.
% El 3er argumento es la frontera actualizada.
% El 4to argumento es la lista de nodos visitados vieja.
% El 5to argumento es la lista de visitados actualizada.

checkall(                      [], Frontier,  Frontier,  Visited,  Visited).

checkall([Neighbour | Neighbours], Frontier1, Frontier3, Visited1, Visited3) :-
    check(Neighbour, Frontier1, Frontier2, Visited1, Visited2),
    checkall(Neighbours, Frontier2, Frontier3, Visited2, Visited3).

%--------------------------------------------------------------------------------------------------%
% check/5
% check(+Node, Old_Frontier, New_Frontier, Old_Visited, New_Visited)
%
% Verifica si se da alguna de las siguientes condiciones para un nodo dado:
%
% (1) Si existe en F un nodo N etiquetado con una posicion P, y generamos P por un mejor camino que 
% el representado por N, % entonces reemplazamos N en F por un nodo N' para P con este nuevo camino.
% (2) Si existe en V un nodo N etiquetado con una posicion P, y generamos P por un mejor camino que 
% el representado por N, entonces N es eliminado de V y se agrega a la frontera un nuevo nodo N' 
% para P con este nuevo camino.

check(node(Pos, Path, Actions, Cost1, Last_Direction), Frontier_Old, Frontier_New, Visited_Old, Visited_Old) :-
    not(member(node(Pos, _, _, _, _), Frontier_Old)),
    not(member(node(Pos, _, _, _, _), Visited_Old)),
    append([node(Pos, Path, Actions, Cost1, Last_Direction)], Frontier_Old, Frontier_New).

check(node(Pos, Path, Actions, Cost1, Last_Direction), Frontier_Old, Frontier_New, Visited_Old, Visited_New) :-
    not(member(node(Pos, Path2, Actions2, Cost2, Last_Direction2), Frontier_Old)),
    member(node(Pos, _, _, Cost2, _), Visited_Old),
    (
        Cost1 < Cost2,
        delete(Visited_Old, node(Pos, Path2, Actions2, Cost2, Last_Direction2), Visited_New),
        append(node(Pos, Path, Actions, Cost1, Last_Direction), Frontier_Old, Frontier_New)
    ;
        Frontier_New = Frontier_Old,
        Visited_New = Visited_Old
    ).

    
check(node(Pos, Path, Actions, Cost1, Last_Direction), Frontier_Old, Frontier_New, Visited_Old, Visited_Old) :-
    member(node(Pos, Path2, Actions2, Cost2, Last_Direction2), Frontier_Old),
    not(member(node(Pos, _, _, _, _), Visited_Old)),
    (
        Cost1 < Cost2, 
        delete(Frontier_Old, node(Pos, Path2, Actions2, Cost2, Last_Direction2), Frontier1),
        append([node(Pos, Path, Actions, Cost1, Last_Direction)], Frontier1, Frontier_New)
    ;
        Frontier_New = Frontier_Old
    ).   

%--------------------------------------------------------------------------------------------------%
% sort_by_f/2
% sort_by_f(+Frontier, -Sorted_Frontier)
%
% Ordena los nodos de la frontera segun su valor f.

sort_by_f(Frontier1, Frontier2) :-
    fquicksort(Frontier1, Frontier2).

%--------------------------------------------------------------------------------------------------%
% quicksort/2

fquicksort([], []).
fquicksort([X|Xs], Ys) :-
    partition(Xs, X, Littles, Bigs),
    fquicksort(Littles, Ls),
    fquicksort(Bigs,    Bs),
    append(Ls, [X|Bs], Ys).

%--------------------------------------------------------------------------------------------------%
% partition/4

partition([], _Y, [], []).

partition([X|Xs], Y, [X|Ls], Bs) :-
    f(X, FX),
    f(Y, FY),
    FX =< FY,
    partition(Xs, Y, Ls, Bs).

partition([X|Xs], Y, Ls, [X|Bs]) :-
    f(X, FX),
    f(Y, FY),
    FX > FY,
    partition(Xs, Y, Ls, Bs).

%--------------------------------------------------------------------------------------------------%
% frontier_positions/2
% frontier_positions(+Frontier, -Frontier_Positions) 
%
% produces a list of Positions from a Frontier, stripping the Positions of the Path and Path_Cost
% components and the surrounding node/3 bookkeeping structure.

frontier_positions([], []).

frontier_positions([node(N, _P, _A, _PC, _LD) | RF], [N | RFP]) :-
    frontier_positions(RF, RFP).

%--------------------------------------------------------------------------------------------------%
% f/2
% f(Nodo, F)
% 
% Calcula el valor f correspondiente a un nodo del espacio de busqueda. 

f(node(Position, _Path, _Actions, Path_Cost, _Last_Direction), F) :-
    is_goal(Goal),
    heuristic(Position, Goal, H),
    F is Path_Cost + H.

%--------------------------------------------------------------------------------------------------%
% heuristic/3
% heuristic(+Posicion1, +Posicion2, -Heuristica)
%
% Calcula el valor heuristico correspondiente a una posicion dada (Posicion1). 
% La heuristica utilizada es la distancia Manhattan de la posicion al nodo meta.

heuristic([R1, C1], [R2, C2], H) :-
    H is (abs(R2 - R1) + abs(C2 - C1)).

%--------------------------------------------------------------------------------------------------%
% terrain_cost/2
% terrain_cost(?Posicion, ?Terrain_Cost)
%
% Determina el costo de pasar por el terreno correspondiente a una posicion.
%
% El 1er argumento es una posicion. 
% El 2do argumento queda ligado al costo correspondiente al terreno correspondiente a la posicion.

terrain_cost(Position, 1) :-
    agent_map(Position, plain, _, _).

terrain_cost(Position, 2) :-
    agent_map(Position, mountain, _, _).

%--------------------------------------------------------------------------------------------------%
% arc/2
% arc(+Posicion1, ?Posicion2)
%
% Determina si hay un arco en el espacio de busqueda de una posicion dada a otra.

% To the north.
arc([R1, C1], [R2, C1]) :-
    R2 is R1 - 1,
    passable([R2, C1]).

% To the east
arc([R1, C1], [R1, C2]) :-
    C2 is C1 + 1,
    passable([R1, C2]).

% To the south
arc([R1, C1], [R2, C1]) :-
    R2 is R1 + 1,
    passable([R2, C1]).

% to the west
arc([R1, C1], [R1, C2]) :-
    C2 is C1 - 1,
    passable([R1, C2]).

%--------------------------------------------------------------------------------------------------%
% passable/1
% passable(+Position)
%
% Determina si una posicion corresponde a un terreno pasable (i.e. montaña o planicie).

passable(Position) :-
    agent_map(Position, Terrain, _, _),
    (
        Terrain = mountain
        ;
        Terrain = plain
    ).

%--------------------------------------------------------------------------------------------------%
% Solo para testear el codigo de busqueda fuera del contexto de un agente.
agent_map(P, T, _C, _U) :-
    cell_land(P, T).

%--------------------------------------------------------------------------------------------------%
hill_climbing(Previous_Position, Start_Direction, Start_Position, End_Position, Action) :-
    findall(
        [H, P],
        (
            arc(Start_Position, P),
            P \= Previous_Position,
            heuristic(P, End_Position, H)
        ),
        Neighbours),
    sort(Neighbours, [[_, New_Position] | _]),
    calculate_action(Start_Direction, Start_Position, New_Position, Actions),
    reverse(Actions, [Action | _]).

%--------------------------------------------------------------------------------------------------%
adjacent([R1, C1], [R2, C1]) :- R2 is R1 - 1.
adjacent([R1, C1], [R1, C2]) :- C2 is C1 + 1.
adjacent([R1, C1], [R2, C1]) :- R2 is R1 + 1.
adjacent([R1, C1], [R1, C2]) :- C2 is C1 - 1.

%--------------------------------------------------------------------------------------------------%
% devuelve en position3 la posicion adyacente a position2 que este mas cercana segun la heuristica a position1.
nearest_adjacent(Position1, Position2, Position3) :-
    findall(
        [H, P],
        (
            adjacent(Position2, P),
            heuristic(Position1, P, H)
        ),
        Neighbours),
    sort(Neighbours, Sorted_Neighbours),
    [[_, Position3] | _] = Sorted_Neighbours.

