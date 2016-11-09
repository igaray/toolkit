%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utiles

%--------------------------------------------------------------------------------------------------%
replace(X, Y) :- retract(X), !, assert(Y).

%--------------------------------------------------------------------------------------------------%
replaceall(X, Y) :- retractall(X), !, assert(Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% State update auxiliar predicates

%--------------------------------------------------------------------------------------------------%
% extract_information_from_vision/9
% extract_information_from_vision(+Turn,
%                                 +Vision,
%                                 +Agents_old,
%                                 +Hostels_old,
%                                 +Treasures_old,
%                                 -Agents_new,
%                                 -Hostels_new,
%                                 -Treasures_new,
%                                 -Last_Action)
% 
% Dada la vision de una percepcion, el turno correspondiente a la percepcion, y las bases de datos 
% del agente de agentes conocidos, posadas conocidas y tesoros conocidos, liga el sexto, septimo
% y octavo argumentos con las bases de datos de agentes, posadas y tesoros actualizadas segun la 
% informacion presente en la percepcion, y liga el ultimo argumento a la ultima accion realizada 
% por el agente.
%
% Por cada elemento de la vision, que consiste en la lista de tres elementos [Pos, Land, Things],
% primero actualiza el mapa del mundo del agente, luego elimina los tesoros conocidos que por
% alguna razon ya no se ven (las posadas no deben ser eliminadas porque no desaparecen) y por
% ultimo llama al predicado extract_information_from_cell/9 que realiza la extraccion de la 
% informacion de esa celda en particular, segun sea el caso de que objeto se encuentra en la celda.

% Si la Vision es una lista vacia, no hay nada que extraer. 
extract_information_from_vision(_Turn, 
                                [],                             
                                _Last_Action,
                                Agents1, Hostels1, Treasures1, 
                                Agents1, Hostels1, Treasures1). 

% Si hay elementos en la Vision, por cada uno extraer la informacion apropiada. 
extract_information_from_vision(Turn,  
                                [[Pos, Land, Things] | Vision], 
                                Last_Action,
                                AG1, HS1, TR1, 
                                AG3, HS3, TR4) :-  
    update_agent_map(Pos, Land, Things, Turn),
    remove_missing_treasures(TR1, Pos, TR2),
    extract_information_from_cell(  Turn, Pos, Things, Last_Action, AG1, HS1, TR2, AG2, HS2, TR3),
    extract_information_from_vision(Turn, Vision, Last_Action, AG2, HS2, TR3, AG3, HS3, TR4).

%--------------------------------------------------------------------------------------------------%
% calcular los vecinos de la celda y si no tienen terreno asignado, ponerlos en el mapa como unknown
update_agent_map(Position, Land, Things, Turn) :-
    replaceall(
        agent_map(Position, _, _, _), 
        agent_map(Position, Land, Things, Turn)
    ),
    add_neighbour_n(Position, Turn),
    add_neighbour_e(Position, Turn),
    add_neighbour_s(Position, Turn),
    add_neighbour_w(Position, Turn).

%--------------------------------------------------------------------------------------------------%
add_neighbour_n([R, C], Turn) :-
    Rn is R - 1,
    Cn is C,     
    % no existe agent map para esa posicion
    not(agent_map([Rn, Cn], _, _, _)), 
    replaceall(
        agent_map([Rn, Cn], _, _, _), 
        agent_map([Rn, Cn], unknown, [], Turn)
    ).
add_neighbour_n([_, _], _).

%--------------------------------------------------------------------------------------------------%
add_neighbour_e([R, C], Turn) :-
    Re is R,     
    Ce is C + 1, 
    not(agent_map([Re, Ce], _, _, _)), 
    replaceall(
        agent_map([Re, Ce], _, _, _), 
        agent_map([Re, Ce], unknown, [], Turn)
    ).
add_neighbour_e([_, _], _).

%--------------------------------------------------------------------------------------------------%
add_neighbour_s([R, C], Turn) :-
    Rs is R + 1, 
    Cs is C,     
    not(agent_map([Rs, Cs], _, _, _)), 
    replaceall(
        agent_map([Rs, Cs], _, _, _), 
        agent_map([Rs, Cs], unknown, [], Turn)
    ).
add_neighbour_s([_, _], _).

%--------------------------------------------------------------------------------------------------%
add_neighbour_w([R, C], Turn) :-
    Rw is R,     
    Cw is C - 1, 
    not(agent_map([Rw, Cw], _, _, _)), 
    replaceall(
        agent_map([Rw, Cw], _, _, _), 
        agent_map([Rw, Cw], unknown, [], Turn)
    ).
add_neighbour_w([_, _], _).

%--------------------------------------------------------------------------------------------------%
% remove_missing_treasures/3
% remove_missing_treasures(+Treasures_Old, +Position, -Treasures_New)
% 
% Si no hay un tesoro en la posicion, eliminarlo de la base de datos de tesoros.
% El primer argumento debe ser la lista de posiciones de los tesoros conocidos. 
% El segundo argumento debe ser la posicion a eliminar.
% El tercer argumento es de salida, y es ligado a la lista actualizada.
remove_missing_treasures(Treasures_old, Position, Treasures_new) :-
    assoc_to_list(Treasures_old, Treasures_old_list),
    member(T-[_, [pos, Position] | _], Treasures_old_list),
    delete(Treasures_old_list, T-[_, [pos, Position] | _], Treasures_new_list),
    list_to_assoc(Treasures_new_list, Treasures_new).
remove_missing_treasures(Treasures, _, Treasures).

%--------------------------------------------------------------------------------------------------%
% extract_information_from_cell/10
% extract_information_from_cell(Turn,
%                               Position
%                               Things_at_position,
%                               Last_action
%                               Agents_old, Hostels_old, Treasures_old,
%                               Agents_new, Hostels_new, Treasures_new,
%                               )
%
% Revisa los objetos encontrados en una celda, y calcula las nuevas listas del estado del agente.

% Si la lista de objetos en la posicion es vacia, no hay nada que agregegar.
extract_information_from_cell(_Turn, 
                              _Pos, 
                              [], 
                              _Last_Action,
                              Agents1, Hostels1, Treasures1, 
                              Agents1, Hostels1, Treasures1).

% Hay un agente en la celda y soy yo, recuperar y devolver la ultima accion.
extract_information_from_cell(Turn,
                              Pos,  
                              [[agent, ThingName, Description] | Things], 
                              Last_Action, 
                              Agents1, Hostels1, Treasures1, 
                              Agents3, Hostels3, Treasures3) :- 
    ag_name(My_Name),
    My_Name = ThingName,
    member([previous_turn_action, Last_Action], Description), 
    extract_information_from_cell(Turn, 
                                  Pos, 
                                  Things, 
                                  Last_Action, 
                                  Agents1, Hostels1, Treasures1, 
                                  Agents3, Hostels3, Treasures3).

% Hay un agente en la celda y no soy yo, actualizar la lista de agentes.
extract_information_from_cell(Turn,
                              Pos, 
                              [[agent, ThingName, Description] | Things], 
                              Last_Action, 
                              Agents1, Hostels1, Treasures1, 
                              Agents3, Hostels3, Treasures3) :- 
    update_agents(Agents1, Agents2, ThingName, Pos, Description, Turn),
    extract_information_from_cell(Turn,
                                  Pos, 
                                  Things, 
                                  Last_Action, 
                                  Agents2, Hostels1, Treasures1, 
                                  Agents3, Hostels3, Treasures3).

% Hay una posada en la celda, actualizar la lista de posadas.
extract_information_from_cell(Turn,
                              Pos, 
                              [[hostel, _ThingName, _Description] | Things], 
                              Last_Action, 
                              Agents1, Hostels1, Treasures1, 
                              Agents3, Hostels3, Treasures3) :- 
    update_inns(Turn, Hostels1, Hostels2, Pos),
    extract_information_from_cell(Turn,
                                  Pos, 
                                  Things, 
                                  Last_Action, 
                                  Agents1, Hostels2, Treasures1, 
                                  Agents3, Hostels3, Treasures3).

% Hay un tesoro en la celda, actualizar la lista de tesoros.
extract_information_from_cell(Turn,
                              Pos, 
                              [[treasure, ThingName, Description] | Things], 
                              Last_Action, 
                              Agents1, Hostels1, Treasures1, 
                              Agents3, Hostels3, Treasures3) :- 
    % Hay un tesoro en Pos, actualizar la lista de tesoros.
    update_treasures(Treasures1, Treasures2, ThingName, Turn, Pos, Description),
    extract_information_from_cell(Turn,
                                  Pos, 
                                  Things, 
                                  Last_Action, 
                                  Agents1, Hostels1, Treasures2, 
                                  Agents3, Hostels3, Treasures3).

%--------------------------------------------------------------------------------------------------%
% update_agents/5
% update_agents(+Agents_old, -Agents_new, +Agent_Name, +Agent_Description, +Turn)
%
% Actualiza la base de datos de otros agentes. 
% El primer argumento es la base de datos de agentes actual.
% El segundo argumento sera ligado a la base de datos actualizada con la informacion provista.
% El tercer argumento es el nombre del agente avistado.
% El cuarto argumento es la descripcion del agente avistado.
% El quinto argumento es el turno en que fue avistado por ultima vez el agente.

update_agents(Agents_Old, Agents_New, Agent_Name, Position, Agent_Description, Turn) :-
    get_assoc(Agent_Name, Agents_Old, Agent_Info_Old),
    update_agent(Agent_Info_Old, Agent_Info_New, Position, Agent_Description, Turn),
    put_assoc(Agent_Name, Agents_Old, Agent_Info_New, Agents_New).

% fallo get assoc porque no estaba el agente en la base de datos
update_agents(Agents_Old, Agents_New, Agent_Name, Position, Agent_Description, Turn) :-
    empty_assoc(Agent_Info_Old),
    update_agent(Agent_Info_Old, Agent_Info_New, Position, Agent_Description, Turn),
    put_assoc(Agent_Name, Agents_Old, Agent_Info_New, Agents_New).

%--------------------------------------------------------------------------------------------------%
update_agent(Agent_Info_Old, Agent_Info_New, Agent_Position, Agent_Description, Turn) :-
    put_assoc(last_seen, Agent_Info_Old, Turn,           Agent_Info1),
    put_assoc(pos,       Agent_Info1,    Agent_Position, Agent_Info2),
    put_assoc(wealth,    Agent_Info2,    poor,           Agent_Info3),
    extract_agent_info(Turn, Agent_Info3, Agent_Info4, Agent_Description),
    update_agent_wealth(Agent_Info4, Agent_Info_New).

%--------------------------------------------------------------------------------------------------%
% extract_agent_info/4
% extract_agent_info(+Turn, +Agent_info_old, -Agent_info_new, +Agent_description)
% 
% Recorre la lista de descripcion del agente para extraer y analizar la informacion de la 
% descripcion de un agente en la percepcion. 
% Inserta cada atributo en la descripcion en la lista asociativa que contiene la informacion acerca
% de un agente. 
% En los casos en que se percibe que el agente es atacado o lastimado por otro agente, se guarda
% tambien el turno en que fue atacado/lastimado, de manera que se pueda distinguir si el evento
% sucedio hace varios turnos o no. 

extract_agent_info(_Turn, Agent_info, Agent_info, []).

extract_agent_info(Turn, AI1, AI3, [[dir, D]                  | Agent_description]) :-
    put_assoc(dir, AI1, D, AI2),
    extract_agent_info(Turn, AI2, AI3, Agent_description).

extract_agent_info(Turn, AI1, AI3, [[unconscious, U]          | Agent_description]) :-
    put_assoc(unconscious, AI1, U, AI2),
    extract_agent_info(Turn, AI2, AI3, Agent_description).
    
extract_agent_info(Turn, AI1, AI3, [[previous_turn_action, A] | Agent_description]) :-
    PT is Turn - 1,
    get_assoc(action_history, AI1, Action_history),
    put_assoc(action_history, AI1, [[A, PT] | Action_history], AI2),
    extract_agent_info(Turn, AI2, AI3, Agent_description).

extract_agent_info(Turn, AI1, AI3, [[previous_turn_action, A] | Agent_description]) :-
    PT is Turn - 1,
    put_assoc(action_history, AI1, [[A, PT]], AI2),
    extract_agent_info(Turn, AI2, AI3, Agent_description).

extract_agent_info(Turn, AI1, AI3, [[attacked_by, A]          | Agent_description]) :-
    put_assoc(attacked_by, AI1, [A, Turn], AI2),
    extract_agent_info(Turn, AI2, AI3, Agent_description).

extract_agent_info(Turn, AI1, AI3, [[harmed_by, A]            | Agent_description]) :-
    put_assoc(harmed_by, AI1, [A, Turn], AI2),
    extract_agent_info(Turn, AI2, AI3, Agent_description).

%--------------------------------------------------------------------------------------------------%
% ver si la ultima accion fue pickup, si lo fue, marcarlo como rico
% si esta inconcsciente, marcarlo como pobre
update_agent_wealth(Agent_Info_Old, Agent_Info_New) :-
    get_assoc(last_action, Agent_Info_Old, pickup(_)),
    put_assoc(wealth, Agent_Info_Old, rich, Agent_Info_New).

update_agent_wealth(Agent_Info_Old, Agent_Info_New) :-
    get_assoc(unconscious, Agent_Info_Old, true),
    put_assoc(wealth, Agent_Info_Old, poor, Agent_Info_New).

% Si no cae en ninguno de los casos anteriores, no modificar los datos del agente. 
update_agent_wealth(X, X).

%--------------------------------------------------------------------------------------------------%
% update_inns/3
% update_inns(+Hostels_Old, -Hostels_New, +Pos)
%
% Actualiza la lista de posadas conocidas. 
% El primer argumento debe ser la lista actual de posadas conocidas por el agente. 
% El segundo argumento sera unificado con la lista actualizada. Si la posada ya era conocida, sera
% la misma lista. 
% El tercer argumento debe ser la posicion de la posada.
update_inns(Turn, Hostels_old, Hostels_new, Pos) :- 
    put_assoc(Pos, Hostels_old, Turn, Hostels_new).

%--------------------------------------------------------------------------------------------------%
% update_treasures/3
% update_treasures(+Treasures_old, -Treasures_new, +Pos)
% 
% Actualiza la lista de tesoros avistados pero no recolectados por el agente. 
% El primer argument debe ser la lista actual de tesoros conocidos por el agente. 
% El segundo argumento sera unificado con la lista actualizada. Si el tesoro ya habia sido visto
% por el agente y se encuentra en la lista, entonces el resultado sera identico al primer argumento.
% El tercer argumento debe ser la posicion del tesoro avistado.
% La base de datos de tesoros consiste de un arreglo asociativo cuyas claves son los nombre de los
% tesoros y cuyos valores son las lista de la forma:
% [[turn, T], [pos, P], [val, V]]
update_treasures(Treasures_old, Treasures_new, Treasure_name, Turn, Pos, Treasure_description) :- 
    append([[turn, Turn], [pos, Pos]], Treasure_description, Treasure_info),
    put_assoc(Treasure_name, Treasures_old, Treasure_info, Treasures_new).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Action determination auxiliary predicates.

%--------------------------------------------------------------------------------------------------%
buscar_celdas_inexploradas(My_Position, Unexplored) :-
    findall(
        [Heuristic, P],
        (
            agent_map([R, C], unknown, _, Turn),
            passable(P),
            heuristic(My_Position, [R, C], Manhattan),
            One_Over_Age is 10 / Turn,
            Heuristic is Manhattan + One_Over_Age
        ), 
        Positions
    ), 
    sort(Positions, Unexplored).

%--------------------------------------------------------------------------------------------------%    
buscame_un_camino_para_explorar(_Direction, _Agent_Position, [], [move_fwd]).
buscame_un_camino_para_explorar(Direction, Agent_Position, [[_,Posicion] | _Cola], Plan) :-
    search(Direction, Agent_Position, Posicion, _Path, Plan, Cost),
    Cost > 0.    
buscame_un_camino_para_explorar(Direction, Agent_Position, [[_, _Posicion] | Cola], Plan) :-
    buscame_un_camino_para_explorar(Direction, Agent_Position, Cola, Plan).

%--------------------------------------------------------------------------------------------------%    
agent_previous_position(Position) :-
    agent_state(_, [[_, _, Attributes, _], none], _, _, _, _, _, _, _),
    member([pos, Position], Attributes).
agent_previous_position(Position) :-
    agent_state(_, [_, [_, _, Attributes, _] | _], _, _, _, _, _, _, _),
    member([pos, Position], Attributes).

%--------------------------------------------------------------------------------------------------%    
agent_in_hostel :-
    agent_position(Position),
    agent_map(Position, _Terrain, Contents, _Turn),
    member([hostel, _, _], Contents).

%--------------------------------------------------------------------------------------------------%    
treasure_at_current_position(Treasure_Name) :-
    agent_position(Position),
    agent_map(Position, _Terrain, Contents, _Turn),
    member([treasure, Treasure_Name, _], Contents).

%--------------------------------------------------------------------------------------------------%
% calculate_path_to_nearest_hostel(Path, Actions, Cost)
% devuelve el camino mas corto al hostel mas cercano,
% la lista de acciones que hay que realizar para seguir el camino,
% y el costo del camino
% si no hay hostels devuelve una lista vacia como el min path, min actions y el costo es 0
calculate_path_to_nearest_hostel(Min_Path, Min_Actions, Min_Cost) :-
    agent_position(Position),
    agent_direction(Direction),
    agent_known_hostels(Hostels),
    assoc_to_keys(Hostels, Hostel_Positions_List),
    % Para cada posicion calcular el costo del camino, el camino, y las acciones necesarias para 
    % seguirla. . 
    findall(
        [Cost, Path, Actions],
        (                                                           
            member(Hostel, Hostel_Positions_List),
            search(Direction, Position, Hostel, Path, Actions, Cost)
        ),
        Paths
    ),
    get_shortest_path(Paths, Min_Path, Min_Actions, Min_Cost).

%--------------------------------------------------------------------------------------------------%
get_shortest_path(   [],       [],          [],        0).
get_shortest_path(Paths, Min_Path, Min_Actions, Min_Cost) :-
    sort(Paths, [[Min_Cost, Min_Path, Min_Actions] | _]).

%--------------------------------------------------------------------------------------------------%
calculate_path_to_nearest_treasure(Min_Path, Min_Actions, Min_Cost) :-
    % gtrace,
	agent_position(Position),
    agent_direction(Direction),
    % Obtener las posiciones de los tesoros.
    agent_known_treasures(Treasures),
    assoc_to_values(Treasures, Treasure_Info_List),
    findall(
        [Heuristica,P],
        (
            member(Treasure_Info, Treasure_Info_List),
            member([pos, P], Treasure_Info),
			heuristic(Position, P, Heuristica)
        ), 
        Treasure_Positions_List
    ),
	sort(Treasure_Positions_List, Tesorosos),
	(
		Tesorosos=[[_,A],[_,B],[_,C] | _],
		A_Buscar=[A,B,C]
	;
		Tesorosos=[[_,A],[_,B]| _],
		A_Buscar=[A,B]
	;
		Tesorosos=[[_,A]| _],
		A_Buscar=[A]
	;
		Tesorosos=[],
		A_Buscar=[]
	),
    % Para cada tesoro calcular la distancia. 
    multiple_search(Direction, Position, A_Buscar, Paths),
    (
        % Si no hay ningun camino hacia un tesoro, el camino y la lista de acciones es vacia, y el costo es 0.
        Paths       = [],
        Min_Path    = [],
        Min_Actions = [],
        Min_Cost    = 0
        ;
        sort(Paths, Sorted_Paths),
        [[Min_Cost, Min_Path, Min_Actions] | _] = Sorted_Paths
    ).

%--------------------------------------------------------------------------------------------------%    
% siempre tiene exito y siempre devuelve un camino porque solo se llama para encontrar a agentes en el rango visual
calculate_path_to_agent(Agent_Name, Path, Actions, Cost) :-
    agent_position(Position),
    agent_direction(Direction),
    agent_known_agents(Agents),
    get_assoc(Agent_Name, Agents, Agent_Info),
    get_assoc(pos, Agent_Info, Agent_Position),
    search(Direction, Position, Agent_Position, Path, Actions, Cost).

%--------------------------------------------------------------------------------------------------%    
agent_under_attack(Attacking_Agent_Name) :-
    % se asegura que solo este bajo ataque este turno
    % recuperarndo el turno actual y haciendo que ligue con Turn
    ag_name(Agent_Name),
    agent_current_turn(Current_Turn),
    agent_position(Position),
    agent_map(Position, _Terrain, Contents, Current_Turn),
    member([agent, Agent_Name, Description], Contents),
    member([attacked_by, [Attacking_Agent_Name | _]], Description).

%--------------------------------------------------------------------------------------------------%    
agent_in_attack_range(Agent_Name) :-
    agent_current_turn(Current_Turn),
    agent_position(Position),
    agent_direction(Direction),
    attackable_position(Position, Direction, Adjacent_Position),
    agent_map(Adjacent_Position, _Terrain, Contents, Current_Turn),
    member([agent, Agent_Name, _], Contents).

%--------------------------------------------------------------------------------------------------%    
agent_in_visual_range(Agent_Name) :-
    agent_current_turn(Current_Turn),
    agent_position(Position),
    agent_direction(Direction),
    visible_position(Position, Direction, Visual_Position),
    agent_map(Visual_Position, _Terrain, Contents, Current_Turn),
    member([agent, Agent_Name, _], Contents).

%--------------------------------------------------------------------------------------------------%    
pending_treasures :-
    agent_known_treasures(Treasures),
    not(empty_assoc(Treasures)).

%--------------------------------------------------------------------------------------------------%    
rich_agent_in_sight(Agent_Name) :-
    agent_position(Position),
    agent_direction(Direction),
    visible_position(Position, Direction, Visible_Position),
    agent_map(Visible_Position, _, Things, _),
    member([agent, Agent_Name | _], Things),
    agent_known_agents(Agents),
    get_assoc(Agent_Name, Agents, Agent_Info),
    get_assoc(wealth, Agent_Info, rich).

%--------------------------------------------------------------------------------------------------%    
attackable_position([R, C], _, [AR, AC]) :- AR is R,     AC is C.

attackable_position([R, C], n, [AR, AC]) :- AR is R - 1, AC is C - 1.
attackable_position([R, C], n, [AR, AC]) :- AR is R - 1, AC is C.
attackable_position([R, C], n, [AR, AC]) :- AR is R - 1, AC is C + 1.

attackable_position([R, C], e, [AR, AC]) :- AR is R - 1, AC is C + 1.
attackable_position([R, C], e, [AR, AC]) :- AR is R,     AC is C + 1.
attackable_position([R, C], e, [AR, AC]) :- AR is R + 1, AC is C + 1.

attackable_position([R, C], s, [AR, AC]) :- AR is R + 1, AC is C + 1.
attackable_position([R, C], s, [AR, AC]) :- AR is R + 1, AC is C.
attackable_position([R, C], s, [AR, AC]) :- AR is R + 1, AC is C - 1.

attackable_position([R, C], w, [AR, AC]) :- AR is R + 1, AC is C - 1.
attackable_position([R, C], w, [AR, AC]) :- AR is R,     AC is C - 1.
attackable_position([R, C], w, [AR, AC]) :- AR is R - 1, AC is C - 1.

visible_position([R, C], n, [VR, VC]) :- VR is R - 3, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 3, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 3, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 3, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 3, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 2, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 2, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 2, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 2, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 2, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 1, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 1, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 1, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 1, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R - 1, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R    , VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R    , VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R    , VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], n, [VR, VC]) :- VR is R    , VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.

visible_position([R, C], e, [VR, VC]) :- VR is R - 2, VC is C + 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R - 1, VC is C + 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C + 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 1, VC is C + 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 2, VC is C + 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R - 2, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R - 1, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 1, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 2, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R - 2, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R - 1, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 1, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 2, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R - 2, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R - 1, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 1, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 2, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.

visible_position([R, C], e, [VR, VC]) :- VR is R + 3, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 3, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 3, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 3, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 3, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 2, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 2, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 2, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 2, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 2, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 1, VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 1, VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 1, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 1, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R + 1, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C + 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C + 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], e, [VR, VC]) :- VR is R    , VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.

visible_position([R, C], w, [VR, VC]) :- VR is R + 2, VC is C - 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R + 1, VC is C - 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R    , VC is C - 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R - 1, VC is C - 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R - 2, VC is C - 3, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R + 2, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R + 1, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R    , VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R - 1, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R - 2, VC is C - 2, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R + 2, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R + 1, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R    , VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R - 1, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R - 2, VC is C - 1, agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R + 2, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R + 1, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R    , VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R - 1, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.
visible_position([R, C], w, [VR, VC]) :- VR is R - 2, VC is C    , agent_map([VR, VC], _, T, _), T \= unknown.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Impresion del estado del agente.

%--------------------------------------------------------------------------------------------------%
display_agent_state :-
    ag_name(Name),
    agent_state(Current_Goal, 
                [[Turn, _Vision, _Attributes, Inventory] | _Percept_History], 
                [Last_Action | _Action_History], 
                Agents, 
                Hostels, 
                Treasures,
                Path, 
                Plan, 
                Cost),
    assoc_to_list(Agents,    AG), 
    assoc_to_list(Hostels,   HS),      
    assoc_to_list(Treasures, TR), 
    write('################################################################################'), nl, 
    write('Current turn:      '), write(Turn),                      nl,
    write('Agent name:        '), write(Name),                      nl,
    write('Current goal:      '), write(Current_Goal),              nl,
    write('Last action:       '), write(Last_Action),               nl,
    write('Known agents:      '), write(AG),                        nl,
    write('Hostels:           '), write(HS),                        nl,
    write('Treasures:         '), write(TR),                        nl,
    write('Inventory:         '), write(Inventory),                 nl,
    write('Current path:      '), write(Path),                      nl,
    write('Current plan:      '), write(Plan),                      nl,
    write('Current path cost: '), write(Cost),                      nl,
    write('Agent map: (# forest, ^ mountain, - plain, ~ water, ? unknown'), nl,
    nl,
    print_agent_map, nl.

%--------------------------------------------------------------------------------------------------%
print_agent_map :- 
    get_min_and_max_row(Min_Row, Max_Row),
    get_min_and_max_column(Min_Column, Max_Column),
    write('  '),
    print_head_columns(Min_Column, Max_Column),
    print_all_map(Min_Row, Max_Row, Min_Column, Max_Column).
    
%--------------------------------------------------------------------------------------------------%
print_head_columns(Max_Column, Max_Column):-
    Column_to_write is Max_Column mod 10,
    write(Column_to_write), nl.

print_head_columns(Min_Column, Max_Column):-
    Column_to_write is Min_Column mod 10,
    write(Column_to_write),
    Next_Column is Min_Column +1,
    print_head_columns(Next_Column, Max_Column).
    
%--------------------------------------------------------------------------------------------------%
get_min_and_max_row(Min_Row,Max_Row) :- 
    findall(R, agent_map([R, _C], _Terrain, _Contents, _Turn), List_Of_Rows),
    min_list(List_Of_Rows,Min_Row),
    max_list(List_Of_Rows,Max_Row).
get_min_and_max_row(1, 1).
    
%--------------------------------------------------------------------------------------------------%
get_min_and_max_column(Min_Column,Max_Column) :- 
    findall(C, agent_map([_R, C], _Terrain, _Contents, _Turn), List_Of_Columns),
    min_list(List_Of_Columns,Min_Column),
    max_list(List_Of_Columns,Max_Column).
get_min_and_max_column(1, 1).

%--------------------------------------------------------------------------------------------------%
print_all_map(Max_Row, Max_Row, Column_Iterator, Max_Column) :-
    Row_to_write is Max_Row mod 10,
    write(Row_to_write), 
    write(' '), 
    print_row(Max_Row, Column_Iterator, Max_Column), nl.

print_all_map(Row_Iterator, Max_Row, Column_Iterator, Max_Column) :-
    Row_to_write is Row_Iterator mod 10,
    write(Row_to_write), 
    write(' '), 
    print_row(Row_Iterator, Column_Iterator, Max_Column),
    Next_Row_Iterator is Row_Iterator + 1,
    nl,
    print_all_map(Next_Row_Iterator, Max_Row, Column_Iterator, Max_Column).
    
%--------------------------------------------------------------------------------------------------%
print_row(Actual_Row, End_Column, End_Column) :-
    agent_map([Actual_Row, End_Column], Terrain, _Things, _Turn),
    (
        Terrain = forest  , write('#')
    ;
        Terrain = plain   , write('-')
    ;
        Terrain = mountain, write('^')
    ;
        Terrain = water   , write('~')
    ;
        Terrain = unknown , write('?')
    ).

print_row(Actual_Row, Actual_Column, End_Column) :-
    agent_map([Actual_Row, Actual_Column], Terrain, _Things, _Turn),
    (
        Terrain = forest  , write('#')
    ;
        Terrain = plain   , write('-')
    ;
        Terrain = mountain, write('^')
    ;
        Terrain = water   , write('~')
    ;
        Terrain = unknown , write('?')
    ),
    Next_Column is Actual_Column +1,
    print_row(Actual_Row, Next_Column, End_Column).

print_row(_Actual_Row, End_Column, End_Column) :-
    write(' ').

print_row(Actual_Row, Actual_Column, End_Column) :-
    write(' '),
    Next_Column is Actual_Column +1,
    print_row(Actual_Row, Next_Column, End_Column).

