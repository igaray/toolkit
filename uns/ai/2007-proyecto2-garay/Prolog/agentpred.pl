% agentpred.pl: Predicates common to both agents

% AGENT'S INTERNAL STATE
:- dynamic mapa/3.               % The agent's map.
:- dynamic camouflage/1.         % camouflage(on) indicates the agent has the
                                 % enemy's attire donned, camouflage(off)
                                 % indicates otherwise.
:- dynamic path/3.               % 1st argument: the path itself, list of
                                 % positions to reach the goal
                                 % 2nd argument: cost of the path
                                 % 3rd argument: list of frontiers, the i-th
                                 % frontier corresponds to the i-th position in
                                 % the path

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% choose_action(+Agent, +Percept, -Action)
% chooses the corresponding action for each agent

% initial action, search for a path to the castle
choose_action(Agent, init, buscando) :-
    agent_position(Position),
    search(Agent,Position, Solution, Cost, Frontier_List),
    replace(path(_,_,_), path(Solution, Cost, Frontier_List)).

% if the simulation terminated, do nothing.
choose_action(_Agent, fin_simulacion(_Razon), none).

% if an enemy army is encountered, put on the camouflage and dont consume the next position
choose_action(_Agent, Percept, poner_disfraz) :-
    path([Next_Position|_], _Cost, _Frontier_List),
    enemy_army_present(Next_Position, Percept),
    camouflage(off),
    replace(camouflage(off), camouflage(on)).

% if an ardos enemy army is encountered, take off the camouflage and dont consume the next position
choose_action(_Agent, Percept, quitar_disfraz) :-
    path([Next_Position|_], _Cost, _Frontier_List),
    ardos_army_present(Next_Position, Percept),
    camouflage(on),
    replace(camouflage(on), camouflage(off)).

% if a broken bridge is encountered along the path, begin a new search and follow the new path
choose_action(Agent, Percept, buscando) :-
    path([Next_Position|_], _Cost, _Frontier_List),
    broken_bridge(Next_Position, Percept),

    % Update map
    replace(mapa(Next_Position, agua, puente(sano)), mapa(Next_Position, agua, -)),

    % Perform new search
    agent_position(Current_Position),
    search(Agent, Current_Position, New_Path, New_Cost, New_Frontier_List),
    replace(path(_,_,_), path(New_Path, New_Cost, New_Frontier_List)).

% if the search returned an empty path, abandon
choose_action(_Agent, _Percept, abandonar) :-
    path([], 0, []).

% if nothing special is encountered, advance along the path
choose_action(_Agent, Percept, avanzar(Direction, Speed)) :-
    path([Next_Position|Remaining_Path], Cost, [_|Remaining_Frontier_List]),
    choose_direction(Next_Position, Percept, Direction),
    choose_speed(Next_Position, Percept, Speed),
    replace(path(_,_,_), path(Remaining_Path, Cost, Remaining_Frontier_List)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% performs the corresponding search for each agent
search(dfs, Position, Solution, Cost, Frontier_List) :-
     dfs_search(Position, Solution, Cost, Frontier_List).
search(astar, Position, Solution, Cost, Frontier_List) :-
     astar_search(Position, Solution, Cost, Frontier_List).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% True if there is an enemy army present in the next position
enemy_army_present(Next_Position, Percept) :-
    get_next_position_percept(Next_Position, Percept, [Next_Position, _, ejercito(enemigo)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% True if there is an ardos army present in the next position
ardos_army_present(Next_Position, Percept) :-
    get_next_position_percept(Next_Position, Percept, [Next_Position, _, ejercito(reino)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compares the position in the path with the percept to decide in which direction to advance
choose_direction(P, perc(_,[P,_,_],[_,_,_],[_,_,_],[_,_,_]), norte).
choose_direction(P, perc(_,[_,_,_],[P,_,_],[_,_,_],[_,_,_]), sur).
choose_direction(P, perc(_,[_,_,_],[_,_,_],[P,_,_],[_,_,_]), este).
choose_direction(P, perc(_,[_,_,_],[_,_,_],[_,_,_],[P,_,_]), oeste).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chooses the speed with which to advance upon the next position
choose_speed(Next_Position, Percept, Speed) :-
   get_next_position_percept(Next_Position, Percept, [Next_Position, Terrain, Contents]),
   get_speed(Terrain, Contents, Speed).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determines speed according to terrain and contents
get_speed(mont,  _Contens,     a_paso_hombre).
get_speed(agua,  puente(sano), al_trote).
get_speed(agua,  puente(det),  a_paso_hombre).
get_speed(pasto, _Contents,    al_galope).
get_speed(_,     _,            a_paso_hombre). % just in case.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determines whether a bridge is broken in the next position
broken_bridge(Next_Position, Percept) :-
    get_next_position_percept(Next_Position, Percept, [Next_Position, agua, puente(roto)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets the percept component corresponding to the next position
% NP: north position, NT: north terrain, NC: north contents
% SP: south position, ST: south terrain, SC: south contents
% EP: east position,  ET: east terrain,  EC: east contents
% WP: west position,  WT: west terrain,  WC: west contents
get_next_position_percept(NP, perc(_Pos,[ NP, NT, NC],[_SP,_ST,_SC],[_EP,_ET,_EC],[_WP,_WT,_WC]), [NP, NT, NC]).
get_next_position_percept(SP, perc(_Pos,[_NP,_NT,_NC],[ SP, ST, SC],[_EP,_ET,_EC],[_WP,_WT,_WC]), [SP, ST, SC]).
get_next_position_percept(EP, perc(_Pos,[_NP,_NT,_NC],[_SP,_ST,_SC],[ EP, ET, EC],[_WP,_WT,_WC]), [EP, ET, EC]).
get_next_position_percept(WP, perc(_Pos,[_NP,_NT,_NC],[_SP,_ST,_SC],[_EP,_ET,_EC],[ WP, WT, WC]), [WP, WT, WC]).

