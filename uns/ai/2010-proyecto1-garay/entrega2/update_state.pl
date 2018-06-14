%--------------------------------------------------------------------------------------------------%
% agent_state(?Current_Intention, 
%             ?Percept_History, 
%             ?Action_History, 
%             ?Agents, 
%             ?Hostels, 
%             ?Treasures, 
%             ?Current_Path, 
%             ?Current_Plan, 
%             ?Current_Path_Cost).

:- dynamic agent_state/9.

%--------------------------------------------------------------------------------------------------%
% agent_map/4
% agent_map(?Position, ?Land, ?Things, ?Turn_Discovered).

:- dynamic agent_map/4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicados de acceso al estado del agente

%--------------------------------------------------------------------------------------------------%
% agent_intention/1
% agent_intention(-Intention)
%
% Unifica el argumento con la intencion actual del agente.
agent_intention(Intention) :-
    agent_state(Intention, _, _, _, _, _, _, _, _).

%--------------------------------------------------------------------------------------------------%
% agent_last_percept/1
% agent_last_percept(-Percept)
%
% Unifica el argumento con la ultima percepcion recibida por el agente.
agent_last_percept(Percept) :- 
    agent_state(_, [Percept | _], _, _, _, _, _, _, _).

%--------------------------------------------------------------------------------------------------%
% agent_last_action/1
% agent_last_action(-Action)
%
% Unifica el argumento con la ultima accion realizada por el agente.
agent_last_action(Action) :-
    agent_state(_, _, [Action | _], _, _, _, _, _, _).

%--------------------------------------------------------------------------------------------------%
% agent_known_agents/1
% agent_known_agents(-Agents)
% 
% Unifica el argumento con la base de datos de agentes enemigos.
agent_known_agents(Agents) :-
    agent_state(_, _, _, Agents, _, _, _, _, _).

%--------------------------------------------------------------------------------------------------%
% agent_known_hostels/1
% agent_known_hostels(-Hostels)
%
% Unifica el argumento con la base de datos de hostels conocidos por el agente.
agent_known_hostels(Hostels) :-
    agent_state(_, _, _, _, Hostels, _, _, _, _).

%--------------------------------------------------------------------------------------------------%
% agent_known_treasures/1
% agent_known_treasures(-Treasures)
%
% Unifica el argumento con la base de datos de tesoros conocidos sin recolectar.
agent_known_treasures(Treasures) :-
    agent_state(_, _, _, _, _, Treasures, _, _, _).

%--------------------------------------------------------------------------------------------------%
% agent_current_path/1
% agent_current_path(-Path)
%
% Unifica el argumento con el camino almacenado.
agent_current_path(Path) :-
    agent_state(_, _, _, _, _, _, Path, _, _).

%--------------------------------------------------------------------------------------------------%
% agent_current_plan/1
% agent_current_plan(-Plan)
%
% Unifica el argumento con la secuencia de acciones almacenada.
agent_current_plan(Plan) :-
    agent_state(_, _, _, _, _, _, _, Plan, _).

%--------------------------------------------------------------------------------------------------%
% agent_current_path_cost/1
% agent_current_path_cost(-Path_Cost)
%
% Unifica el argumento con el costo del camino almacenado.
agent_current_path_cost(Cost) :-
    agent_state(_, _, _, _, _, _, _, _, Cost).

%--------------------------------------------------------------------------------------------------%
% agent_current_turn/1
% agent_current_turn(-Turn)
%
% Unifica el argumento con el turno actual
agent_current_turn(Turn) :-
    agent_last_percept([Turn | _]).

%--------------------------------------------------------------------------------------------------%
% agent_position/1
% agent_position(-Position)
%
% Unifica el argumento con la posicion actual del agente.
agent_position(Position) :- 
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _, _, _, _),
    member([pos, Position], Attributes).

%--------------------------------------------------------------------------------------------------%
% agent_direction/1
% agent_direction(-Direction)
%
% Unifica el argumento con la direccion actual del agente.
agent_direction(Direction) :- 
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _, _, _, _),
    member([dir, Direction], Attributes).

%--------------------------------------------------------------------------------------------------%
% agent_stamina/1
% agent_stamina(-Stamina)
%
% Unifica el argumento con el valor actual de stamina del agente. 
agent_stamina(Stamina) :-
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _, _, _, _),
    member([stamina, Stamina], Attributes).

%--------------------------------------------------------------------------------------------------%
% agent_max_stamina/1
% agent_max_stamina(-Max_Stamina)
%
% Unifica el argumento con el valor maximo de stamina del agente.  
agent_max_stamina(Max_Stamina) :-
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _, _, _, _),
    member([max_stamina, Max_Stamina], Attributes).

%--------------------------------------------------------------------------------------------------%
% agent_fight_skill/1
% agent_fight_skill(-Fight_Skill)
%
% Unifica el argumento con la habilidad de combate del agente. 
agent_fight_skill(Fight_Skill) :- 
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _, _, _, _),
    member([fight_skill, Fight_Skill], Attributes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicados de actualizacion del estado del agente
%
%--------------------------------------------------------------------------------------------------%
% update_state/2
% update_state(+Target, +Value)
% 
% update_state actualiza las varias partes de del estado interno del agente.
% El primer argumento es que parte actualizar.
% El segundo argumento es el valor con el cual actualizarlo.
%
% En el caso de una actualizacion del estado segun una percepcion nueva, el trabajo de la extraccion
% de la informacion la hace el predicado extract_information_from_vision/9

%--------------------------------------------------------------------------------------------------%
update_state(current_intention, NI) :- 
    agent_state(    CI, PH, AH, AG, HS, TR, Path, Plan, Cost),
    replaceall(
        agent_state(CI, PH, AH, AG, HS, TR, Path, Plan, Cost),
        agent_state(NI, PH, AH, AG, HS, TR, Path, Plan, Cost)
    ).

%--------------------------------------------------------------------------------------------------%
update_state(percept, [Turn, Vision, Attributes, Inventory]) :- 
    agent_state(CI, PH, AH, AG_Old, HS_Old, TR_Old, Path, Plan, Cost),
    extract_information_from_vision(Turn, Vision, Action, AG_Old, HS_Old, TR_Old, AG_New, HS_New, TR_New),
    replaceall(
        agent_state(CI, 
                    PH, 
                    AH, 
                    AG_Old, HS_Old, TR_Old, Path, Plan, Cost),
        agent_state(CI, 
                    [[Turn, Vision, Attributes, Inventory] | PH], 
                    [Action | AH], 
                    AG_New, HS_New, TR_New, Path, Plan, Cost)
    ).

%--------------------------------------------------------------------------------------------------%
update_state(current_path, New_Path) :- 
    agent_state(CI, PH, AH, AG, HS, TR, Path, Plan, Cost),
    replaceall(
        agent_state(CI, PH, AH, AG, HS, TR,     Path, Plan, Cost),
        agent_state(CI, PH, AH, AG, HS, TR, New_Path, Plan, Cost)
    ).

%--------------------------------------------------------------------------------------------------%
update_state(current_plan, New_Plan) :- 
    agent_state(CI, PH, AH, AG, HS, TR, Path, Plan, Cost),
    replaceall(
        agent_state(CI, PH, AH, AG, HS, TR, Path,     Plan, Cost),
        agent_state(CI, PH, AH, AG, HS, TR, Path, New_Plan, Cost)
    ).

%--------------------------------------------------------------------------------------------------%
update_state(current_path_cost, New_Cost) :- 
    agent_state(CI, PH, AH, AG, HS, TR, Path, Plan, Cost),
    replaceall(
        agent_state(CI, PH, AH, AG, HS, TR, Path, Plan,     Cost),
        agent_state(CI, PH, AH, AG, HS, TR, Path, Plan, New_Cost)
    ).

