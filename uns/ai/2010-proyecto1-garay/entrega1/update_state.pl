%--------------------------------------------------------------------------------------------------%
% agent_state(?Current_Goal, ?Percept_History, ?Action_History, ?Agents, ?Inns, ?Treasures).
%
% Name:             nombre del agente, lo mismo que devuelve ag_name/1
% Current_Goal:     meta actual del agente
%        meta actual del agente, ya sea, evadir enemigos y buscar posada, o buscar tesoro, explorar, atacar, 
%            si es explorar (cuando no hay tesoro conocido) moverse a una posicion desconocida mas cercana
%                si no hay posiciones sin explorar, ir a la posicion desconocida que se exploro hace mas tiempo
%            si es juntar oro, ir hacia el oro mas cercano para juntar
%            si es atacar, ir al agente enemigo y atacar
%            si es huir, ir a la posada mas cercana evitando enemigos. 
% Percept_History:  historial de percepciones, la primera es la ultima percepcion recibida
% Action_History:   historial de acciones, la primera es la ultima accion tomada
% Agents:           lista de agentes conocidos
%                       cada agente es [nombre del agente, posicion donde fue visto por ultima vez, ultima accion que se lo vio realizar, estimacion de su stamina, estimacion de su oro ]
%                       agentes agresivos: lista de nombres de agentes que lo han derrotado a otros agentes, incluyendose, por ende con mas probabilidad de ser agresivos o tener alta fight_skill, a ser tenidos en cuenta
%                       agentes debiles: lista de agentes 
% Inns:             lista de las posiciones de las posadas conocidas
% Treasures:        lista de las posiciones de tesoros conocidos no recogidos
:- dynamic agent_state/6.

%--------------------------------------------------------------------------------------------------%
% agent_map(Position, Land, Things, Turn_Discovered).
:- dynamic agent_map/4.

%--------------------------------------------------------------------------------------------------%
% sample_percept(-Percept) unifica el argumento con una percepcion de prueba, para propositos de 
% testeo. 
sample_percept(Percept) :-
    Turn = 1,
    Vision = [
        [[06,06],    water, []],
        [[05,06],    water, []],
        [[04,06],    water, []],
        [[03,06],    plain, []],
        [[06,05],    plain, []],
        [[05,05],    plain, []],
        [[04,05],    water, []],
        [[03,05],    plain, []],
        [[06,07],    plain, [[agent,emig,[[dir,n],[unconscious,false],[previous_turn_action,move_fwd]]]]],
        [[05,07],    plain, [[treasure,t1,[[val,100]]]]],
        [[04,07],    plain, []],
        [[03,07],    plain, []],
        [[06,08],    plain, []],
        [[05,08],    plain, []],
        [[04,08],    plain, []],
        [[03,08],   forest, []],
        [[06,09], mountain, []],
        [[05,09],    plain, []],
        [[04,09],   forest, []],
        [[03,09],    plain, []]
        ],
    Attributes = [[pos, [2,3]], [dir, n], [stamina, 100], [max_stamina, 150], [fight_skill, 100]],
    Inventory = [],
    Percept = [Turn, Vision, Attributes, Inventory].

%--------------------------------------------------------------------------------------------------%
% update_state/2
% update_state(+Target, +Value)
% 
% Update_state actualiza las varias partes de del estado interno del agente.
% El primer argumento es que parte actualizar.
% El segundo argumento es el valor con el cual actualizarlo.
% El primer argumento debe ser o bien el atomo current_goal, o bien el atomo percept.
% En el primer caso se cambia la meta actual del agente al valor del segundo argumento.
% En el segundo caso se actualiza todo el estado del agente segun la informacion provista en la 
% percepcion, que esta ligada al segundo argumento.
%
% En el caso de una actualizacion del estado segun una percepcion nueva, el trabajo de la extraccion
% de la informacion la hace el predicado extract_information_from_vision/9
update_state(current_goal, Value) :- 
    agent_state(Current_Goal, Percept_History, Action_History, Agents, Inns, Treasures),
    replaceall(
        agent_state(Current_Goal, Percept_History, Action_History, Agents, Inns, Treasures),
        agent_state(Value,        Percept_History, Action_History, Agents, Inns, Treasures)
    ).

update_state(percept, [Turn, Vision, Attributes, Inventory]) :- 
    agent_state(Current_Goal, Percept_History, Action_History, Agents_old, Inns_old, Treasures_old),
    %trace,
    extract_information_from_vision(Turn, 
                                    Vision, 
                                    Agents_old, 
                                    Inns_old, 
                                    Treasures_old, 
                                    Agents_new, 
                                    Inns_new, 
                                    Treasures_new, 
                                    Action),
    %notrace,
    replaceall(
        agent_state(Current_Goal, 
                    Percept_History,            
                    Action_History,  
                    Agents_old, 
                    Inns_old, 
                    Treasures_old),
        agent_state(Current_Goal, 
                    [[Turn, Vision, Attributes, Inventory] | Percept_History], 
                    [Action | Action_History], 
                    Agents_new, 
                    Inns_new, 
                    Treasures_new)
    ).

%--------------------------------------------------------------------------------------------------%
% Update State auxiliar predicates

%--------------------------------------------------------------------------------------------------%
% extract_information_from_vision/9
% extract_information_from_vision(  +Turn,
%                                   +Vision,
%                                   +Agents_old,
%                                   +Inns_old,
%                                   +Treasures_old,
%                                   -Agents_new,
%                                   -Inns_new,
%                                   -Treasures_new,
%                                   -Last_Action)
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
                                Agents1, 
                                Inns1, 
                                Treasures1, 
                                Agents1, 
                                Inns1, 
                                Treasures1, 
                                _Last_Action).

% Si hay elementos en la Vision, por cada uno extraer la informacion apropiada. 
extract_information_from_vision(Turn,  
                                [[Pos, Land, Things] | Vision], 
                                Agents1, 
                                Inns1, 
                                Treasures1, 
                                Agents3, 
                                Inns3, 
                                Treasures4,  
                                Last_Action) :-
    replaceall(
        agent_map(Pos, _, _, _), 
        agent_map(Pos, Land, Things, Turn)
    ),
    remove_missing_treasures(Treasures1, Pos, Treasures2),
    extract_information_from_cell(  Pos, 
                                    Things, 
                                    Agents1, Inns1, Treasures2, 
                                    Agents2, Inns2, Treasures3, Last_Action),
    extract_information_from_vision(Turn, 
                                    Vision, 
                                    Agents2, Inns2, Treasures3, 
                                    Agents3, Inns3, Treasures4, Last_Action).

%--------------------------------------------------------------------------------------------------%
% extract_information_from_cell/9
% extract_information_from_cell(Position
%                               Things_at_position,
%                               Agents_old, Inns_old, Treasures_old,
%                               Agents_new, Inns_new, Treasures_new,
%                               Last_action)
%
% Revisa los objetos encontrados en una celda, y calcula las nuevas listas del estado del agente.

% Si la lista de objetos en la posicion es vacia, no hay nada que agregegar.
extract_information_from_cell(  _Pos, 
                                [],
                                Agents1, Inns1, Treasures1, 
                                Agents1, Inns1, Treasures1, _Last_Action).

% Hay un agente en la celda y soy yo, recuperar y devolver la ultima accion.
extract_information_from_cell(  Pos, 
                                [[agent, ThingName, Description] | Things], 
                                Agents1, Inns1, Treasures1, 
                                Agents3, Inns3, Treasures3,  Last_Action) :- 
    ag_name(My_Name),
    My_Name = ThingName,
    member([previous_turn_action, Last_Action], Description), 
    extract_information_from_cell(  Pos, 
                                    Things, 
                                    Agents1, Inns1, Treasures1, 
                                    Agents3, Inns3, Treasures3, Last_Action).

% Hay un agente en la celda y no soy yo, actualizar la lista de agentes.
extract_information_from_cell(  Pos, 
                                [[agent, ThingName, Description] | Things], 
                                Agents1, Inns1, Treasures1, 
                                Agents3, Inns3, Treasures3, Last_Action) :- 
    write('Encontre un AGENTE en la celda '), write_position(Pos), write(' ahora lo recordare.'), nl,
    update_agents(Agents1, Agents2, ThingName, Description, Turn),
    extract_information_from_cell(  Pos, 
                                    Things, 
                                    Agents2, Inns1, Treasures1, 
                                    Agents3, Inns3, Treasures3, Last_Action).

% Hay una posada en la celda, actualizar la lista de posadas.
extract_information_from_cell(  Pos, 
                                [[hostel, _ThingName, _Description] | Things], 
                                Agents1, Inns1, Treasures1, 
                                Agents3, Inns3, Treasures3, Last_Action) :- 
    write('Encontre un HOTEL en la celda '), write_position(Pos), write(' ahora la recordare.'), nl,
    update_inns(Inns1, Inns2, Pos),
    extract_information_from_cell(  Pos, 
                                    Things, 
                                    Agents1, Inns2, Treasures1, 
                                    Agents3, Inns3, Treasures3, Last_Action).

% Hay un tesoro en la celda, actualizar la lista de tesoros.
extract_information_from_cell(  Pos, 
                                [[treasure, _ThingName, _Description] | Things], 
                                Agents1, Inns1, Treasures1, 
                                Agents3, Inns3, Treasures3, Last_Action) :- 
    % Hay un tesoro en Pos, actualizar la lista de tesoros.
    write('Encontre un TESORO en la celda '), write_position(Pos), write(' ahora la recordare.'), nl,
    update_treasures(Treasures1, Treasures2, [Pos, Turn]),
    extract_information_from_cell(  Pos, 
                                    Things, 
                                    Agents1, Inns1, Treasures2, 
                                    Agents3, Inns3, Treasures3, Last_Action).

%--------------------------------------------------------------------------------------------------%
% update_agents/5
update_agents(  [], 
                [[Agent_Name,[Agent_Description], Turn]], 
                Agent_Name, 
                Agent_Description, 
                Turn).

update_agents(  [[Agent_Name, Old_Agent_Description, _Old_Turn] | Tail], 
                [[Agent_Name, Agent_New_Description, Turn] | Tail], 
                Agent_Name, 
                Agent_Description, 
                Turn) :-
    append(Old_Agent_Description, [Agent_Description], Agent_New_Description).
    
update_agents(  [[Agent_Name_Foo | Resto] | Tail], 
                [[Agent_Name_Foo | Resto] | Tail2], 
                Agent_Name, 
                Agent_Description, 
                Turn) :-
    Agent_Name_Foo \= Agent_Name,
    update_agents(Tail, Tail2, Agent_Name, Agent_Description, Turn).

%--------------------------------------------------------------------------------------------------%
% update_inns/3
% update_inns(+Inns_Old, -Inns_New, +Pos)
%
% Actualiza la lista de posadas conocidas. 
% El primer argumento debe ser la lista actual de posadas conocidas por el agente. 
% El segundo argumento sera unificado con la lista actualizada. Si la posada ya era conocida, sera
% la misma lista. 
% El tercer argumento debe ser la posicion de la posada.
update_inns(Inns_old, Inns_new, Pos) :- 
    ord_add_element(Inns_old, Pos, Inns_new).

%--------------------------------------------------------------------------------------------------%
% update_treasures/3
% update_treasures(+Treasures_old, -Treasures_new, +Pos)
% 
% Actualiza la lista de tesoros avistados pero no recolectados por el agente. 
% El primer argument debe ser la lista actual de tesoros conocidos por el agente. 
% El segundo argumento sera unificado con la lista actualizada. Si el tesoro ya habia sido visto
% por el agente y se encuentra en la lista, entonces el resultado sera identico al primer argumento.
% El tercer argumento debe ser la posicion del tesoro avistado.
update_treasures([], [[[F, C], Turn]], [[F, C], Turn]).

update_treasures([[[F, C],Old_Turn] | Tail], [[[F, C], Turn] | Tail], [[F, C], Turn]) :- 
    write('ReEncontre el TESORO de '), 
    write('['), write(F), write(','), write(C), write(']'), 
    write(' ahora lo recordare que sigue alli.'), nl.

update_treasures([[Pos1, Turn_Pos1] | Tail], [[Pos1, Turn_Pos1] | Treasures_new], [Pos2, Turn]) :- 
    Pos1 \= Pos2, 
    update_treasures(Tail, Treasures_new, [Pos2,Turn]).

%--------------------------------------------------------------------------------------------------%
% agent_last_percept/1
% agent_last_percept(-Percept)
%
% Unifica el argumento con la ultima percepcion recibida por el agente.
agent_last_percept(Percept) :- 
    agent_state(_, [Percept | _], _, _, _, _).

%--------------------------------------------------------------------------------------------------%
% agent_last_action/1
% agent_last_action(-Action)
%
% Unifica el argumento con la ultima accion realizada por el agente.
agent_last_action(Action) :-
    agent_state(_, _, [Action | _], _, _, _).

%--------------------------------------------------------------------------------------------------%
% agent_position/1
% agent_position(-Position)
%
% Unifica el argumento con la posicion actual del agente.
agent_position(Position) :- 
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _),
    member([pos, Position], Attributes).

%--------------------------------------------------------------------------------------------------%
% agent_direction/1
% agent_direction(-Direction)
%
% Unifica el argumento con la direccion actual del agente.
agent_direction(Direction) :- 
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _),
    member([dir, Direction], Attributes).

%--------------------------------------------------------------------------------------------------%
% agent_stamin/1
% agent_stamina(-Stamina)
%
% Unifica el argumento con el valor actual de stamina del agente. 
agent_stamina(Stamina) :-
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _),
    member([stamina, Stamina], Attributes).

%--------------------------------------------------------------------------------------------------%
% agent_max_stamina/1
% agent_max_stamina(-Max_Stamina)
%
% Unifica el argumento con el valor maximo de stamina del agente.  
agent_max_stamina(Max_Stamina) :-
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _),
    member([max_stamina, Max_Stamina], Attributes).

%--------------------------------------------------------------------------------------------------%
% agent_fight_skill/1
% agent_fight_skill(-Fight_Skill)
%
% Unifica el argumento con la habilidad de combate del agente. 
agent_fight_skill(Fight_Skill) :- 
    agent_state(_, [[_, _, Attributes, _] | _], _, _, _, _),
    member([fight_skill, Fight_Skill], Attributes).

%--------------------------------------------------------------------------------------------------%
% remove_missing_treasures/3
% remove_missing_treasures(+Treasures_Old, +Position, -Treasures_New)
% Si no hay un tesoro en la posicion, eliminarlo de la base de datos de tesoros.
% El primer argumento debe ser la lista de posiciones de los tesoros conocidos. 
% El segundo argumento debe ser la posicion a eliminar.
% El tercer argumento es de salida, y es ligado a la lista actualizada.
remove_missing_treasures([], _Pos, []).

remove_missing_treasures([[Pos,T] | Tail], Pos, Tail).

remove_missing_treasures([[Pos_Foo, Turn_Foo] | Tail], Pos, [[Pos_Foo, Turn_Foo] | New_Tail]) :- 
    Pos_Foo \= Pos, 
    remove_missing_treasures(Tail,Pos,New_Tail).
