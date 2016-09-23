% TODO: fusionar los predicados update_treasures y remove_missing_treasures
% TODO: asegurarse que "juntar el tesoro mas cercano" incluya el caso de que el tesoro este en mi posicion actual

%--------------------------------------------------------------------------------------------------%
% decide_action/1
% decide_action(-Action)
%
% Determina la proxima accion que tomara el agente.
%
% El 1er argumento queda ligado a la representacion de la accion a tomar.

decide_action(Action) :-
    determine_intention,
    determine_action(Action).

%--------------------------------------------------------------------------------------------------%
% determine_intention/0
% 
% Determina la intencion del agente segun lo que se percibio.

% Situacion (1):
% Si hay un tesoro en la posicion actual, su intencion sera juntar el tesoro.
determine_intention :-
    treasure_at_current_position(_),
    update_state(current_intention, gather_treasure),
    write('Hay un tesoro en mi posicion actual.'), nl,
    write('Mi intencion es levantarlo. [gather_treasure]'), nl, nl.

% Situacion (2):
% Si no se da la situacion 1 y si el agente esta en un hostel y su stamina actual es menor a su 
% maxima stamina, su intencion sera quedarse descansando hasta que lo echen.
determine_intention :-
    agent_in_hostel,
    agent_stamina(Stamina),
    agent_max_stamina(Max_Stamina),
    Stamina < Max_Stamina + 10,
    update_state(current_intention, resting),
    write('Estoy en un hostel, y mi stamina actual es menor a mi maxima stamina.'), nl,
    write('Mi intencion es quedarme a descansar. [resting]'), nl, nl.

% Situacion (3):
% Si no se dan las situaciones 1 o 2, y su stamina es menor o igual el costo del camino al hostel 
% mas cercano mas un 10% del costo, su intencion sera ir al hostel.
% Si no hay hostels, esto falla siempre. Explorara hasta que quede inconsciente por cansancio.
determine_intention :-
    calculate_path_to_nearest_hostel(_Path, Plan, Cost),
    Cost > 0,
    agent_stamina(Stamina),
    Stamina =< Cost * 1.1,
    update_state(current_intention, go_to_hostel),
    update_state(current_plan, Plan), 
    update_state(current_path_cost, Cost), 
    write('Mi stamina es menor o igual al costo del camino al hostel mas cercano, mas un margen.'), nl,
    write('Mi intencion es dirigirme hacia el hostel mas cercano. [go_to_hostel]'), nl, nl.

% Situacion (4):
% Si no se dan las situaciones 1, 2, o 3 y el agente fue atacado el turno anterior por otro agente,
% su intencion sera defenderse del agente enemigo.
% Como dijo Napoleon Bonaparte, "La mejor defensa es una buena ofensa."
determine_intention :-
    agent_under_attack(Attacking_Agent_Name),
    agent_in_attack_range(Attacking_Agent_Name),
    update_state(current_intention, defend_myself_from(Attacking_Agent_Name)),
    write('Estoy bajo ataque!'), nl,
    write('Mi intencion es defenderme. [defend_myself_from('), write(Attacking_Agent_Name), write(')]'), nl, nl.

% Situacion (5):
% Si no se dan las situaciones, 1, 2, 3 o 4 y la intencion anterior del agente era defenderse de un
% agente atacante o cazar a un agente atacante, entonces su intencion sera cazar al agente atacante.
% Cuando la intencion del agente es cazar a otro, si el agente esta en rango de ataque lo ataca, 
% y sino se mueve hacia el agente que esta huyendo.
% El agente saldra del modo caceria cuando se den algunas de las situaciones 1, 2, 3 o 4 o cuando 
% el agente atacante este inconsciente. 
determine_intention :-
    (
        agent_intention(defend_myself_from(Agent_Name))
        ;
        agent_intention(hunting(Agent_Name))
    ),
    update_state(current_intention, hunting(Agent_Name)),
    write('Estoy cazando a un agente.'), nl,
    write('Mi intencion es seguir cazandolo. [hunting('), write(Agent_Name), write(')]'), nl, nl.

% Situacion (6):
% Si no se dan las situaciones 1, 2, 3, 4, o 5 y el agente estaba en modo defensa o caceria y el 
% agente atacante esta inconsciente, y hay tesoros sin juntar, la intencion del agente sera juntar 
% el tesoro mas cercano. 
determine_intention :-
    (
        agent_intention(defend_myself_from(Agent_Name))
        ;
        agent_intention(hunting(Agent_Name))
    ),
    % El agente atacante esta inconsciente.
    agent_known_agents(Agents),
    get_assoc(Agent_Name, Agents, Agent_Info),
    get_assoc(unconscious, Agent_Info, true),
    pending_treasures,
    calculate_path_to_nearest_treasure(_Path, Plan, Cost),
    % Puede ser que haya tesoros por recolectar, pero no conocemos el camino hacia ellos. 
    % En ese caso no debe ser la intencion del agente tratar de seguir un camino que no existe 
    % hacia el tesoro mas cercano. 
    % Si no hay camino, el costo sera 0.
    % Verificando que el costo no sea 0, y fallando en el caso en que lo es, se asegura que si no 
    % hay camino, el agente hara otras cosas. 
    Cost > 0,
    update_state(current_plan,      Plan),
    update_state(current_intention, go_to_treasure),
    write('Estaba cazando a un agente, pero esta inconsciente ahora, y hay tesoros por juntar.'), nl,
    write('Mi intencion es ir a juntar algun tesoro. [go_to_treasure]'), nl, nl.

% Situacion (7):
% Si no se dan las situaciones 1, 2, 3, 4, 5 o 6 y hay tesoros conocidos por recolectar, 
% entonces la intencion del agente sera ir al tesoro mas cercano. 
determine_intention :-
    pending_treasures,
    calculate_path_to_nearest_treasure(_Path, Plan, Cost),
    Cost > 0,
    update_state(current_plan,      Plan),
    update_state(current_intention, go_to_treasure),
    write('Hay tesoros por juntar.'), nl,
    write('Mi intencion es ir a juntarlos. [go_to_treasure]'), nl, nl.

% Situacion (8): 
% Si no se dan las situaciones 1, 2, 3, 4, 5, 6 o 7 y hay un agente enemigo que el agente ha
% marcado como "rico", entonces la intencion del agente sera atacar al agente enemigo rico.
determine_intention :-
    rich_agent_in_sight(Agent_Name),
    update_state(current_intention, attack(Agent_Name)),
    write('Hay un agente rico a la vista.'), nl,
    write('Mi intencion es ir a atacarlo para robarle sus tesoros. [attack('), write(Agent_Name), write(')]'), nl, nl.

determine_intention :-
    update_state(current_intention, explore),
    write('No hay nada para hacer.'), nl,
    write('Mi intencion es generar un plan de exploracion. [explore]'), nl, nl.

%--------------------------------------------------------------------------------------------------%    
% determine_action/1
% 
% Determina la accion segun la intencion actual del agente.
% El primer argumento es un parametro de salida, liga con la accion que tomara el agente este turno.

% Situacion (1):
% Si la intencion del agent es descansar, no toma ninguna accion. 
determine_action(Action) :-
    agent_intention(resting),
    agent_in_hostel,
    Action = none, 
    write('Mi intencion es descansar en un hostel. [resting]'), nl,
    write('Mi accion sera no hacer nada. [none]'), nl, nl.
    
% Situacion (2):
% Si la intencion del agente es juntar un tesoro, recupera el nombre del tesoro en su posicion 
% actual y realiza la accion pickup del tesoro correspondiente.
determine_action(Action) :-
    agent_intention(gather_treasure),
    treasure_at_current_position(Treasure_Name),
    Action = pickup(Treasure_Name),
    write('Mi intencion es juntar un tesoro en mi posicion. [gather_treasure]'), nl,
    write('Mi accion sera juntarlo [pickup('), write(Treasure_Name), write(')]'), nl, nl.

% Situacion (3):
% Si la intencion del agente es ir al hostel mas cercano para recuperar stamina, entonces recupera
% la secuencia de acciones que tiene que tomar para llegar al hostel, almacena el resto de las 
% acciones y ejecuta la primera.
determine_action(Action) :-
    agent_intention(go_to_hostel),
    agent_current_plan([Action | Plan]),
    update_state(current_plan, Plan),
    write('Mi intencion es dirigirme hacia el hostel.'), nl,
    write('Mi accion sera seguir el camino que calcule hacia el. ['), write(Action), write(']'), nl, nl.

% Situacion (4):
% Si la intencion del agente es defenderse de otro agente atacante, y el agente esta en rango de
% ataque, entonces realiza la accion attack con el nombre de agente correspondiente.
% Se sabe que el agente atacante siempre estara en rango de ataque porque para que el agente haya
% sido atacado, el otro tiene que haber realizado el turno anterior la accion attack, y por ende
% esta en rango.
determine_action(Action) :-
    agent_intention(defend_myself_from(Agent_Name)),
    Action = attack(Agent_Name),
    write('Mi intencion es defenderme de un agente atacante.'), nl,
    write('Mi accion sera atacarlo. [attack('), write(Agent_Name), write(')]'), nl, nl.

% Situacion (5.1):
% Si la intencion del agente es cazar a otro agente, y el agente atacante esta en rango, entonces
% realiza la accion attack con el nombre de agente correspondiente.
determine_action(Action) :-
    agent_intention(hunting(Agent_Name)),
    agent_in_attack_range(Agent_Name),
    Action = attack(Agent_Name),
    write('Mi intencion es cazar a un agente, y esta en rango de ataque.'), nl,
    write('Mi accion sera atacarlo. [attack('), write(Agent_Name), write(')]'), nl, nl.

% Situacion (5.2):
% Si la intencion del agente es cazar a otro agente, y no esta en rango, entonces el agente calcula
% el camino mas corto hacia el agente y se mueve por ese camino. 
% El camino se recomputa nuevamente cada vez para tomar en cuenta las acciones de evasion del agente
% enemigo.
determine_action(Action) :-
    agent_intention(hunting(Agent_Name)),
    calculate_path_to_agent(Agent_Name, _Path, [Action | _Actions], _Cost),
    write('Mi intencion es cazar a un agente, y no esta en rango de ataque.'), nl,
    write('Mi accion sera moverme hacia el. ['), write(Action), write(']'), nl, nl.

% Situacion (6) y (7):
% Si la intencion del agente es ir a recolectar un tesoro que tenia pendiente, entonces el agente
% recupera el camino hacia el tesoro y lo sigue, y actualiza el plan actual con el resto de las 
% acciones necesarias para llegar al tesoro. 
determine_action(Action) :-
    agent_intention(go_to_treasure),
    agent_current_plan([Action | Plan]),
    update_state(current_plan, Plan),
    write('Mi intencion es recolectar un tesoro que tenia pendiente.'), nl,
    write('Mi accion sera moverme hacia el. ['), write(Action), write(']'), nl, nl.

% Situacion (8.1):
% Si la intencion del agente es atacar a otro agente, y el agente esta en rango, entonces el agente
% realizara la accion attack con el nombre de agente correspondiente. 
determine_action(Action) :-
    agent_intention(attack(Agent_Name)),
    agent_in_attack_range(Agent_Name),
    Action = attack(Agent_Name),
    write('Mi intencion es atacar a otro agente, y esta en rango de ataque.'), nl,
    write('Mi accion sera atacarlo. [attack('), write(Agent_Name), write(')]'), nl, nl.

% Situacion (8.2):
% Si la intencion del agente es atacar a otro agente y el agente no esta en rango de ataque pero si
% en rango visual, entonces el agente seguira el camino mas corto al agente enemigo.
% El camino se recomputa cada turno para tomar en cuenta las acciones evasivas del otro agente.
determine_action(Action) :-
    agent_intention(attack(Agent_Name)),
    calculate_path_to_agent(Agent_Name, _Path, [Action | _Actions], _Cost),
    write('Mi intencion es atacar a otro agente, y no esta en rango de ataque.'), nl,
    write('Mi accion sera moverme hacia el. ['), write(Action), write(']'), nl, nl.

% Situacion (9):
% Si la intencion del agente es explorar, entonces seguira el camino hacia la celda no explorada
% que hace mas tiempo no se vio. 
determine_action(Action) :-
    agent_intention(explore),
    (
        agent_position(Pos),
        agent_direction(Direction),
        ady_at_cardinal(Pos, Direction, PosInFront),
        (
            not(passable(PosInFront))
        ;
            agent_known_hostels(Hostels),
            assoc_to_keys(Hostels, List_Of_Positions),
            member(PosInFront, List_Of_Positions)
        ),      
        next_90_clockwise(DesiredDir, Direction),
        Action = turn(DesiredDir)
    ;
        Action = move_fwd
    ),
    write('Mi intencion es explorar.'), nl,
    write('Mi accion sera moverme por el mundo. ['), write(Action), write(']'), nl, nl.


% Caso fall-back. 
% Si a pesar de conocer su intencion el agente no puede determinar que hacer, no realiza ninguna
% accion.
determine_action(none) :-
    write('Hubo un problema. No se que hacer.'), nl.

