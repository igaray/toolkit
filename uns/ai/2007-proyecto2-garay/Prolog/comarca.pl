:- consult(outputpred).
:- consult(search).
:- consult(agentedfs).
:- consult(agenteastar).
:- consult(agentpred).

% ENVIRONMENT INTERNAL STATE

:- dynamic agent_position/1.     % Maintains the agent's current position
:- dynamic agents_last_action/1. % Maintains the agent's last action
:- dynamic next_percept/1.       % Maintains the agent's next percept

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GENERAL ENVIRONMENT SIMULATOR

start_simulation(Initial_Position, Agent, Termination_Reason, Simulation_Record) :-

    % Clear and initialize the environment internal state
    retractall( agent_position(_)                ),
    retractall( agents_last_action(_)            ),
    retractall( next_percept(_)                  ),
    retractall( path(_, _, _)                    ),
    retractall( camouflage(_)                    ),
    asserta(    agent_position(Initial_Position) ),
    asserta(    agents_last_action(none)         ),
    asserta(    next_percept(init)               ),
    asserta(    path([], 0, [])                  ),
    asserta(    camouflage(off)                  ),

    % Run the simulation
    once(run_environment(update_state, Agent, termination_function(Termination_Reason), Simulation_Record)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run_environment(+Update_Function, +Agent, +Termination_Function, Simulation_History)
run_environment(_Update_Function, _Agent, Termination_Function, []) :-
    call(Termination_Function).

run_environment(Update_Function, Agent, Termination_Function, [[Position, Action, Current_Path, Current_Paths_Cost, Current_Positions_Frontier]|Simulation_Record]) :-
    get_percept(P),

    % create and call the agent predicate
    A =.. [Agent, P, Action],
    call(A),

    % create and call the update predicate
    U =.. [Update_Function, Action],
    call(U),

    % get position and path data to store in the simulation history
    agent_position(Position),
    (
     path(Current_Path, Current_Paths_Cost, [Current_Positions_Frontier|_]) ;
     path(Current_Path, Current_Paths_Cost, Current_Positions_Frontier)
    ),

    run_environment(Update_Function, Agent, Termination_Function, Simulation_Record).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERACTIVE ENVIRONMENT SIMULATOR

go(Initial_Position, Agent) :-
    start_interactive_simulation(Initial_Position, Agent).

% start_interactive_simulation(Initial_Position, +Agent)
% Starts the simulation, running Agent from Initial_Position
% Initial_Position is a two-element list, first element indicates column
% component of the initial position, second element indicates row component of
% the initial position.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_interactive_simulation(Initial_Position, Agent) :-

    % Clear and initialize the environment internal state
    retractall( agent_position(_)                ),
    retractall( agents_last_action(_)            ),
    retractall( next_percept(_)                  ),
    retractall( path(_, _, _)                    ),
    retractall( camouflage(_)                    ),
    asserta(    agent_position(Initial_Position) ),
    asserta(    agents_last_action(none)         ),
    asserta(    next_percept(init)               ),
    asserta(    path([], 0, [])                  ),
    asserta(    camouflage(off)                  ),

    % Run the simulation
    once(run_interactive_environment(update_state, Agent, termination_function(Reason))),
    print('Simulation terminated: '),
    print(Reason),
    nl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run_interactive_environment(+Update_Function, +Agent, +Termination_Function)
run_interactive_environment(_Update_Function, _Agent, Termination_Function) :-
    call(Termination_Function),
    show_world,
    print('Termination function succeeded.'), 
    nl.

run_interactive_environment(Update_Function, Agent, Termination_Function) :-
    show_world,
    get_percept(P),

    % create and call the agent predicate
    A =.. [Agent, P, Action],
    call(A),

    % create and call the update predicate
    U =.. [Update_Function, Action],
    call(U),

    % I/O
    print_percept(P), nl,
    print_action(Action), nl,
    (
     path(Current_Path, Current_Paths_Cost, [Current_Positions_Frontier|_]) ;
     path(Current_Path, Current_Paths_Cost, Current_Positions_Frontier)
    ),
    print_path(Current_Path, Current_Paths_Cost, Current_Positions_Frontier),
    get_char(_),

    run_interactive_environment(Update_Function, Agent, Termination_Function).



% Predicates common to the two simulation environments

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% termination_function/1
% termination_function(-Reason)
% Determines whether the simulation should terminate, and why.
% The simulation terminates if the agent abandons, if the agents dies, or if
% the agents finds the castle.

% The agent abandons
termination_function(abandono) :-
    agents_last_action(abandonar).

% The agent found the castle
termination_function(trompetas) :-
    agent_position(Position),
    celda(Position, pasto, castillo).

% The agent dies if he crosses woods, or water without a bridge, or water with a broken bridge.
termination_function(muerto) :-
    agent_position(Position),
    (
     celda(Position, agua, puente(roto)) ;
     celda(Position, agua, -)            ;
     celda(Position, bosque, -)
    ).

% The agent dies if he crosses an intact bridge galloping
termination_function(muerto) :-
    agents_last_action(avanzar(_Direccion, al_galope)),
    agent_position(Position),
    celda(Position, agua, puente(sano)).

% The agent dies if he crosses a deteriorated bridge galloping or at a trot
termination_function(muerto) :-
    (
     agents_last_action(avanzar(Direccion, al_galope)) ;
     agents_last_action(avanzar(Direccion, al_trote))
    ),
    agent_position(Position),
    celda(Position, agua, puente(det)).

% The agent dies if he crosses a friendly army with camouflage on
termination_function(muerto) :-
    camouflage(on),
    agent_position(Position),
    celda(Position, _Terrain, ejercito(reino)).

% The agent dies if he crosses an enemy army with camouflage off
termination_function(muerto) :-
    camouflage(off),
    agent_position(Position),
    celda(Position, _Terrain, ejercito(enemigo)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_percept(-P)
% Predicate that obtains the next percept.
get_percept(P) :-
    next_percept(P).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_state(+Accion)
% Update predicate for the environment's internal state.
update_state(avanzar(Direccion, Velocidad)) :-

    % Calculate and update the agent's next position based on current position and direction.
    next_position(Direccion, Next_Position),
    valid_position(Next_Position),
    replace(agent_position(_), agent_position(Next_Position)),
    replace(agents_last_action(_), agents_last_action(avanzar(Direccion, Velocidad))),

    % Calculate and update the agent's next percept
    percieve(Next_Position, Next_Percept),
    replace(next_percept(_), next_percept(Next_Percept)).

update_state(poner_disfraz) :-
    replace(agents_last_action(_), agents_last_action(poner_disfraz)).

update_state(quitar_disfraz) :-
    replace(agents_last_action(_), agents_last_action(quitar_disfraz)).

update_state(abandonar) :-
    replace(agents_last_action(_), agents_last_action(abandonar)).

update_state(buscando) :-
    % Calculate and update the agent's next percept
    agent_position(Position),
    percieve(Position, Next_Percept),
    replace(next_percept(_), next_percept(Next_Percept)),
    replace(agents_last_action(_), agents_last_action(buscando)).

update_state(none) :-
    % Calculate and update the agent's next percept
    agent_position(Position),
    percieve(Position, Next_Percept),
    replace(next_percept(_), next_percept(Next_Percept)),
    replace(agents_last_action(_), agents_last_action(none)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% next_position/2
next_position(norte, [R1, C]) :-
    agent_position([R, C]),
    R1 is R - 1.
next_position(este, [R, C1]) :-
    agent_position([R, C]),
    C1 is C + 1.
next_position(sur, [R1, C]) :-
    agent_position([R, C]),
    R1 is R + 1.
next_position(oeste, [R, C1]) :-
    agent_position([R, C]),
    C1 is C - 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% percieve/2
percieve([R, C], Percept) :-
    % Generate perception positions
    NR is R - 1,
    NC is C,
    ER is R,
    EC is C + 1,
    SR is R + 1,
    SC is C,
    WR is R,
    WC is C - 1,

    % Get perceptions from world
    celda([NR, NC], North_Terrain, North_Contents),
    celda([SR, SC], South_Terrain, South_Contents),
    celda([ER, EC], East_Terrain,  East_Contents),
    celda([WR, WC], West_Terrain,  West_Contents),

    % Generate the perception
    AlNorte = [[NR, NC], North_Terrain, North_Contents],
    AlSur   = [[SR, SC], South_Terrain, South_Contents],
    AlEste  = [[ER, EC], East_Terrain,  East_Contents],
    AlOeste = [[WR, WC], West_Terrain,  West_Contents],
    Percept = perc([R, C], AlNorte, AlSur, AlEste, AlOeste).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% valid_position/1
valid_position(Position) :-
    (
     celda(Position, pasto, Contents) ;
     celda(Position, mont,  Contents) ;
     celda(Position, agua,  puente(sano)) ;
     celda(Position, agua,  puente(det))
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replace/2
replace(X, Y) :- retract(X), !, assert(Y).
