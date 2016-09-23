

:- consult( 'auxpred.pl' ).
:- consult( 'aspibot.pl' ).
:- consult( 'habitacion1.pl' ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    ENVIRONMENT INTERNAL STATE                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic env_agents_position/1.    % Maintains the agents current position.
:- dynamic env_agents_last_action/1. % Maintains the agent's last action.
:- dynamic env_max_charge/1.         % Maintains the agent's battery charge.
:- dynamic env_current_charge/1.     % Maintains the agent's current battery charge.
:- dynamic env_next_percept/1.       % Maintains the agent's next percept.
:- dynamic env_termination_reason/1. % Maintains the reason the agent terminated the simulation.
:- dynamic env_cells_cleaned/1.      % Maintains the total amount of cells cleaned.
:- dynamic env_charge_used/1.        % Maintains a counter of the amount of charge used. 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                      ENVIRONMENT SIMULATOR                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sim_setup( +Max_Battery_Charge )                                                                 %
% Clear and initialize the simulator internal state                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sim_setup( Max_Battery_Charge ) :-
    retractall( env_agents_position( _ )                 ),
    retractall( env_agents_last_action( _ )              ),
    retractall( env_battery_charge( _ )                  ),
    retractall( env_current_charge( _ )                  ),
    retractall( env_next_percept( _ )                    ),
    retractall( env_termination_reason( _ )              ),
    retractall( env_cells_cleaned( _ )                   ),
    assert(     env_agents_last_action( none )           ),
    assert(     env_max_charge( Max_Battery_Charge )     ),
    assert(     env_current_charge( Max_Battery_Charge ) ),
    assert(     env_next_percept( init )                 ),
    assert(     env_cells_cleaned( 0 )                   ),
    assert(     env_charge_used( 0 )                     ).
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run_step/0                                                                                       %
% Run one step of the simulation.                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_step :-
    termination,
    !.

run_step :-
    print('getting percept'),nl,
    get_percept( P ),
    print('percept: '),print(P),nl,
    print('calling aspibot '),nl,

    aspibot( P, Action ),
    print('action '),print(Action),nl,
    update_state( Action ),
    get_percept( P1),print(P1),nl.

    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run_steps/1                                                                                       %
% Run one step of the simulation.                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_steps( 1 ) :-
    run_step.

run_steps( N ) :-
    print('N: '), print(N),nl,
    N1 is N - 1,
    run_step,
    run_steps( N1 ).
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% termination/0                                                                                    %
% Check for simulation termination.                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
termination :- 
    print('entering termination 1'), nl,
    % The agent shut itself down at home. 
    env_agents_last_action( apagar ),
    env_agents_position( Agents_Position ),
    celda( Agents_Position, home, - ),
    replace( termination_reason( _ ), termination_reason( shutdown_normal ) ),
    print( 'El agente se apago en home.' ), nl.

termination :- 
    print('entering termination 2'),nl,
    % No battery charge left. 
    env_agents_last_action( apagar ),
    env_current_charge( 0 ),
    replace( termination_reason( _ ), termination_reason( shutdown_battery ) ), 
    print( 'El agente se apago porque se le agoto la bateria.'), nl.

    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_percept( -P )                                                                                %
% Obtain the next percept.                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_percept( P ) :-
    env_next_percept( P ).

    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_state( +Action )                                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_state( aspirar ) :- 
    print('entering update_state aspirar'),nl,
    % Update the agent's last action.
    replace_all( env_agents_last_action( _ ), env_agents_last_action( aspirar ) ),

    % Update the battery state.
    update_current_charge( aspirar ),
    
    % Update the total amount of charge used. 
    update_charge_used( aspirar ),

    % Update amount of cells cleaned.
    env_cells_cleaned( CC ),
    CC1 is CC + 1,
    replace_all( env_cells_cleaned( _ ), env_cells_cleaned( CC1 ) ),

    % Update the cell status.
    env_agents_position( Agents_Position ),
    celda( Agents_Position, Terreno, _ ),
    replace( celda( Agents_Position, Terreno , _ ), celda( Agents_Position, Terreno, limpio ) ),
    
    % Generate and update the next percept.
    percieve( Agents_Position, Percept ),
    replace_all( env_next_percept( _ ), env_next_percept( Percept) ),
    print('exiting update_state aspirar'),nl,
    !.

update_state( apagar ) :- 
    print('entering update_state apagar'),nl,
    replace_all( env_agents_last_action( _ ), env_agents_last_action( apagar ) ),
    replace_all( env_next_percept( _ ), env_next_percept( end_simulation( shutdown_normal ) ) ),
    print('exiting update_state apagar'),nl,
    !.

update_state( Direction ) :-

    print('entering update_state '),print(Direction),nl,
    
    % Caculate the agent's next position.
    env_agents_position( Agents_Position ),
    print('agents position '),print(Agents_Position),nl,
    next_position( Agents_Position, Direction, Next_Position ),
    print('next_position '),print( Next_Position ),nl,
    
    % Update the agent's last action. 
    replace( env_agents_last_action( _ ), env_agents_last_action( Direction ) ), 
    
    update_current_charge( Direction ),
    update_charge_used( Direction ),

    (
        ( % El agente se desplaza hacia Direction y llega a home.
            celda( Next_Position, home, - ),
            print('update_state en caso home '),print(Next_Position),nl,
            percieve( Next_Position, Percept ), 
            replace( env_agents_position( _ ), env_agents_position( Next_Position ) )
        ) ;
        ( % El agente se desplaza hacia Direction donde hay una baldosa.
            celda( Next_Position, bal, _ ),
            print('update_state en caso baldoza '),print(Next_Position),nl,
            percieve( Next_Position, Percept ), 
            replace( env_agents_position( _ ), env_agents_position( Next_Position ) )
        ) ;
        ( % El agente se desplaza hacia Direction donde hay alfombra.
            celda( Next_Position, alf, _ ),
            print('update_state en caso alfombra '),print(Next_Position),nl,
            percieve( Next_Position, Percept ), 
            print('despues de percieve de alfombra'),nl,
            replace( env_agents_position( _ ), env_agents_position( Next_Position ) )
        ) ;
        ( % El agente se desplaza hacia Direction donde hay un obstáculo
            celda( Next_Position, obs, - ),
            print('update_state en caso obstaculo '),print(Next_Position),nl,
            percieve( Next_Position, Percept )
            % En este caso, la posicion del agente no se cambia. 
        ) 
    ),
    print('salio de los cuatro casos'),nl,
    replace( env_next_percept( _ ), env_next_percept( Percept ) ),
    print('exiting update_state'),nl,
    !.   
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_current_charge/1                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_current_charge( aspirar ) :-
    env_current_charge( X ),
    Y is X - 1,
    replace( env_current_charge( _ ), env_current_charge( Y ) ).

update_current_charge( Direction ) :-

    env_agents_position( Agents_Position ),
    next_position( Agents_Position, Direction, Next_Position ),

    env_current_charge( X ),
    celda( Next_Position, Terrain, _ ),
    (
        (
            Terrain = home,
            env_max_charge( Max_Charge ),
            Y is Max_Charge
        ) ;
        (
            Terrain = bal,
            Y is X - 1
        ) ;
        (
            Terrain = alf,
            Y is X - 2
        ) ;
        (
            Terrain = obs,
            Y is X - 1
        )
    ),
    replace( env_current_charge( _ ), env_current_charge( Y ) ),
    !.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_charge_used( +Action )                                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_charge_used( Action ) :-
    (
        Action = adelante ; 
        Action = atras ; 
        Action = izquierda ;
        Action = derecha
    ),
    env_agents_position( P ),
    env_charge_used( C1 ),
    celda( P, T, _E ),
    terrain_cost( T, C2 ),
    C3 is C1 + C2,
    replace( env_charge_used( _ ), env_charge_used( C3 ) ).

update_charge_used( aspirar ) :-
    env_charge_used( C1 ),
    C2 is C1 + 1,
    replace( env_charge_used( _ ), env_charge_used( C2 ) ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% percieve( -Percept )                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
percieve( Current_Position, Percept ) :- 
    celda( Current_Position, obs, - ),
    env_current_charge( X ),
    Percept = [1, 0, 0, X].

percieve( Current_Position, Percept ) :- 
    celda( Current_Position, alf, limpio ),
    env_current_charge( X ),
    Percept = [0, 0, 1, X].

percieve( Current_Position, Percept ) :- 
    celda( Current_Position, alf, sucio ),
    env_current_charge( X ),
    Percept = [0, 1, 1, X].

percieve( Current_Position, Percept ) :- 
    celda( Current_Position, bal, limpio ),
    env_current_charge( X ),
    Percept = [0, 0, 0, X].

percieve( Current_Position, Percept ) :- 
    celda( Current_Position, bal, sucio ),
    env_current_charge( X ),
    Percept = [0, 1, 0, X].

percieve( Current_Position, Percept ) :- 
    celda( Current_Position, home, - ),
    env_current_charge( X ),
    Percept = [0, 0, 0, X].

    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% next_charge( [+Row, +Column], -Next_Charge )                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
next_charge( [F, C], Next_Charge ) :-
    current_charge( Current_Charge ),
    (
        (
            celda( [ F, C ], bal, _ ),
            Next_Charge is Current_Charge - 1
        ) ;
        ( 
            celda( [ F, C ], alf, _ ),
            Next_Charge is Current_Charge - 2
        ) ;
        ( 
            celda( [ F, C ], home, _ ),
            battery_charge( B ), 
            Next_Charge is B
        ) ;
        (
            celda( [ F, C ], obs, - ),
            Next_Charge is Current_Charge - 1
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% terrain_cost( +Position, -Cost )                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terrain_cost( Position, Cost ) :-
    celda( Position, Terrain, _State ),
    terrain_cost( Terrain, Cost ).

terrain_cost( home, 1 ).
terrain_cost( bal,  1 ).
terrain_cost( alf,  2 ).
