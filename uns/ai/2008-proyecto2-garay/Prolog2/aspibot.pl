:- consult( 'auxpred.pl' ).
:- consult( 'search.pl' ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                       AGENT INTERNAL STATE                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic agt_world_rep/3.         % Coleccion de agt_world_rep(Pos, Tipo, Estado)
:- dynamic agt_home_position/1.     % Maintains the agent's home position.
:- dynamic agt_current_position/1.  % Maintains the agent's current position.
:- dynamic agt_previous_position/1. % Maintains the agent's previous position.
:- dynamic agt_last_action/1.       % Maintains the agent's last action.
:- dynamic agt_intention/1.         % Maintains the agent's intention.
:- dynamic agt_frontier/1.          % Maintains a list of positions to be explored.
:- dynamic thought/1.               % Maintains the agent'ts latest thought. 
:- dynamic is_goal/1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% aspibot_setup( +Agent_Position )                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
aspibot_setup( Agent_Position ) :- 
    retractall( agt_world_rep( _ )                      ),
    retractall( agt_home_position( _ )                  ),
    retractall( agt_current_position( _ )               ),
    retractall( agt_previous_position( _ )              ),
    retractall( agt_last_action( _ )                    ),
    retractall( agt_intention( _ )                      ),
    retractall( agt_frontier( _ )                       ),
    retractall( thought( _ )                            ),
    assert(     agt_world_rep( Agent_Position, home, -) ),
    assert(     agt_agents_frontier( [] )               ),
    assert(     agt_home_position( Agent_Position )     ),
    assert(     agt_current_position( Agent_Position )  ),
    assert(     agt_previous_position( _ )              ),
    assert(     agt_last_action( none )                 ),
    assert(     agt_intention( explorar )               ),
    assert(     agt_frontier( [] )                      ),
    assert(     thought( '' )                           ).

    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% aspibot( +Percept, -Action )                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-01-----------------------------------------------------------------------------------------------%
% En la situacion inicial, el agente recupera su posicion actual (la cual sera home) agrega a la 
% frontera de celdas a explorar (inicialmente vacia) los vecinos de home, y decide tomer el primero 
% y avanzar hacia adelante.

aspibot( init, adelante ) :-

    print('01'),nl,
    % No es necesario actualizar la posicion actual del agente porque aun no se ha movido.
    agt_current_position( Current_Position ),
    
    % Calcula la proxima posicion.
    next_position( Current_Position, adelante, Next_Position ),

    % Actualiza la frontera de exploracion, eliminando el primer elemento. 
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, [_|New_Frontier] ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),

    % Actualiza la ultima accion para el siguiente turno. 
    replace_all( agt_last_action( _ ), agt_last_action( adelante ) ),

    % Actualiza el pensamiento. 
    update_thought( '01: Comienzo a limpiar hacia adelante.', 
                    init, 
                    Current_Position, 
                    Next_Position, 
                    adelante, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-02-----------------------------------------------------------------------------------------------%
% El agente detecta que choco, y se dirige a una posicion en su frontera que no es adyacente.
% Ya que choco y no se movio, la posicion anterior refleja la posicion actual (1), por lo cual no 
% es necesario actualizarla. 
% Calcula la posicion del obstaculo contra el cual choco (2).
% Recupera el primer elemento de la frontera de exploracion (3), pero no lo elimina de la frontera
% (porque aun no llego a el).
% Si no es adyacente, obtiene la posicion inmediatamente ADELANTE a la posicion recuperada de la 
% frontera (4).
% Si esta posicion no es un obstaculo, calcula un camino mediante A* desde la posicion 
% actual (1) hasta la posicion (4) inmediatamente adelante del destino recuperado de la frontera,
% y de este camino recupera la primera posicion (4). 
% Si el costo del camino es menor o igual que la energia actual, calcula la direccion en la cual 
% se encuentra (5) la primera posicion del camino, y la devuelve como la accion tomada por el 
% agente, actualizando el valor de ultima accion, y tomando nota en su representacion interna de la
% habitacion que la posicion (6) contiene un obstaculo. 

aspibot( [1, Sucio, Alfombra, Energia], Action ) :-
    print('02'),nl,
    
    agt_intention( explorar ),

    % Ya que choco, la posicion actual del agente del turno anterior
    % refleja su verdadera posicion, ya que no se movio.
    agt_current_position( Current_Position ),
    agt_last_action( Last_Action ),
    next_position( Current_Position, Last_Action, Obs_Position ),

    % Recupera el primer elemento de la frontera.
    agt_frontier( [[F1, C1]|_Frontier] ),

    % Si la posicion recuperada de la frontera no es adyacente...
    \+ (direction( Current_Position, [F1, C1], _ ) ),

    % ...calcula una posicion adyacente ADELANTE a esa
    % (porque los elementos de la frontera no han sido explorados aun)...
    F2 is F1 - 1,

    % ...y si esa posicion no es obstaculo...
    agt_world_rep( [F2, C1], Terrain, _ ),
    Terrain \= obs,

    % ...busca un camino desde la posicion actual hasta esa nueva posicion,
    % recuperando la primera posicion de ese camino, que seguramente es adyacente.
    search( Current_Position, [F2, C1], Cost, [Next_Position|_] ),

    % Si el costo de ese camino es menor o igual que la carga actual...
    Cost =< Energia,

    % ...el agente se dirige hacia alli.
    direction( Current_Position, Next_Position, Action ),
    
    % Actualiza la ultima accion, registrando la accion tomada. 
    replace_all( agt_last_action(_), agt_last_action( Action ) ),
    
    % Actualiza la representacion interna de la habitacion, registrando la posicion del obstaculo. 
    replace_all( agt_world_rep( Obs_Position, _, _ ), agt_world_rep( Obs_Position, obs, - ) ),

    % Actualiza el pensamiento.
    update_thought( '02: Choque, dirigiendome una posicion no adyacente mediante A*.',
                    [1, Sucio, Alfombra, Energia],
                    Current_Position,
                    Next_Position,
                    Action,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-03-----------------------------------------------------------------------------------------------%
% Igual que 02, pero en direccion ATRAS. 

aspibot( [1, Sucio, Alfombra, Energia], Action ) :-
    print('03'),nl,
    agt_intention( explorar ),
    agt_current_position( Current_Position ),
    agt_last_action( Last_Action ),
    next_position( Current_Position, Last_Action, Obs_Position ),
    agt_frontier( [[F1, C1]|_Frontier] ),
    \+ (direction( Current_Position, [F1, C1], _ ) ),
    F2 is F1 + 1,
    agt_world_rep( [F2, C1], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F2, C1], Cost, [Next_Position|_] ),
    Cost =< Energia,
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_last_action(_), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Obs_Position, _, _ ), agt_world_rep( Obs_Position,obs,-) ),
    update_thought( '03: Choque, dirigiendome una posicion no adyacente usando A*.',
                    [1, Sucio, Alfombra, Energia],
                    Current_Position,
                    Next_Position,
                    Action,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-04-----------------------------------------------------------------------------------------------%
% Igual que 02, pero en direccion DERECHA. 

aspibot( [1, Sucio, Alfombra, Energia], Action ) :-
    print('04'),nl,
    agt_intention( explorar ),
    agt_current_position( Current_Position ),
    agt_last_action( Last_Action ),
    next_position( Current_Position, Last_Action, Obs_Position ),
    agt_frontier( [[F1, C1]|_] ),
    \+ (direction( Current_Position, [F1, C1], _ ) ),
    C2 is C1 + 1,
    agt_world_rep( [F1, C2], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F1, C2], Cost, [Next_Position|_] ),
    Cost =< Energia,
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_last_action(_), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Obs_Position, _, _ ), agt_world_rep( Obs_Position,obs,-) ),
    update_thought( '04: Choque, dirigiendome una posicion no adyacente mediante A*.',
                    [1, Sucio, Alfombra, Energia],
                    Current_Position,
                    Next_Position,
                    Action,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-05-----------------------------------------------------------------------------------------------%
% Igual que 02, pero en direccion IZQUIERDA. 

aspibot( [1, Sucio, Alfombra, Energia], Action ) :-
    print('05'),nl,
    agt_intention( explorar ),
    agt_current_position( Current_Position ),
    agt_last_action( Last_Action ),
    next_position( Current_Position, Last_Action, Obs_Position ),
    agt_frontier( [[F1, C1]|_] ),
    \+ (direction( Current_Position, [F1, C1], _ ) ),
    C2 is C1 - 1,
    agt_world_rep( [F1, C2], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F1, C2], Cost, [ Next_Position | _ ] ),
    Cost =< Energia,
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_last_action(_), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Obs_Position, _ , _ ), agt_world_rep( Obs_Position, obs, - ) ),
    update_thought( '05: Choque, dirigiendome una posicion no adyacente mediante A*.',
                    [1, Sucio, Alfombra, Energia],
                    Current_Position,
                    Next_Position,
                    Action,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-06---------------------------------------------------------------------------------------------%
% El agente detecta que choco. 
% Ya que choco y no se movio, la posicion anterior refleja la posicion actual (1), por lo cual no 
% es necesario actualizarla. 
% Calcula la posicion del obstaculo contra el cual choco (2).
% Recupera la posicion de home (3).
% Realiza una busqueda desde su posicion actual (1) hasta la posicion de home (3).
% Si el costo mas cuatro es MENOR a su carga actual, 
% recupera el primer elemento de la frontera (4), la cual sera adyacente a su posicion actual (1),
% calcula la direccion en la cual debe dirigirse, 
% y actualiza su representacion interna de la habitacion, registrando la posicion del obstaculo (2),
% su ultima accion, y eliminando el elemento recuperado de la frontera. 

aspibot( [1, Sucio, Alfombra, Energia], Action ) :-
    print('06'),nl,

    agt_intention( explorar ),

    % Ya que choco, la posicion actual del agente del turno anterior 
    % refleja su verdadera posicion, ya que no se movio. 
    agt_current_position( Current_Position ),
    agt_last_action( Last_Action ),
    next_position( Current_Position, Last_Action, Obs_Position ),

    % Recuperar la posicion de home. 
    agt_home_position( Home_Position ),
    
    % Calcular el costo del camino desde la posicion actual hasta home. 
    search( Current_Position, Home_Position, Cost, _),
    Y is Cost + 4,
    Y =< Energia,
    
    % Recuperar la proxima posicion a explorar de la frontera.
    agt_frontier( [Next_Position|New_Frontier] ),

    % Calcular la direccion en que se encuentra.
    direction( Current_Position, Next_Position, Action ),

    % Actualiza la representacion interna de la habitacion, registrando la posicion del obstaculo. 
    replace_all( agt_world_rep( Obs_Position, _, _ ), agt_world_rep( Obs_Position, obs, -) ),
    
    % Actualiza la frontera, eliminando la posicion recuperada. 
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),
    
    % Actualiza la ultima accion, registrando la accion tomada. 
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    
    % Actualiza el pensamiento.
    update_thought( '06: Choque, dirigiendome a una posicion adyacente. Uso A* para asegurarme que me alcanza la energia.', 
                    [1, Sucio, Alfombra, Energia], 
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-07---------------------------------------------------------------------------------------------%
% El agente detecta que choco. 
% Ya que choco y no se movio, la posicion anterior refleja la posicion actual (1), por lo cual no 
% es necesario actualizarla. 
% Calcula la posicion del obstaculo contra el cual choco (2).
% Recupera la posicion de home (3).
% Realiza una busqueda desde su posicion actual (1) hasta la posicion de home (3).
% Si el costo mas cuatro es MAYOR a su carga actual, 
% recupera la primera posicion del camino a home (4), la cual sera adyacente a su posicion 
% actual (1), calcula la direccion en la cual debe dirigirse, 
% y actualiza su representacion interna de la habitacion, registrando la posicion del obstaculo (2),
% actualiza su ultima accion, y cambia su intencion a 'volver'. 

aspibot( [1, Sucio, Alfombra, Energia], Action ) :-
    print('07'),nl,
    
    agt_intention( explorar ),

    % Ya que choco, la posicion actual del agente del turno anterior 
    % refleja su verdadera posicion, ya que no se movio. 
    agt_current_position( Current_Position ),
    agt_last_action( Last_Action ),
    next_position( Current_Position, Last_Action, Obs_Position ),

    % Recupera la posicion de home.
    agt_home_position( Home_Position ),

    % Calcular el costo del camino desde la posicion actual hasta home.
    search( Current_Position, Home_Position, Cost, [Next_Position|_]),

    % Si el costo mas cuatro es mayor a su energia...
    Y is Cost + 4,
    Y > Energia,
    
    % Calcula la direccion en que se encuentra la primera posicion del camino hacia home. 
    direction( Current_Position, Next_Position, Action ),

    % Actualiza la representacion interna de la habitacion, registrando la posicion del obstaculo. 
    replace_all( agt_world_rep( Obs_Position, _, _ ), agt_world_rep( Obs_Position, obs, -) ),

    % Actualiza la ultima accion, registrando la accion tomada. 
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),

    % Cambia su intencion a volver a casa.
    replace_all( agt_intention( _ ), agt_intention( volver ) ),

    % Actualiza el pensamiento.
    update_thought( '07: Choque, dirigiendome a home mediante A*.',
                    [1, Sucio, Alfombra, Energia],
                    Current_Position,
                    Next_Position,
                    Action,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-08-----------------------------------------------------------------------------------------------%
% El agente se queda sin bateria sobre una baldosa limpia y se apaga. 
% Como se movio, la posicion actual almacenada no refleja su posicion real, luego 
% si la posicion actual no es igual a la posicion anterior, actualiza la posicion actual.
% Actualiza la ultima accion, la cual sera apagar.
% Actualzia la representacion interna de la habiatacion, de manera que refleje que la posicion 
% actual contiene una baldosa LIMPIA, y actualiza el pensamiento. 

aspibot( [ 0, 0, 0, 0], apagar ) :-
    print('08'),nl,

    % Actualiza la posicion actual.
    % Como se movio, la posicion actual almacenada no refleja su posicion real. 
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    Current_Position \= Prev_Position,
    replace_all( agt_current_position(_  ), agt_current_position( Current_Position ) ),

    % Actualiza la ultima accion.
    replace_all( agt_last_action( _ ), agt_last_action( apagar ) ),

    % Actualiza la representacion interna de la habitacion.
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, limpio ) ),
    
    % Actualiza el pensamiento. 
    update_thought( '08: Low power, apagandome sobre una baldosa limpia.', 
                    [0, 0, 0, 0], 
                    Prev_Position, 
                    Current_Position, 
                    apagar, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-09-----------------------------------------------------------------------------------------------%
% El agente se queda sin bateria sobre una baldosa sucia y se apaga. 
% Como se movio, la posicion actual almacenada no refleja su posicion real, luego 
% si la posicion actual no es igual a la posicion anterior, actualiza la posicion actual.
% Actualiza la ultima accion, la cual sera apagar.
% Actualzia la representacion interna de la habiatacion, de manera que refleje que la posicion 
% actual contiene una baldosa SUCIA, y actualiza el pensamiento. 

aspibot( [ 0, 1, 0, 0], apagar ) :-
    print('09'),nl,
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    Current_Position \= Prev_Position,
    
    replace_all( agt_current_position(_  ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( apagar ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, sucio ) ),
    update_thought( '09: Low power, apagandome sobre una baldosa sucia.', 
                    [0, 1, 0, 0], 
                    Prev_Position, 
                    Current_Position, 
                    apagar, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-10-----------------------------------------------------------------------------------------------%
% El agente se queda sin bateria sobre una alfombra limpia y se apaga. 
% Como se movio, la posicion actual almacenada no refleja su posicion real, luego 
% si la posicion actual no es igual a la posicion anterior, actualiza la posicion actual.
% Actualiza la ultima accion, la cual sera apagar.
% Actualzia la representacion interna de la habiatacion, de manera que refleje que la posicion 
% actual contiene una ALFOMBRA LIMPIA, y actualiza el pensamiento. 

aspibot( [ 0, 0, 1, 0], apagar ) :-
    print('10'),nl,
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    Current_Position \= Prev_Position,
    
    replace_all( agt_current_position(_  ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( apagar ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, limpio ) ),
    update_thought( '10: Low power, apagandome sobre una alfombra limpia.', 
                    [0, 0, 1, 0], 
                    Prev_Position, 
                    Current_Position, 
                    apagar, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-11-----------------------------------------------------------------------------------------------%
% El agente se queda sin bateria sobre una alfombra sucia y se apaga. 
% Como se movio, la posicion actual almacenada no refleja su posicion real, luego 
% si la posicion actual no es igual a la posicion anterior, actualiza la posicion actual.
% Actualiza la ultima accion, la cual sera apagar.
% Actualzia la representacion interna de la habiatacion, de manera que refleje que la posicion 
% actual contiene una ALFOMBRA SUCIA, y actualiza el pensamiento. 

aspibot( [ 0, 1, 1, 0], apagar ) :-
    print('11'),nl,
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    Current_Position \= Prev_Position,

    replace_all( agt_current_position(_  ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( apagar ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, sucio ) ),
    update_thought( '11: Low power, apagandome sobre una alfombra sucia.', 
                    [0, 1, 1, 0], 
                    Prev_Position, 
                    Current_Position, 
                    apagar, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.
    
%-12-----------------------------------------------------------------------------------------------%
% El agente choca, se queda sin carga y se apaga.
% Ya que choco y no se movio, la posicion anterior refleja la posicion actual (1), por lo cual no 
% es necesario actualizarla. 
% Actualiza su ultima accion, registrando que se apaga,
% actualiza su representacion interna, registrando la posicion del obstaculo.

aspibot( [ 1, Sucio, Alfombra, 0], apagar ) :-
    print('12'),nl,
    
    % Ya que choco, la posicion actual del agente del turno anterior
    % refleja su verdadera posicion, ya que no se movio.
    agt_current_position( Current_Position ),
    agt_last_action( Last_Action ),
    next_position( Current_Position, Last_Action, Obs_Position ),

    % Actualiza la ultima accion, registrando que se apaga. 
    replace_all( agt_last_action( _ ), agt_last_action( apagar ) ),

    % Actualiza la representacion interna de la habitacion, registrando la posicion del obstaculo. 
    replace_all( agt_world_rep( Obs_Position, _, _ ), agt_world_rep( Obs_Position, obs, - ) ),
    
    % Actualizar el pensamiento del agente. 
    update_thought( '12: Low power, apagandome despues de chocar.', 
                    [1, Sucio, Alfombra, 0], 
                    Current_Position, 
                    Obs_Position, 
                    apagar, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-13-----------------------------------------------------------------------------------------------%
% El agente se queda sin bateria despues de aspirar.
% Ya que aspiro y no se movio, la posicion anterior refleja la posicion actual (1), por lo cual no 
% es necesario actualizarla. 

aspibot( [ 0, 0, Alfombra, 0], apagar ) :-
    print('13'),nl,
    
    % Verifica que la ultima accion fue aspirar.
    agt_last_action( aspirar ),
    
    % Ya que aspiro, la posicion actual del agente del turno anterior
    % refleja su verdadera posicion, ya que no se movio.
    agt_current_position( Current_Position ),
    next_position( Current_Position, aspirar, Next_Position ),

    % Actualiza la ultima accion.
    replace_all( agt_last_action( _ ), agt_last_action( apagar ) ),

    % Actualiza el pensamiento del agente. 
    update_thought( '13: Low power, apagandome despues de aspirar.', 
                    [0, 0, Alfombra, 0], 
                    Current_Position, 
                    Next_Position, 
                    apagar, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.
    
%-15-----------------------------------------------------------------------------------------------%
% El agente se encuentra en home y tiene una frontera vacia, 
% ya exploro y aspiro todo el mundo explorable, luego se apaga.

aspibot( [ Choco, 0, 0, Energia], apagar ) :-
    print('15'),nl,
    
    % Verifica que la frontera es vacia.
    agt_frontier( [] ),

    % Calcula la posicion actual.
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    
    % Verifica que la posicion actual es la de home. 
    agt_home_position( Current_Position ),

    % Actualiza el pensamiento del agente. 
    update_thought( '15: Estoy en home y no hay nada por explorar. Apagandome.',
                    [Choco, 0, 0, Energia], 
                    Current_Position, 
                    Current_Position,
                    apagar,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-16-----------------------------------------------------------------------------------------------%
% El agente se encuentra en una BALDOSA SUCIA y aspira. 
% Busca un camino de a home mediante A* y si su energia es menor al costo del camino mas el costo 
% de aspirar, aspira. 
% Actualiza su representacion interna para reflejar la baldosa sucia. 
% Actualiza su posicion actual, agrega a la frontera los vecinos de la posicion actual para que 
% luego sean explorados, actualiza su ultima accion, y actualiza su representacion interna para 
% reflejar que la baldosa quedo limpia,y por ultimo actualiza su pensamiento.

aspibot( [0, 1, 0, Energia], aspirar ) :-
    print('16'),nl,
    
    agt_intention( explorar ),
    
    % Calcula la posicion actual.
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),

    % Busca un camino a home. 
    agt_home_position( Home_Position ), 
    search( Current_Position, Home_Position, Cost, _Path ), 

    % Si el costo del camino a home mas uno es menor que la carga actual...
    Y is Cost + 1,
    Y =< Energia,

    % Actualiza la representacion interna de la habitacion del agente. 
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, sucio ) ),

    % ...actualiza la posicion del agente...
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    
    % ...agrega las posiciones adyacentes a la frontera para ser exploradas luego...
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, New_Frontier ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),

    % ...actualizar la ultima accion del agente...
    replace_all( agt_last_action( _ ), agt_last_action( aspirar ) ),
    
    % ...y registrar que despues de aspirar quedo limpia la baldosa.
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, limpio)),

    % Actualizar el pensamiento del agente. 
    update_thought( '16: Estoy en una baldosa sucia. Uso A* para asegurarme que me alcanza la energia. Voy a aspirar.', 
                    [0, 1, 0, Energia], 
                    Prev_Position, 
                    Current_Position, 
                    Last_Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.
    
%-17-----------------------------------------------------------------------------------------------%
% El agente se encuentra en una ALFOMBRA SUCIA y aspira. 
% Busca un camino de a home mediante A* y si su energia es menor al costo del camino mas el costo 
% de aspirar, aspira. 
% Actualiza su representacion interna para reflejar la alfombra sucia. 
% Actualiza su posicion actual, agrega a la frontera los vecinos de la posicion actual para que 
% luego sean explorados, actualiza su ultima accion, y actualiza su representacion interna para 
% reflejar que la alfombra quedo limpia,y por ultimo actualiza su pensamiento.

aspibot( [0, 1, 1, Energia], aspirar ) :-
    print('17'),nl,
    agt_intention( explorar ),
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _Path ),
    Y is Cost + 1,
    Y =< Energia,
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, sucio ) ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, New_Frontier ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),
    replace_all( agt_last_action( _ ), agt_last_action( aspirar ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, limpio)),
    update_thought( '17: Estoy en una alfombra sucia. Uso A* para asegurarme que me alcanza la energia. Voy a aspirar.', 
                    [0, 1, 1, Energia], 
                    Prev_Position,
                    Current_Position,
                    Last_Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-20-----------------------------------------------------------------------------------------------%
% El agente se encuentra en una BALDOSA LIMPIA y no tiene suficiente bateria para continuar 
% explorando, luego intenta volver a home.
% Calcula la posicion actual, calcula un camino desde la posicion actual hasta home. 
% Si el costo del camino mas el peor caso (moverse a una alfombra) es mayor o igual que la energia, 
% actualiza la frontera agregando los vecinos de la posicion actual para explorarlos luego, 
% actualiza la posicion actual, cambia su intencion a 'volver', calcula la direccion en la cual debe 
% dirigirse para llegar a home y la registra como la ultima accion realizada. 
% Por ultimo actualiza su representacion interna de la habitacion y su pensamiento. 

aspibot( [0, 0, 0, Energia], Action ) :-
    print('20'),nl,
    agt_intention( explorar ),
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, [[F1, C1]|_Path] ),
    Y is Cost + 3,
    Y >= Energia,
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, New_Frontier ),
    direction( Current_Position, [F1, C1], Action ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ), agt_intention( volver ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, limpio ) ),
    update_thought( '20: Estoy en una baldosa limpia, no tengo suficiente energia para explorar. Vuelvo a home mediante A*.', 
                    [0, 0, 0, Energia], 
                    Prev_Position, 
                    Current_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-19-----------------------------------------------------------------------------------------------%
% El agente se encuentra en una BALDOSA LIMPIA y tiene suficiente bateria para continuar explorando.
% La proxima posicion a explorar en la frontera es adyacente.
% Calcula la posicion actual, realiza una busqueda mediante A* desde la posicion actual hasta 
% home, y si el costo mas el costo del peor caso (moverse a una alfombra) es menor que su energia,
% efectua el movimiento.
% Calcula la direccion en que debe moverse, la devuelve como la ultima accion realizada, y actualiza 
% la frontera agregandole los vecinos de la posicion actual para ser explorados luego. 
% Luego actualiza el pensamiento.

aspibot( [0, 0, 0, Energia], Action ) :-
    print('19'),nl,
    agt_intention( explorar ),
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _Path ),
    Y is Cost + 3,
    Y =< Energia,
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, [Next_Position|New_Frontier] ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _ , _ ), agt_world_rep( Current_Position, bal, limpio ) ),
    update_thought( '19: Estoy en una baldosa limpia. Uso A* para asegurarme que me alcanza la energia. Voy a moverme.', 
                    [0, 0, 0, Energia], 
                    Current_Position, 
                    Next_Position, 
                    Last_Action,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-21-----------------------------------------------------------------------------------------------%
% El agente se encuentra en una baldosa limpia y tiene suficiente bateria para continuar explorando.
% La proxima posicion a explorar en la frontera no es adyacente.
% Calcula la posicion actual, si la proxima posicion a explorar de la frontera no es adyacente, 
% calcula la posicion de la posicion inmediatamente ADELANTE a la posicion a explorar.
% Si la posicion calculada no es un obstaculo, calcula un camino desde esa posicion hasta home. 
% Si el costo del camino mas el peor caso (moverse a una alfombra) es menor a su energia, 
% realiza otra busqueda mediante A* desde la posicion actual hasta la posicion destino, recupera la
% primera posicion, calcula la direccion en la que queda, y la registra como la ultima accion.
% Luego actualiza la posicion actual, y registra en su representacion interna que limpio la baldosa.

aspibot( [0, 0, 0, Energia ], Action ) :-
    print('21'),nl,
    agt_intention( explorar ), 
    
    % Calcula la posicion actual.
    agt_current_position( Prev_Position ), 
    agt_last_action( Last_Action ), 
    next_position( Prev_Position, Last_Action, Current_Position ), 

    % ...si el primer elemento de la frontera no es adyacente...
    agt_frontier( [[F2, C2]|_Frontier] ), 
    \+ direction( Current_Position, [F2, C2], Action ),
    
    % ...calcula un vecino adelante del elemento de la frontera...
    F3 is F2 - 1, 
    
    % si la posicion calcula no es un obstaculo...
    agt_world_rep( [F3, C2], Terrain, _ ), 
    Terrain \= obs, 
    
    % ...y el costo de moverse de ahi hasta home...
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _Path1 ),
    Y is Cost + 3,
    Y =< Energia,
    
    % ...realiza una busqueda desde la posicion actual hasta la posicion destino...
    search( Current_Position, [F3, C2], _Cost, [Next_Position|_Path2] ),

    % ...calcula la direccion en la que queda...
    direction( Current_Position, Next_Position, Action ),
    
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, limpio ) ), 

    % Actualizar el pensamiento del agente. 
    update_thought( '21: Estoy en una baldosa limpia. Uso A* para asegurarme que me alcanza la energia. Voy a moverme.', 
                    [0, 0, 0, Energia], 
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.
    
%-22-----------------------------------------------------------------------------------------------%
aspibot( [0, 0, 0, Energia ], Action ) :-
    print('22'),nl,
    agt_intention( explorar ), 
    agt_current_position( Prev_Position ), 
    agt_last_action( Last_Action ), 
    next_position( Prev_Position, Last_Action, Current_Position ), 
    agt_frontier( [[F2, C2]|_Frontier] ), 
    \+ direction( Current_Position, [F2, C2], Action ),
    F3 is F2 + 1, 
    agt_world_rep( [F3, C2], Terrain, _ ), 
    Terrain \= obs, 
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _Path1 ),
    Y is Cost + 3,
    Y =< Energia,
    search( Current_Position, [F3, C2], _Cost, [Next_Position|_Path2] ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, limpio ) ), 
    update_thought( '22: Estoy en una baldosa limpia. Uso A* para asegurarme que me alcanza la energia. Voy a moverme.', 
                    [0, 0, 0, Energia], 
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.
  
%-23-----------------------------------------------------------------------------------------------%
aspibot( [0, 0, 0, Energia ], Action ) :-
    print('23'),nl,
    agt_intention( explorar ), 
    agt_current_position( Prev_Position ), 
    agt_last_action( Last_Action ), 
    next_position( Prev_Position, Last_Action, Current_Position ), 
    agt_frontier( [[F2, C2]|_Frontier] ), 
    \+ direction( Current_Position, [F2, C2], Action ),
    C3 is C2 + 1, 
    agt_world_rep( [F2, C3], Terrain, _ ), 
    Terrain \= obs, 
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _Path1 ),
    Y is Cost + 3,
    Y =< Energia, 
    search( Current_Position, [F2, C3], _Cost, [Next_Position|_Path2] ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, limpio ) ), 
    update_thought( '23: Estoy en una baldosa limpia. Uso A* para asegurarme que me alcanza la energia. Voy a moverme.', 
                    [0, 0, 0, Energia], 
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.  
 
%-24-----------------------------------------------------------------------------------------------%
aspibot( [0, 0, 0, Energia ], Action ) :-
    print('24'),nl,
    agt_intention( explorar ), 
    agt_current_position( Prev_Position ), 
    agt_last_action( Last_Action ), 
    next_position( Prev_Position, Last_Action, Current_Position ), 
    agt_frontier( [[F2, C2]|_Frontier] ), 
    \+ direction( Current_Position, [F2, C2], Action ),
    C3 is C2 - 1, 
    agt_world_rep( [F2, C3], Terrain, _ ), 
    Terrain \= obs, 
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _Path1 ),
    Y is Cost + 3,
    Y =< Energia,
    search( Current_Position, [F2, C3], _Cost, [Next_Position|_Path2] ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, limpio ) ), 
    update_thought( '24: Estoy en una baldosa limpia. Uso A* para asegurarme que me alcanza la energia. Voy a moverme.', 
                    [0, 0, 0, Energia], 
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-26------------------------------------------------------------------------------------------------%
% El agente se encuentra en home y tiene una frontera no vacia. 
% La proxima posicion a explorar es adyacente.

aspibot( [ 0, 0, Alfombra, Energia ], Action ):-
    print('26'),nl,
    
    % Calcula la posicion actual.
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),

    % Verifica que la posicion actual es la de home. 
    agt_home_position( Current_Position ),
    
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, [ Next_Position | New_Frontier ] ),
    
    direction( Current_Position, Next_Position, Action ),
    
    replace_all( agt_intention( _ ), agt_intention( explorar ) ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    
    string_concat( '26: Estoy en home y tengo que seguir explorando.', String ),
    update_thought( String, 
                    [0, 0, Alfombra, Energia],
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-27-----------------------------------------------------------------------------------------------%
% El agente se encuentra en home y tiene una frontera no vacia. 
% La proxima posicion a explorar no es adyacente.
% Calcula la posicion actual y verifica que es home. 
% Recupera el proximo destino de la frontera. 
% Si no es adyacente a la posicion actual, calcula la posicion inmediatamente ADELANTE a la 
% posicion destino, y si no es un obstaculo, calcula un camino mediante A* desde la posicion 
% actual hasta la posicion obtenida. 
% Si el costo de moverse a la primera posicion del camino calculado mas el peor caso (moverse a una
% alfombra) es menor a la energia actual, calcula la direccion en la cual debe moverse, 
% registra la posicion actual, cambia la intencion a explorar, y registra la direccion tomada como
% la ultima accion tomada. 

aspibot( [ 0, 0, Alfombra, Energia] , Action ):-
    print('27'),nl,
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Current_Position ),
    agt_frontier( [[F, C]|_Frontier ] ),
    \+ (direction( Current_Position, [F, C], _ ) ),
    F2 is F - 1,
    agt_world_rep( [F2,C], Terrain, _ ),
    Terrain \= obs,
    \+ direction( Current_Position, [F2, C], Action ),
    search( Current_Position, [ F2, C ], Cost, [ Next_Position | _Path ] ),
    Y is Cost + 4,
    Y =< Energia,
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ), agt_intention( explorar ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    update_thought( '27: Estoy en home y tengo que seguir explorando.', 
                    [0, 0, Alfombra, Energia],
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-28-----------------------------------------------------------------------------------------------%
aspibot( [ 0, 0, Alfombra, Energia] , Action ):-
    print('28'),nl,
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Current_Position ),
    agt_frontier( [[F, C]|_Frontier ] ),
    \+ (direction( Current_Position, [F, C], _ ) ),
    F2 is F + 1,
    agt_world_rep( [F2,C], Terrain, _ ),
    Terrain \= obs,
    \+ direction( Current_Position, [ F2 , C ]  ,Action ),
    search( Current_Position, [ F2, C ], Cost, [ Next_Position | _Path ] ),
    Y is Cost + 4,
    Y =< Energia,
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ), agt_intention( explorar ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    update_thought( '28: Estoy en home y tengo que seguir explorando.', 
                    [0, 0, Alfombra, Energia],
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.  
    
%-29-----------------------------------------------------------------------------------------------%
aspibot( [ 0, 0, Alfombra, Energia] , Action ):-
    print('29'),nl,
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Current_Position ),
    agt_frontier( [[F, C] | _Frontier ] ),
    \+ (direction( Current_Position, [F, C], _ ) ),
    C2 is C + 1,
    agt_world_rep( [F,C2], Terrain, _ ),
    Terrain \= obs,
    \+ direction( Current_Position, [ F , C2 ]  ,Action ),
    search( Current_Position, [ F, C2 ], Cost, [ Next_Position | _Path ] ),
    Y is Cost + 4,
    Y =< Energia,
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ), agt_intention( explorar ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    update_thought( '29: Estoy en home y tengo que seguir explorando.',
                    [0, 0, Alfombra, Energia],
                    Current_Position,
                    Next_Position,
                    Action,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-30-----------------------------------------------------------------------------------------------%
aspibot( [ 0, 0, Alfombra, Energia] , Action ):-
    print('30'),nl,
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Current_Position ),
    agt_frontier( [[F, C]| _Frontier ] ),
    \+ (direction( Current_Position, [F, C], _ ) ),
    C2 is C - 1,
    agt_world_rep( [F,C2], Terrain, _ ),
    Terrain \= obs,
    \+ direction( Current_Position, [ F , C2 ], Action ),
    search( Current_Position, [ F, C2 ], Cost, [ Next_Position | _Path2 ] ),
    Y is Cost + 4,
    Y =< Energia,
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ), agt_intention( explorar ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    update_thought( '30: Estoy en home y tengo que seguir explorando.', 
                    [0, 0, Alfombra, Energia],
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-32-----------------------------------------------------------------------------------------------%
% El agente se encuentra en una ALFOMBRA LIMPIA y tiene suficiente bateria para continuar explorando.
% La proxima posicion a explorar en la frontera es adyacente.
% Calcula la posicion actual, realiza una busqueda mediante A* desde la posicion actual hasta 
% home, y si el costo mas el costo del peor caso (moverse a una alfombra) es menor que su energia,
% efectua el movimiento.
% Calcula la direccion en que debe moverse, la devuelve como la ultima accion realizada, y actualiza 
% la frontera agregandole los vecinos de la posicion actual para ser explorados luego. 
% Luego actualiza el pensamiento.

aspibot( [0, 0, 1, Energia], Action ) :-
    print('32'),nl,
    agt_intention( explorar ),
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _Path ),
    Y is Cost + 4,
    Y =< Energia,
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, [ Next_Position | New_Frontier ] ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, limpio ) ),
    update_thought( '32: Estoy en una alfombra limpia, y tengo suficiente energia para explorar y volver a casa.', 
                    [0, 0, 1, Energia], 
                    Current_Position, 
                    Next_Position, 
                    aspirar, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-33-----------------------------------------------------------------------------------------------%
% El agente se encuentra en una ALFOMBRA LIMPIA y no tiene suficiente bateria para continuar 
% explorando, luego intenta volver a home.
% Calcula la posicion actual, calcula un camino desde la posicion actual hasta home. 
% Si el costo del camino mas el peor caso (moverse a una alfombra) es mayor o igual que la energia, 
% actualiza la frontera agregando los vecinos de la posicion actual para explorarlos luego, 
% actualiza la posicion actual, cambia su intencion a 'volver', calcula la direccion en la cual debe 
% dirigirse para llegar a home y la registra como la ultima accion realizada. 
% Por ultimo actualiza su representacion interna de la habitacion y su pensamiento. 

aspibot( [0, 0, 1, Energia], Action ) :-
    print('33'),nl,
    agt_intention( explorar ),
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, [Next_Position|_Path] ),
    Y is Cost + 4,
    Y >= Energia,
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, New_Frontier ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ), agt_intention( volver ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, limpio ) ),
    update_thought( '33: Estoy en una alfombra limpia, no tengo suficiente energia para explorar. Vuelvo a home mediante A*.', , 
                    [0, 0, 1, Energia], 
                    Current_Position,
                    Next_Position,
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.


%-25---------------------------------------------------------------------------------------------------%
% El agente se encuentra en una ALFOMBRA LIMPIA y tiene suficiente bateria para continuar explorando.
% La proxima posicion a explorar en la frontera no es adyacente.
% Calcula la posicion actual, si la proxima posicion a explorar de la frontera no es adyacente, 
% calcula la posicion de la posicion inmediatamente ADELANTE a la posicion a explorar.
% Si la posicion calculada no es un obstaculo, calcula un camino desde esa posicion hasta home. 
% Si el costo del camino mas el peor caso (moverse a una alfombra) es menor a su energia, 
% realiza otra busqueda mediante A* desde la posicion actual hasta la posicion destino, recupera la
% primera posicion, calcula la direccion en la que queda, y la registra como la ultima accion.
% Luego actualiza la posicion actual, y registra en su representacion interna que limpio la baldosa.

aspibot( [0, 0, 1, Energia ], Action ) :-
    print('25'),nl,
    agt_intention( explorar ),
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_frontier( [[F2, C2]| _ ] ),
    \+ direction( Current_Position, [F2, C2], _ ),
    F3 is F2 - 1,
    agt_world_rep( [F3, C2], Terrain, _ ),
    Terrain \= obs,
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _ ),
    Y is Cost + 4,
    Y =< Energia,
    search( Current_Position, [F3, C2], _ , [ Next_Position | _ ] ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ),  intention( explorar ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, limpio ) ),
    update_thought( '25: Estoy en una alfombra limpia. Uso A* para asegurarme que me alcanza la energia. Voy a moverme.', 
                    [0, 0, 1, Energia],
                    Current_Position,
                    Next_Position,
                    Action,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-34-----------------------------------------------------------------------------------------------%
aspibot( [0, 0, 1, Energia ], Action ) :-
    print('34'),nl,
    agt_intention( explorar ), 
    agt_current_position( Prev_Position ), 
    agt_last_action( Last_Action ), 
    next_position( Prev_Position, Last_Action, Current_Position ), 
    agt_frontier( [[F2, C2]|_Frontier] ), 
    \+ direction( Current_Position, [F2, C2], Action ),
    F3 is F2 + 1, 
    agt_world_rep( [F3, C2], Terrain, _ ), 
    Terrain \= obs, 
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _Path1 ),
    Y is Cost + 4,
    Y =< Energia,
    search( Current_Position, [F3, C2], _Cost, [Next_Position|_Path2] ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ),  agt_intention( explorar ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, limpio ) ),
    update_thought( '34: Estoy en una alfombra limpia. Uso A* para asegurarme que me alcanza la energia. Voy a moverme.', 
                    [0, 0, 1, Energia], 
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.
  
%-35-----------------------------------------------------------------------------------------------%
aspibot( [0, 0, 1, Energia ], Action ) :-
    print('35'),nl,
    agt_intention( explorar ), 
    agt_current_position( Prev_Position ), 
    agt_last_action( Last_Action ), 
    next_position( Prev_Position, Last_Action, Current_Position ), 
    agt_frontier( [[F2, C2]|_Frontier] ), 
    \+ direction( Current_Position, [F2, C2], Action ),
    C3 is C2 + 1, 
    agt_world_rep( [F2, C3], Terrain, _ ), 
    Terrain \= obs, 
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, _Path1 ),
    Y is Cost + 4,
    Y =< Energia,
    search( Current_Position, [F2, C3], Cost, [Next_Position|_Path2] ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ),  agt_intention( explorar ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, limpio ) ), 
    update_thought( '35: Estoy en una alfombra limpia. Uso A* para asegurarme que me alcanza la energia. Voy a moverme.', 
                    [0, 0, 1, Energia], 
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.  
 
%-36------------------------------------------------------------------------------------------------%
aspibot( [0, 0, 1, Energia ], Action ) :-
    print('36'),nl,
    agt_intention( explorar ), 
    agt_current_position( Prev_Position ), 
    agt_last_action( Last_Action ), 
    next_position( Prev_Position, Last_Action, Current_Position ), 
    agt_frontier( [[F2, C2]|_Frontier] ), 
    \+ direction( Current_Position, [F2, C2], Action ),
    C3 is C2 - 1, 
    agt_world_rep( [F2, C3], Terrain, _ ), 
    Terrain \= obs, 
    search( Current_Position, [F2, C3], Cost, _Path1 ),
    Y is Cost + 4,
    Y =< Energia,
    search( Current_Position, [F2, C3], Cost, [Next_Position|_Path2] ),
    direction( Current_Position, Next_Position, Action ),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_intention( _ ),  agt_intention( explorar ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, limpio ) ), 
    update_thought( '36: Estoy en una alfombra limpia. Uso A* para asegurarme que me alcanza la energia. Voy a moverme.', 
                    [0, 0, 1, Energia], 
                    Current_Position, 
                    Next_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.
    
%-37------------------------------------------------------------------------------------------------%
% El agente tiene como intencion 'volver'.
% Calcula la posicion actual, busca un camino a casa mediante A*, calcula la direccion en la cual 
% debe moverse para llegar a la primera posicion del camino encontrado, si el costo es menor que la 
% energia, devuelve la direccion como accion, y la registra como la ultima accion tomada.

aspibot( [0, Sucio, Terrain, Energy ], Action ) :-
    print('37'),nl,
    agt_intention( volver ),
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, [ Next_Position |_Path ] ),
    direction( Current_Position, Next_Position, Action ),
    Cost =< Energy,
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    update_thought( '37: Estoy volviendo a home.', 
                    [0, 0, Terrain, Energy], 
                    Prev_Position, 
                    Current_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-38------------------------------------------------------------------------------------------------%
% El agente se encuentra en una celda cualquiera, y tiene la frontera vacia. Vuelve a home. 
% Calcula la posicion actual, busca un camino a casa mediante A*, calcula la direccion en la cual 
% debe moverse para llegar a la primera posicion del camino encontrado, devuelve la direccion como 
% accion, y la registra como la ultima accion tomada, registra su posicion actual, y cambia su 
% intencion a 'volver'.
aspibot( [ 0, Sucio, Alfombra, Energia], Action ):-
    print('38'),nl,
    agt_intention( explorar ),
    agt_frontier( [] ),
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, _Cost, [ Next_Position | _Path ] ),
    direction( Current_Position, Next_Position, Action),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_intention( _ ), agt_intention( volver ) ),
    update_thought( '38: No tengo mas nada que hacer, vuelvo a home mediante A*.', 
                    [0, Sucio, Alfombra, Energia],
                    Current_Position,
                    Next_Position,
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-39------------------------------------------------------------------------------------------------%
% El agente choco y tiene la frontera vacia. Vuelve a home. 
% Calcula la posicion actual, busca un camino a casa mediante A*, calcula la direccion en la cual 
% debe moverse para llegar a la primera posicion del camino encontrado, devuelve la direccion como 
% accion, y la registra como la ultima accion tomada, actualiza su representacion interna de la 
% habitacion con la posicion del obstaculo, y cambia su intencion a volver.
aspibot( [ 1, Sucio, Alfombra, Energia], Action ):-
    print('39'),nl,
    agt_intention( explorar ),
    agt_frontier( [] ),
    agt_current_position( Current_Position ),
    agt_last_action( Last_Action ),
    next_position( Current_Position, Last_Action, Obs_Position ),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, _Cost, [ Next_Position | _Path ] ),
    direction( Current_Position, Next_Position, Action),
    replace_all( agt_world_rep( _ ), agt_current_position( Obs_Position, obs, - ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_intention( _ ), agt_intention( volver ) ),
    update_thought( '39: Choque y no tengo mas nada que hacer, vuelvo a home.',
                    [1, Sucio, Alfombra, Energia],
                    Current_Position,
                    Next_Position,
                    Action,
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.
    
%-40-----------------------------------------------------------------------------------------------%
% El agente se encuentra en una BALDOSA SUCIA y no tiene bateria suficiente para seguir explorando, 
% entonces vuelve home.
aspibot( [0, 1, 0,Energia], Action):-
    print('40'),nl,
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, [ Next_Position | _Path]),
    Energia =< Cost + 1,
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, New_Frontier ),
    direction( Current_Position, Next_Position, Action),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_intention( _ ), agt_intention( volver ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, bal, sucio ) ),
    update_thought( '40: Estoy en una baldosa sucia. Uso A* para asegurarme que me alcanza la energia. No me alccanza, vuelvo a home.' , 
                    [0, 1, 0, Energia], 
                    Prev_Position, 
                    Current_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.

%-41-----------------------------------------------------------------------------------------------%
% El agente se encuentra en una ALFOMBRA SUCIA y no tiene bateria suficiente para seguir explorando, 
% vuelve a home.
aspibot( [0, 1, 1, Energia], Action):-
    print('41'),nl,
    agt_current_position( Prev_Position ),
    agt_last_action( Last_Action ),
    next_position( Prev_Position, Last_Action, Current_Position ),
    agt_home_position( Home_Position ),
    search( Current_Position, Home_Position, Cost, [ Next_Position | _Path]),
    Energia =< Cost + 1,
    agt_frontier( Frontier ),
    update_neighbours( Current_Position, Frontier, New_Frontier ),
    direction( Current_Position, Next_Position, Action),
    replace_all( agt_current_position( _ ), agt_current_position( Current_Position ) ),
    replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),
    replace_all( agt_last_action( _ ), agt_last_action( Action ) ),
    replace_all( agt_intention( _ ), agt_intention( volver ) ),
    replace_all( agt_world_rep( Current_Position, _, _ ), agt_world_rep( Current_Position, alf, sucio ) ),
    update_thought( '41: Estoy en una baldosa sucia. Uso A* para asegurarme que me alcanza la energia. No me alccanza, vuelvo a home.', 
                    [0, 1, 1, Energia], 
                    Prev_Position, 
                    Current_Position, 
                    Action, 
                    Thought ),
    replace_all( thought( _ ), thought( Thought ) ),
    !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_neighbours( Current_Position, Frontier, New_Frontier )                                    %
% Example usage:                                                                                   %
%                                                                                                  %
%   agt_frontier( Frontier ),                                                                      %
%   agt_current_position( Current_Position ),                                                      %
%   update_neighbours( Current_Position, Frontier, New_Frontier ),                                 %
%   replace_all( agt_frontier( _ ), agt_frontier( New_Frontier ) ),                                    %
%                                                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_neighbours( Current_Position, Frontier, New_Frontier ) :-

    % Calculate the positions of the cells ahead, behind, to the left and to the right.
    next_position( Current_Position, adelante,    Adelante ), 
    next_position( Current_Position, atras,       Atras ), 
    next_position( Current_Position, izquierda,   Izquierda ), 
    next_position( Current_Position, derecha,     Derecha ),

    agt_add_to_frontier( [Adelante, Atras, Derecha, Izquierda], Frontier, New_Frontier ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agt_select_and_remove( +Current_Position, +Energia, +Frontier, -Next_Position )                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agt_select_and_remove( _Current_Position, _Energia, [], [] ).

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F - 1,
    agt_world_rep( [F2, C], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F2, C], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost =< Energia,
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F + 1,
    agt_world_rep( [F2, C], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F2, C], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost =< Energia,
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C + 1,
    agt_world_rep( [F, C2], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F, C2], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost =< Energia,
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C is C - 1,
    agt_world_rep( [F,C2], Terrain,_),
    Terrain \= obs,
    search( Current_Position, [F, C2], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost =< Energia,
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position) :-
    Neighbour = [F, C],
    F2 is F - 1,
    agt_world_rep( [F2, C], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F2, C], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost > Energia,
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F + 1,
    agt_world_rep( [F2, C], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F2,C], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost > Energia,
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C + 1,
    agt_world_rep( [F,C2], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F,C2], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost > Energia,
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C is C - 1,
    agt_world_rep( [F,C2], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F,C2], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost > Energia,
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position,Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F - 1,
    agt_world_rep( [F2, C], obs, _ ),
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove(Current_Position,Energia,[Neighbour|Neighbours],Next_Position):-
    Neighbour = [F, C],
    F2 is F + 1,
    agt_world_rep( [F2,C], obs, _ ),
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C + 1,
    agt_world_rep( [F, C2], obs, _ ),
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C - 1,
    agt_world_rep( [F, C2], obs, _ ),
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F - 1,
    \+ agt_world_rep( [F2, C], _, _ ),
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F + 1,
    \+ agt_world_rep( [F2, C], _, _ ),
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C + 1,
    \+ agt_world_rep( [F, C2], _, _ ),
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select_and_remove( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C - 1,
    \+ agt_world_rep( [F, C2], _, _ ),
    replace_all( agt_frontier( _), agt_frontier( Neighbours ) ),
    agt_select_and_remove( Current_Position, Energia, Neighbours, Next_Position ),
    !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agt_select( +Current_Position, +Energia, +Frontier, -Next_Position )                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agt_select( _Current_Position, _Energia, [], [] ).

agt_select( Current_Position, Energia, [Neighbour|_Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F - 1,
    agt_world_rep( [F2, C], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F2, C], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost =< Energia,
    !.

agt_select( Current_Position, Energia, [Neighbour|_Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F + 1,
    agt_world_rep( [F2, C], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F2, C], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost =< Energia,
    !.

agt_select( Current_Position, Energia, [Neighbour|_Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C + 1,
    agt_world_rep( [F, C2], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F, C2], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost =< Energia,
    !.

agt_select( Current_Position, Energia, [Neighbour|_Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C is C - 1,
    agt_world_rep( [F, C2], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F, C2], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost =< Energia,
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F - 1,
    agt_world_rep( [F2, C], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F2, C], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost > Energia,
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ):-
    Neighbour = [F, C],
    F2 is F + 1,
    agt_world_rep( [F2, C], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F2, C], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost > Energia,
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C + 1,
    agt_world_rep( [F, C2], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F, C2], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost > Energia,
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ):-
    Neighbour = [F, C],
    C is C - 1,
    agt_world_rep( [F, C2], Terrain, _ ),
    Terrain \= obs,
    search( Current_Position, [F, C2], Cost, Path ),
    Path = [Next_Position|_Rest],
    Cost > Energia,
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ):-
    Neighbour = [F, C],
    F2 is F - 1,
    agt_world_rep( [F2, C], obs, _ ),
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F + 1,
    agt_world_rep( [F2, C], obs, _ ),
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C + 1,
    agt_world_rep( [F, C2], obs, _ ),
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C - 1,
    agt_world_rep([F, C2], obs, _ ),
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    F2 is F - 1,
    \+ agt_world_rep( [F2, C], _, _ ),
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ):-
    Neighbour = [F, C],
    F2 is F + 1,
    \+ agt_world_rep( [F2, C], _, _ ),
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C + 1,
    \+ agt_world_rep( [F, C2], _, _ ),
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.

agt_select( Current_Position, Energia, [Neighbour|Neighbours], Next_Position ) :-
    Neighbour = [F, C],
    C2 is C - 1,
    \+ agt_world_rep( [F, C2], _, _ ),
    agt_select( Current_Position, Energia, Neighbours, Next_Position ),
    !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agt_add_to_frontier( +Neighbours, +Frontier, -New_Frontier )                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agt_add_to_frontier( [], Frontier, Frontier ) :- !.

agt_add_to_frontier( [Neighbour|Neighbours], Frontier, [Neighbour|Rest_of_Frontier] ) :-
    \+ member( Neighbour, Frontier),
    \+ agt_world_rep( Neighbour, _, _ ),
    agt_add_to_frontier( Neighbours, Frontier, Rest_of_Frontier ),
    !.

agt_add_to_frontier( [Neighbour|Neighbours], Frontier, Rest_of_Frontier ) :-
    \+ member( Neighbour, Frontier ),
    agt_world_rep( Neighbour, _ ,_ ),
    agt_add_to_frontier( Neighbours, Frontier, Rest_of_Frontier ),
    !.

agt_add_to_frontier( [Neighbour|Neighbours], Frontier, [Neighbour|Rest_of_Frontier] ) :-
    member( Neighbour, Frontier ),
    delete( Frontier, Neighbour, Temp_Frontier ),
    \+ agt_world_rep( Neighbour ,_ , _ ),
    agt_add_to_frontier( Neighbours, Temp_Frontier, Rest_of_Frontier ),
    !.
