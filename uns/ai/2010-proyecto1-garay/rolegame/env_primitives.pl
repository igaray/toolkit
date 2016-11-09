%% Agent-Environment interaction
%%
%% Environment primitives (for interaction with the agent)

:- [interface].

:- dynamic id_name/2.


start_env:- start_server(8001),
            write('antes de crear el thread'), nl,
            thread_create(registration_handler,_,[]).
            %registration_handler.

registration_handler:- write('entr� a registration_handler/1'), nl,
                      recv_msg(AgID, i_am(AgName)),
                      write('recibi� mensaje i_am de '), write(AgName), nl,
                      (id_name(_OtroAgID, AgName), % El nombre ya est� siendo usado por otro agente.
                       send_msg(AgID, used_name)
                        ;
                       assert(id_name(AgID, AgName)), % Se registra el nuevo nombre
                       %ip:dont_fail(ag_registration_setup(AgName)),
                       dont_fail(ag_registration_setup(AgName)),
                       send_msg(AgID, connected)
                       ),
                       registration_handler_freq(RHF),
                       sleep(RHF),
                       registration_handler.


registered_agents(Agents):- findall(Agent, id_name(_ID, Agent), Agents).


% gets percept request from Ag if available
perc_request_available(Ag):-
                     id_name(AgID, Ag),
                     recv_msg(AgID, perc_req, 0.01). %antes 0.001

give_percept(Perc, Ag):-
                     id_name(AgID, Ag),
                     send_msg(AgID, percept(Perc)). % la estructura percept va por si acaso. No estoy seguro que la necesite
                    
action_available(Action, Ag):-
                     id_name(AgID, Ag),
                     recv_msg(AgID, action(Action), 0.01),  % la estructura action es fundamental, sino el get_action puede consumir
                     send_msg(AgID, ack).                   % un pedido de percepci�n (de un agente que entr� tarde por ejemplo, o simplemente
                                                            % se tard� en pedir la percepci�n).
%give_action_fback(Fback, Ag):- send_msg(Ag, Fback).


% Hacer un thread que escuche por conexiones, y que cada vez que reciba un
% i_am(Ag) inicie la rutina Register Agent!!!
% Ver donde es m�s apropiado poner todo esto!!!

%%%%%%%%%%%%%%% AUXILIARES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dont_fail(Goal):- call(Goal), !.

dont_fail(_Goal).