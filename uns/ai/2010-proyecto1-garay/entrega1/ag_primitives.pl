%% Agent-Environment interaction primitives
% Agent-primitives for interaction with the environment

:- [interface].

register_me(AgName, Status):-
                 write('entro al register me'),nl,
                 connect(env,localhost,8001),
%                 sleep(1),
                 write('pasó el connect'),nl,
                 send_msg(env, i_am(AgName)), %VER
                 write('envió el mensaje i_am'),nl,
                 recv_msg(env, Status),
                 !. % Por si acaso, para evitar posible backtracking.

get_percept(Perc):- send_msg(env, perc_req),
                    recv_msg(env, percept(Perc)).
                    
do_action(Action):- send_msg(env, action(Action)),
                    recv_msg(env, ack).

% el recv_msg podriar quitarlo para permitir que el agente siga "pensando"
% luego que comunicó la acción a realizar.
% Si se quita el recv_msg se corre el riesgo de que un agente sature la
% cola de mensajes del environment enviando acciones (sin percibir).

% do_action with feedback
% Facilita la detección de errores. Por ejemplo, que el agente no respete las
% convenciones para los nombres de las acciones.

do_actionF(Action, Status):- send_msg(env, action(Action)),
                             recv_msg(env, Status).