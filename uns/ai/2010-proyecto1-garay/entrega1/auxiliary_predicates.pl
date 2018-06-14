%--------------------------------------------------------------------------------------------------%
display_agent_state :-
    ag_name(Name),
    agent_state(Current_Goal, 
                [[Turn, Vision, Attributes, Inventory] | _Percept_History], 
                [Last_Action | _Action_History], 
                _Agents, 
                Inns, 
                Treasures),
    write('################################################################################'), nl, 
    write('Agent name:      '), write(Name), nl,
    write('Current goal:    '), write(Current_Goal), nl,
    write('Last action:     '), write(Last_Action), nl, 
    write('Known Agents:    '), write_agents(Agents), nl,
    write('Inns:            '), write_inns(Inns), nl,
    write('Treasures  :     '), write_treasures(Treasures), nl,
    write('Inventory:      '), write_inventory(Inventory), nl, nl, 
    write('Map :           '), nl, nl, write_all_map, nl, 
    nl. 

write_percept([Turn, Vision, Attributes, Inventory]) :-
    write('Percept:         '), nl, 
    write('    Turn:        '), write(Turn), nl, 
    write('    Vision:      '), nl,
    write_vision(Vision),
    write('    Attributes:  '), write_attributes(Attributes), nl, 
    write('    Inventory:   '), write_inventory(Inventory), nl.
    
%--------------------------------------------------------------------------------------------------%
write_vision([]).
write_vision([[Pos, Land, Things] | Vision]) :- 
    write('        '), write_position(Pos), write(', '), write_land(Land), write(', '), write(Things), nl, 
    write_vision(Vision).

%--------------------------------------------------------------------------------------------------%
write_attributes(Attributes) :-
    write('['), 
    write_attributes1(Attributes),
    write(']').

write_attributes1([]).
write_attributes1([Attribute]) :-
    write_attribute(Attribute).
write_attributes1([Attribute | Attributes]) :-
    write_attribute(Attribute), 
    write(', '),
    write_attributes1(Attributes).

write_attribute([Attribute, Value]) :-
    write('['), write(Attribute), write(','), write(Value), write(']').

%--------------------------------------------------------------------------------------------------%
write_inventory(Inventory) :-
    write(Inventory).

%--------------------------------------------------------------------------------------------------%
write_agents([]).
write_agents([[Agent_Name, _Resto, Turn] | TailAgents]) :-
    write('[Nombre agente :'), write(Agent_Name), 
    write(', Ultimo turno visto: '), write(Turn), write('] '), 
    write_agents(TailAgents).
    % Por comodidad no se imprime la información guardada debido a que se encuentra en estado 
    % natural de la percepcion.

%--------------------------------------------------------------------------------------------------%
write_inns(Inns) :- write_position_list(Inns).

%--------------------------------------------------------------------------------------------------%
write_treasures([]).
write_treasures([[[X,Y],T]|Tail]) :- 
    write(' ['), 
    write('['), 
    write(X), 
    write(','), 
    write(Y), 
    write(']'), 
    write(','), 
    write(T), 
    write('] '), 
    write_treasures(Tail).

%--------------------------------------------------------------------------------------------------%
write_position_list(Positions) :-
    write('['),
    write_position_list1(Positions),
    write(']').

write_position_list1([]).
write_position_list1([Position]) :-
    write_position(Position).
write_position_list1([Position | Positions]) :-
    write_position(Position), 
    write(', '), 
    write_position_list1(Positions).

%--------------------------------------------------------------------------------------------------%
write_land(mountain) :- write('mountain').
write_land(water)    :- write('   water').
write_land(forest)   :- write('  forest').
write_land(plain)    :- write('   plain').

%--------------------------------------------------------------------------------------------------%
write_position(Position) :-
    position_to_string(Position, String),
    write(String).
    
position_to_string( [F, C], String ) :-
    number_to_string(F, Fs),
    number_to_string(C, Cs),
    string_concat( '[', Fs,   S1 ),
    string_concat( S1,  ',', S2 ),
    string_concat( S2,  Cs,   S3 ),
    string_concat( S3,  ']', String).

number_to_string( N, Ns ) :-
    N < 10,
    string_concat( '0', N, Ns ).
number_to_string( N, Ns ) :-
    string_concat( N, '', Ns ).    

string_concatenation([], '').
string_concatenation([S], S1) :-
    string_concat(S, '', S1).
string_concatenation([S1, S2|T], S3) :-
    string_concat(S1, S2, Temp),
    string_concatenation([Temp|T], S3).

%--------------------------------------------------------------------------------------------------%
write_all_map :-
    forall(agent_map(Pos, plain, _Things, Turn), 
        (
            write(' ['), 
            write_position(Pos), 
            write(','), 
            write_land(plain), 
            write(', '), 
            write(Turn), 
            write('] ')
        )), nl, nl,
    forall(agent_map(Pos, water, _Things, Turn), 
        (
            write(' ['), 
            write_position(Pos), 
            write(','), 
            write_land(water), 
            write(', '), 
            write(Turn), 
            write('] ')
        )), nl, nl,
    forall(agent_map(Pos, forest, _Things, Turn), 
        (
            write(' ['),
            write_position(Pos),
            write(','),
            write_land(forest),
            write(', '),
            write(Turn),
            write('] ')
        )), nl, nl,
    forall(agent_map(Pos, mountain, _Things, Turn), 
        (
            write(' [') , 
            write_position(Pos), 
            write(','), 
            write_land(mountain), 
            write(', '), 
            write(Turn), 
            write('] ')
        )), nl.

%--------------------------------------------------------------------------------------------------%
replace(X, Y) :- retract(X), !, assert(Y).

replaceall(X, Y) :- retractall(X), !, assert(Y).

