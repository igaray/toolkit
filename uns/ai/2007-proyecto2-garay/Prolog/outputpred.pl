% outputpred.pl: Miscelaneous output predicates

print_simulation_record([]).
print_simulation_record([[P,A,CP,CPC,CPF]|SR]) :-
    print('Position:           '), print_position(P), nl,
    print('Action:             '), print(A), nl,
    print('Current Path:       '), print_list_line(CP), nl,
    print('Current Path Cost:  '), print(CPC), nl,
    print('Positions Frontier: '), print_list_line(CPF), nl, nl,
    print_simulation_record(SR).



print_list_line([]).
print_list_line([Head|Tail]) :-
    print(Head),
    print(' '),
    print_list_line(Tail).



print_list_column([]).
print_list_column([Head|Tail]) :-
    tab(4),
    print(Head),
    print_list_column(Tail).



print_percept(perc(Position, North, South, East, West)) :-
    print('Percept:'), nl,
    print('    Position:     '), print_position(Position), nl,
    print('    To the North: '), print_percept_component(North), nl,
    print('    To the South: '), print_percept_component(South), nl,
    print('    To the East:  '), print_percept_component(East), nl,
    print('    To the West:  '), print_percept_component(West), nl.

print_percept(Percept) :-
    print('Percept: '),
    print(Percept),
    nl.

print_percept_component([Position, Terrain, Contents]) :-
    print_position(Position),
    print_terrain(Terrain),
    print(Contents).

print_terrain(agua)   :- print(' agua   ').
print_terrain(mont)   :- print(' mont   ').
print_terrain(bosque) :- print(' bosque ').
print_terrain(pasto)  :- print(' pasto  ').



print_action(Action) :-
    print('Action: '),
    print(Action).



print_path(Current_Path, Current_Paths_Cost, Current_Positions_Frontier) :-
    print('Current Path:       '), print_list_line(Current_Path), nl,
    print('Current Path Cost:  '), print(Current_Paths_Cost), nl,
    print('Positions Frontier: '), print_list_line(Current_Positions_Frontier), nl, nl.



print_selected(node(Position, Path, Path_Cost)) :-
    print('Selected Node: '),
    print_position(Position), nl,
    print('         Path: '), print_list_line(Path), nl,
    print('         Cost: '), print(Path_Cost), nl.



print_neighbours(Neighbours) :-
    print('Neighbours: '), nl,
    print_node_list(Neighbours).



print_frontier(Frontier) :-
    print('Frontier: '), nl,
    print_node_list(Frontier), nl.



print_node_list([]).
print_node_list([node(Posicion, _Camino, _Costo_Camino)|Tail]) :-
    tab(4),
    print_position(Posicion),
    nl,
    print_node_list(Tail).



print_position([Row,Column]) :-
    print('['),
    show_number(Row),
    print(', '),
    show_number(Column),
    print(']').



%% show_world/0
%% Prints a representation of the world's current state.

show_world :-
    findall([X,Y,Z], celda(X,Y,Z), Node_List),
    nl,
    max_column(MC),
    print('  '),
    print_column_numbers(0, MC),
    print('  '),
    print_horizontal_line(MC),
    nl,
    show_world(Node_List, 0, 0),
    nl.



show_world([], _CN, _RN).
show_world([Head|Tail], Cell_Number, Row_Number) :-
    show_cell(Head, Cell_Number, Row_Number, Row_Number1),
    Cell_Number1 is Cell_Number + 1,
    show_world(Tail, Cell_Number1, Row_Number1).



show_cell([Position,Terrain,Contents], Cell_Number, Row_Number, Row_Number1) :-
    print_row_number(Cell_Number, Row_Number, Row_Number1),
    print('|'),
    show_terrain(Terrain),
    show_contents(Contents),
    show_agent(Position),
    print_separator(Position).

print_row_number(Cell_Number, Row_Number, Row_Number1) :-
    max_column(MC),
    0 is Cell_Number mod (MC + 1),
    !,
    show_number(Row_Number),
    Row_Number1 is Row_Number + 1.
print_row_number(_Cell_Number, Row_Number, Row_Number).



print_separator([_Row,Column]) :-
    max_column(Column),
    !,
    print('|'),
    nl,
    print('  '),
    print_horizontal_line(Column),
    nl.
print_separator(_Position).


print_column_numbers(C, 0) :-
    print('   '),
    show_number(C),
    print('  '),
    nl.
print_column_numbers(C, X) :-
    print('   '),
    show_number(C),
    print('  '),
    C1 is C + 1,
    Y is X - 1,
    print_column_numbers(C1, Y).



print_horizontal_line(0) :-
    print('+------+').
print_horizontal_line(X) :-
    print('+------'),
    Y is X - 1,
    print_horizontal_line(Y).



show_agent([Row,Column]) :-
    agent_position([Row,Column]),
    print(' X').

show_agent([_Row,_Column]) :-
    print('  ').



show_number(X) :-
    X < 10,
    !,
    print(' '),
    print(X).
show_number(X) :-
    print(X).



show_terrain(agua)   :- print('A ').
show_terrain(bosque) :- print('B ').
show_terrain(pasto)  :- print('P ').
show_terrain(mont)   :- print('M ').



show_contents(-)                 :- print('  ').
show_contents(puente(sano))      :- print('PS').
show_contents(puente(det))       :- print('PD').
show_contents(puente(roto))      :- print('PR').
show_contents(ejercito(reino))   :- print('ER').
show_contents(ejercito(enemigo)) :- print('EE').
show_contents(castillo)          :- print('C ').


