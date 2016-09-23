:- consult('ag_primitives.pl').
:- consult('extras_for_agents.pl').
:- consult('auxiliary_predicates.pl').
:- consult('update_state.pl').
:- consult('decide_action.pl').

:- dynamic ag_name/1.

%--------------------------------------------------------------------------------------------------%
s :- start_ag.

si(InstanceID) :- start_ag_instance(InstanceID).

start_ag :- 
    AgName = emignator,
    register_me(AgName, Status),
    !,
    write('REGISTRATION STATUS: '), write(Status), nl, nl,
    Status = connected,
    assert(ag_name(AgName)),
    assert(agent_state(none, [none], [none], [], [], [])),
    run.

start_ag_instance(InstanceID) :-
    AgClassName = emignator,
    AgInstanceName =.. [AgClassName, InstanceID],
    register_me(AgInstanceName, Status),
    !,
    write('REGISTRATION STATUS: '), write(Status), nl, nl,
    Status = connected,
    assert(ag_name(AgInstanceName)),
    assert(agent_state(none, [none], [none], [], [], [])),
    run.

run :-
    get_percept(Percept),
    %----------------------------------------------------------------------------------------------%
    % AGENT CODE (internal state update and action choice)
    update_state(percept, Percept),
    %display_agent_state,
    print_agent_map,
    decide_action(Action),
    write('Action taken: '), write(Action), nl,
    %----------------------------------------------------------------------------------------------%
    do_action(Action),
    run.

	
	
	%-------------------------------- MAP PRINTER -------------------------------------------------%
	print_agent_map :- 
		nl,nl,nl, 
		get_min_and_max_row(Min_Row, Max_Row),
		get_min_and_max_column(Min_Column, Max_Column),
		write('  '),
		print_head_columns(Min_Column, Max_Column),
		print_all_map(Min_Row, Max_Row, Min_Column, Max_Column),nl,nl,nl.
		
	print_head_columns(Max_Column, Max_Column):-
		Column_to_write is Max_Column mod 10,
		write(Column_to_write), nl.
	
	print_head_columns(Min_Column, Max_Column):-
		Column_to_write is Min_Column mod 10,
		write(Column_to_write),
		Next_Column is Min_Column +1,
		print_head_columns(Next_Column, Max_Column).
		
	get_min_and_max_row(Min_Row,Max_Row) :- 
		findall(R, agent_map([R, _C], _Terrain, _Contents, _Turn), List_Of_Rows),
		min_list(List_Of_Rows,Min_Row),
		max_list(List_Of_Rows,Max_Row).
		
	get_min_and_max_column(Min_Column,Max_Column) :- 
		findall(C, agent_map([_R, C], _Terrain, _Contents, _Turn), List_Of_Columns),
		min_list(List_Of_Columns,Min_Column),
		max_list(List_Of_Columns,Max_Column).
	
	
	print_all_map(Max_Row, Max_Row, Column_Iterator, Max_Column) :-
		Row_to_write is Max_Row mod 10,
		write(Row_to_write), 
		write(' '), 
		print_row(Max_Row, Column_Iterator, Max_Column), nl.
	
	print_all_map(Row_Iterator, Max_Row, Column_Iterator, Max_Column) :-
		Row_to_write is Row_Iterator mod 10,
		write(Row_to_write), 
		write(' '), 
		print_row(Row_Iterator, Column_Iterator, Max_Column),
		Next_Row_Iterator is Row_Iterator + 1,
		nl,
		print_all_map(Next_Row_Iterator, Max_Row, Column_Iterator, Max_Column).
		
	print_row(Actual_Row, End_Column, End_Column) :-
		agent_map([Actual_Row, End_Column], Terrain, _Things, _Turn),
		(
			Terrain = forest  , write('#')
		;
			Terrain = plain   , write('-')
		;
			Terrain = mountain, write('^')
		;
			Terrain = water   , write('~')
		).
	
	print_row(Actual_Row, Actual_Column, End_Column) :-
		agent_map([Actual_Row, Actual_Column], Terrain, _Things, _Turn),
		(
			Terrain = forest  , write('#')
		;
			Terrain = plain   , write('-')
		;
			Terrain = mountain, write('^')
		;
			Terrain = water   , write('~')
		),
		Next_Column is Actual_Column +1,
		print_row(Actual_Row, Next_Column, End_Column).
	
	print_row(Actual_Row, End_Column, End_Column) :-
		write('?').
	
	print_row(Actual_Row, Actual_Column, End_Column) :-
		write('?'),
		Next_Column is Actual_Column +1,
		print_row(Actual_Row, Next_Column, End_Column).
	%----------------------------------------------------------------------------------------------%