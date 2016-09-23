:- consult('ag_primitives.prolog').
:- consult('extras_for_agents.prolog').
:- consult('auxiliary_predicates.prolog').
:- consult('update_state.prolog').
:- consult('decide_action.prolog').
:- consult('search.prolog').

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
    empty_assoc(Agents),
    empty_assoc(Hostels),
    empty_assoc(Treasures),
    assert(agent_state(none, [none], [none], Agents, Hostels, Treasures, [], [], 0)),
    run.

start_ag_instance(InstanceID) :-
    AgClassName = emignator,
    AgInstanceName =.. [AgClassName, InstanceID],
    register_me(AgInstanceName, Status),
    !,
    write('REGISTRATION STATUS: '), write(Status), nl, nl,
    Status = connected,
    assert(ag_name(AgInstanceName)),
    empty_assoc(Agents),
    empty_assoc(Hostels),
    empty_assoc(Treasures),
    assert(agent_state(none, [none], [none], Agents, Hostels, Treasures, [], [], 0)),
    run.

run :-
    get_percept(Percept),
    %----------------------------------------------------------------------------------------------%
    % AGENT CODE (internal state update and action choice)
    %write(1), read(_), nl,
    update_state(percept, Percept),
    %write(2), read(_), nl,
    display_agent_state,
    %write(3), read(_), nl,
    decide_action(Action),
    %write(4), read(_), nl,
    write('Action taken: '), write(Action), nl, 
    nl,
    %----------------------------------------------------------------------------------------------%
    do_action(Action),
    run.

