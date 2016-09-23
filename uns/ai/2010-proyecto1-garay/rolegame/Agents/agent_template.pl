%% Player-Agent joystick

:- consult(ag_primitives), consult(extras_for_agents).

run:-
      get_percept(Perc),
      
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      % AGENT CODE
      % (internal state update and action choice)
      %
      %
      %
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      do_action(Action),
      
      run.
      
      
:- dynamic ag_name/1.


start_ag:- AgName = jk,
           register_me(AgName, Status),
           !,
           write('REGISTRATION STATUS: '),
           write(Status), nl, nl,
           Status = connected,
           assert(ag_name(AgName)),
           run.
   
s:- start_ag.


start_ag_instance(InstanceID):-
                    AgClassName = jk,
                    AgInstanceName =.. [AgClassName, InstanceID],
                    register_me(AgInstanceName, Status),
                    !,
                    write('REGISTRATION STATUS: '),
                    write(Status), nl, nl,
                    Status = connected,
                    assert(ag_name(AgInstanceName)),
                    run.

si(InstanceID):- start_ag_instance(InstanceID).