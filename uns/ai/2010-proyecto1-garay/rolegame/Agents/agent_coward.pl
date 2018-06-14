%% Player-Agent coward

:- consult(ag_primitives), consult(extras_for_agents).

:- dynamic last_perc/1.

last_perc(none).

run:-
      get_percept(Perc),
      
      % AGENT CODE
      % (internal state update and action choice)
      
      ag_name(AgName),
      
      display_ag(AgName, Perc), nl,
      
      update_state(Perc),
      
      decide_action(Action),

      do_action(Action),
      
      run.



update_state(Perc):- retract(last_perc(_)),
                     assert(last_perc(Perc)).


decide_action(Action):-

      last_perc([_Turn, Vision, Attrs, _Inv]),

      member([pos, Pos], Attrs),
      member([dir, Dir], Attrs),
      ady_at_cardinal(Pos, Dir, PosInFront),
      member([PosInFront, Land, Content], Vision),
      (Land = water
           ;
       Land = forest
           ;
       member([agent, EnemyName, _],Content), write('aaaahhhhh, no me ataques '), write(EnemyName), write('!!!'),nl %si es un agente, y entonces lo esquiva!!!
       ),
      next_90_clockwise(Dir, DesiredDir),
      Action = turn(DesiredDir).
      

decide_action(Action):-

      last_perc([_Turn, Vision, Attrs, _Inv]),
      member([pos, MyPos], Attrs),
      member([MyPos, _Land, Content], Vision),
      member([treasure, TrName, _], Content),
      write('iuuuujuuuuu, encontre un tesoro conocido como '), write(TrName), write('!!!'),nl,
      write('voy a intentar recogerlo...'),nl,
      Action = pickup(TrName).
      
      
      
decide_action(move_fwd).




:- dynamic ag_name/1.


start_ag:- AgName = coward,
           register_me(AgName, Status),
           !,
           write('REGISTRATION STATUS: '),
           write(Status), nl, nl,
           Status = connected,
           assert(ag_name(AgName)),
           run.

s:- start_ag.


start_ag_instance(InstanceID):-
                    AgClassName = coward,
                    AgInstanceName =.. [AgClassName, InstanceID],
                    register_me(AgInstanceName, Status),
                    !,
                    write('REGISTRATION STATUS: '),
                    write(Status), nl, nl,
                    Status = connected,
                    assert(ag_name(AgInstanceName)),
                    run.

si(InstanceID):- start_ag_instance(InstanceID).