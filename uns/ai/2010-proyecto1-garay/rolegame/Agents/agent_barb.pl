%% Player-Agent barb

:- consult(ag_primitives).

:- consult(extras_for_agents).

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


decide_action(attack(Victim)):-

      attackable_agent(Victim).


decide_action(Action):-

      last_perc([_Turn, Vision, Attrs, _Inv]),

      member([pos, Pos], Attrs),
      member([dir, Dir], Attrs),
      ady_at_cardinal(Pos, Dir, PosInFront),
      member([PosInFront, Land, _Content], Vision),

      (Land = water
           ;
       Land = forest),

      next_90_clockwise(DesiredDir, Dir),
      Action = turn(DesiredDir).


decide_action(Action):-

      last_perc([_Turn, Vision, Attrs, _Inv]),
      member([pos, MyPos], Attrs),
      member([MyPos, _Land, Content], Vision),
      member([treasure, TrName, _], Content),
      write('He encontrado un tesoro conocido como '), write(TrName), write('.'),nl,
      write('voy a intentar recogerlo...'),nl,
      Action = pickup(TrName).



decide_action(move_fwd).




attackable_agent(Victim):-

      last_perc([_Turn, Vision, Attrs, _Inv]),

      member([pos, MyPos], Attrs),
      member([dir, MyDir], Attrs),

      pos_in_attack_range(MyPos, MyDir, PosInAttackRange),

      member([PosInAttackRange, _Land, Content], Vision),
      member([agent, Victim, AgProperties], Content),

      ag_name(MyName), %Debería llamarlo my_name!!!

      Victim \= MyName,

      member([unconscious, false], AgProperties).


%pos_in_attack_range(+MyPos, +MyDir, -PosInAttackRange)

pos_in_attack_range(MyPos, _MyDir, MyPos).

pos_in_attack_range(MyPos, MyDir, FrontPos):-
	ady_at_cardinal(MyPos, MyDir, FrontPos).

pos_in_attack_range(MyPos, MyDir, FrontRightPos):-
	ady_at_cardinal(MyPos, MyDir, FrontPos),
        next_90_clockwise(MyDir, NextDir),
        ady_at_cardinal(FrontPos, NextDir, FrontRightPos).

pos_in_attack_range(MyPos, MyDir, FrontLeftPos):-
      ady_at_cardinal(MyPos, MyDir, FrontPos),
      next_90_clockwise(PrevDir, MyDir),
      ady_at_cardinal(FrontPos, PrevDir, FrontLeftPos).



:- dynamic ag_name/1.


start_ag:- AgName = barb,
           register_me(AgName, Status),
           !,
           write('REGISTRATION STATUS: '),
           write(Status), nl, nl,
           Status = connected,
           assert(ag_name(AgName)),
           run.

s:- start_ag.


start_ag_instance(InstanceID):-
                    AgClassName = barb,
                    AgInstanceName =.. [AgClassName, InstanceID],
                    register_me(AgInstanceName, Status),
                    !,
                    write('REGISTRATION STATUS: '),
                    write(Status), nl, nl,
                    Status = connected,
                    assert(ag_name(AgInstanceName)),
                    run.

si(InstanceID):- start_ag_instance(InstanceID).
