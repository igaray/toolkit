costados(n,w,e).
costados(s,e,w).
costados(e,n,s).
costados(w,s,n).

%--------------------------------------------------------------------------------------------------%
decide_action(attack(Victim)) :-
    attackable_agent(Victim).

decide_action(Action) :-
    agent_last_percept([_Turn, Vision, Attrs, _Inv]),
    agent_position(Position),
    member([pos, Pos], Attrs),
    Position = Pos,
    member([Pos, _Land, Content], Vision),
    member([hostel, TrName, _], Content), agent_stamina(Stamina), agent_max_stamina(Max_Stamina) ,Stamina < Max_Stamina,
    write('Esperaré en esta posada hasta recargar mi stamina.'), nl,
    Action = turn(none).

decide_action(Action) :-
    agent_last_percept([_Turn, Vision, Attrs, _Inv]),
    member([pos, Pos], Attrs),
    member([dir, Dir], Attrs),
    costados(Dir, Derecha, _Izquierda),
    ady_at_cardinal(Pos, Derecha, PosInRight),
    member([PosInRight, _Land, Content], Vision),
    (
      member([treasure, TrName, _], Content), write('Vi un TESORO a mi DERECHA y voy a juntarlo.'), nl
    ;
      member([hostel, TrName, _], Content), agent_stamina(Stamina), Stamina < 50, write('Voy a recargar mi salud en el HOTEL de mi DERECHA.'), nl
    ),
    Action = turn(Derecha).

decide_action(Action) :-
    agent_last_percept([_Turn, Vision, Attrs, _Inv]),
    member([pos, Pos], Attrs),
    member([dir, Dir], Attrs),
    costados(Dir, _Derecha, Izquierda),
    ady_at_cardinal(Pos, Izquierda, PosInLeft),
    member([PosInLeft, _Land, Content], Vision),
    (
      member([treasure, TrName, _], Content), write('Vi un TESORO a mi IZQUIERDA y voy a juntarlo.'), nl
    ;
      member([hostel, TrName, _], Content), agent_stamina(Stamina), Stamina < 50, write('Voy a recargar mi salud en el HOTEL de mi IZQUIERDA.'), nl
    ),
    Action = turn(Izquierda).
    
decide_action(Action) :-
    agent_last_percept([_Turn, Vision, Attrs, _Inv]),
    member([pos, Pos], Attrs),
    member([dir, Dir], Attrs),
    ady_at_cardinal(Pos, Dir, PosInFront),
    member([PosInFront, Land, _Content], Vision),
    (
      Land = water, write('Uh, me tropece con AGUA, voy a girar.'), nl
    ;
      Land = forest, write('Uh, me tropece con un BOSQUE, voy a girar.'), nl
    ),
    next_90_clockwise(DesiredDir, Dir),
    Action = turn(DesiredDir).


decide_action(Action) :-
    agent_last_percept([_Turn, Vision, Attrs, _Inv]),
    member([pos, MyPos], Attrs),
    member([MyPos, _Land, Content], Vision),
    member([treasure, TrName, _], Content),
    write('He encontrado un tesoro conocido como '), write(TrName), write('.'), nl,
    write('voy a intentar recogerlo...'),nl,
    Action = pickup(TrName).

decide_action(move_fwd).

%--------------------------------------------------------------------------------------------------%
attackable_agent(Victim):-
    agent_last_percept([_Turn, Vision, Attrs, _Inv]),
    member([pos, MyPos], Attrs),
    member([dir, MyDir], Attrs),
    pos_in_attack_range(MyPos, MyDir, PosInAttackRange),
    member([PosInAttackRange, _Land, Content], Vision),
    member([agent, Victim, AgProperties], Content),
    ag_name(MyName), %Debería llamarlo my_name!!!
    Victim \= MyName,
    member([unconscious, false], AgProperties).

%--------------------------------------------------------------------------------------------------%
%pos_in_attack_range(+MyPos, +MyDir, -PosInAttackRange)

pos_in_attack_range(MyPos, _MyDir, MyPos).
pos_in_attack_range(MyPos, MyDir, FrontPos) :-
    ady_at_cardinal(MyPos, MyDir, FrontPos).
pos_in_attack_range(MyPos, MyDir, FrontRightPos) :-
    ady_at_cardinal(MyPos, MyDir, FrontPos),
    next_90_clockwise(MyDir, NextDir),
    ady_at_cardinal(FrontPos, NextDir, FrontRightPos).
pos_in_attack_range(MyPos, MyDir, FrontLeftPos) :-
    ady_at_cardinal(MyPos, MyDir, FrontPos),
    next_90_clockwise(PrevDir, MyDir),
    ady_at_cardinal(FrontPos, PrevDir, FrontLeftPos).

