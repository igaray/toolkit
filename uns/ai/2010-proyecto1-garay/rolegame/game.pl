% Things to do:
/*
Reflejar que los agentes pueden decidir no hacer nada en un dado turno (tal vez basta con definir 
do(nothing) en las primitivas del agente, que simplemente no haga nada. VER)

Hacer una thread que cada 10 segundos, o un poco más, elimine los nombres de los agentes que se 
desconectaron (elimina todos aquellos cuyos ids no aparecen en la lista de which_agents).

Generalizaciones:
- Hacer algo parecido a los atributo climbing y climbing_for, que sea time executing_action y 
executing action_for. Además definir un set_executin_action (en lugar de set_climbing) que lo ponga 
a ejecutar la accion, y dependiendo de la misma lo pone el tiempo que corresponda (en game settings 
defino para cada accion su costo en tiempo, al igual que defino su costo en stamina).
*/

:- dynamic turn/1, forbidden_entry/3, ag_attr/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Java communication dynamic predicates

:- dynamic j_agent_action/2, j_agent_becomes_unconscious/1, j_drop_all/1.
% j_agent_moved_to/2, j_agent_turned_dir/2,
% j_agent_conscious/1, j_agent_unconscious/1,
% j_agent_picked_up/2, j_agent_dropped/2,
% j_agent_attacked/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

turn(0).

:- consult(comarca), consult(game_settings), consult(env_primitives), consult(extras).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Type inheritance hierarchy

is_a_direct(hostel, building).
is_a_direct(treasure, object).

is_a(Type, AncestorType):- is_a_direct(Type, AncestorType).
is_a(Type, AncestorType):- is_a_direct(Type, ParentType), is_a(ParentType, AncestorType).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% type_property(Type, Property).
%%
%% Aún no lo usé

type_property_direct(treasure, pickable).
type_property(Type, Property) :- 
    type_property_direct(Type, Property).
type_property(Type, Property) :- 
    is_a(Type, AncestorType),
    type_property_direct(AncestorType, Property).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Configuración de la Comarca

% cell_land(Pos, Land)
%
% Land:
%
% plain
% water
% mountain
% forest

cell_content(Pos, Content) :- 
    findall(Thing, at(Thing, Pos), Content).

% at(Thing, Pos)

at([agent, AgName], Pos):- ag_attr(AgName, pos, Pos).
at([BType, BName], Pos):- building(BType, BName, Pos, _BDescr).
at([ObjType, ObjName], Pos):- object_at([ObjType, ObjName, _ObjDescr], Pos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Agents info:
%
% Attributes
% ag_attr(AgName, AttrName, Value)
%
% Attributes: max_stamina, fight_skill, load_capacity (skill attributes)
%             pos, dir, (current) stamina, load, resting, resting_for,
%             unconscious, unconscious_for, climbing, climbing_for
%
% Inventory
% has(Ag, [ObjType, ObjName, ObjDescription]) % o carry/carries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic ag_attr/3, has/2.

% Attribute resting (derived attribute)
%ag_attr(AgName, resting, false):- ag_attr(AgName, resting_for, -1), !.
%ag_attr(_AgName, resting, true).


% Attribute unconscious (derived attribute)
ag_attr(AgName, unconscious, false) :- 
    ag_attr(AgName, unconscious_for, -1).
%ag_attr(_AgName, unconscious, true).
ag_attr(AgName, unconscious, true) :- 
    ag_attr(AgName, unconscious_for, UF),
    UF >= 0.

% Attribute climbing (derived attribute)
ag_attr(AgName, climbing, false) :- 
    ag_attr(AgName, climbing_for, -1).
ag_attr(AgName, climbing, true) :- 
    ag_attr(AgName, climbing_for, CF), 
    CF >= 0.

% Attribute current_turn_action (derived attribute)
ag_attr(AgName, previous_turn_action, Action) :- 
    ag_attr(AgName, last_action, [Last_action, ActionTurn]),
    turn(CurrTurn),
    (
        CurrTurn is ActionTurn + 1, % CurrTurn es el turno siguiente a ActionTurn
        Action = Last_action
    ;
        not(CurrTurn is ActionTurn + 1),
        Action = none
    ).

ag_attr(AgName, attacked_by, Attackers) :- 
    ag_attr(AgName, attacked_by_possibly_empty, Attackers),
    Attackers \= [].

ag_attr(AgName, harmed_by, Attackers) :- 
    ag_attr(AgName, harmed_by_possibly_empty, Attackers),
    Attackers \= [].

ag_attr(AgName, fight_skill, FightSkill) :- 
    ag_attr(AgName, attacks_won, AttacksWon),
    fight_skill_function(AttacksWon, FightSkill).

% Para que no se calcule cada vez que se consulta el atributo, podría incluirse un atributo
% fight_skill_computed, y que fight_skill consulte de ahí mientras ninguno de los valores
% empleados para calcular fight skill haya cambiado. Ver como determinarlo, pero me parece que no
% conviene.

ag_attr(AgName, max_stamina, MaxStamina) :- 
    ag_attr(AgName, num_of_training_actions, TrainingActions),
    max_stamina_function(TrainingActions, MaxStamina).

%ag_attr(_AgName, previous_turn_action, none).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ag_registration_setup(+AgName)
%%
%% Initializes agent info

ag_registration_setup(AgName):-
    % assert(ag_attr(AgName, max_stamina, MaxSt)),
    % initial_fight_skill(InitFS),
    % assert(ag_attr(AgName, fight_skill, InitFS)),
    % assert(ag_attr(AgName, load_capacity, 10)),
    % ags_starting_pos(StartingPos),
    assert(ag_attr(AgName, attacks_won, 0)),
    assert(ag_attr(AgName, num_of_training_actions, 0)),
    ag_starting_position([StartingPos, StartingDir]),
    assert(ag_attr(AgName, pos, StartingPos)), % set Agent position
    assert(ag_attr(AgName, dir, StartingDir)), % set Agent direction
    ag_attr(AgName, max_stamina, MaxSt),
    assert(ag_attr(AgName, stamina, MaxSt)), % set current Agent stamina
    assert(ag_attr(AgName, unconscious_for, -1)), % set Agent "conscious state"
    assert(ag_attr(AgName, climbing_for, -1)), % set Agent "climbing state"
    %assert(ag_attr(AgName, last_action_done, none)),
    %assert(ag_attr(AgName, last_action_turn, 0)),
    assert(ag_attr(AgName, last_action, [none, 0])),
    assert(ag_attr(AgName, attacked_by_possibly_empty, [])),
    assert(ag_attr(AgName, harmed_by_possibly_empty, [])),
    ag_attr(AgName, pos, AgPos),
    ag_attr(AgName, dir, AgDir),
    assert(j_new_agent(AgName, AgPos, AgDir)).

n_of_connected_ags(NCA):- which_agents(Agents),
                          length(Agents, NCA).

ag_starting_position(SPos):- n_of_connected_ags(NCA),
                             ags_starting_positions(StartingPositions),
                             length(StartingPositions, NOfStartingPositions),
                             InitialPosIndex is ((NCA - 1) mod NOfStartingPositions) + 1,
                             element_at(InitialPosIndex, StartingPositions, SPos).

% update_attr(+Ag, +Attr, _CurrValue, _NewValue, +Where)
%
% ej: update_attr(Ag, stamina, CurrValue, NewValue, NewValue is CurrValue + 1)
%     update_attr(Ag, dir, CurrValue, NewValue, next_90_clockwise(CurrValue, NewValue))
%     update_attr(Ag, pos, _CurrValue, [1,1], true)
%
% ACLARACIÓN: la meta Where no debe fallar.

update_attr(Ag, Attr, CurrValue, NewValue, Where):-
                retract(ag_attr(Ag, Attr, CurrValue)),
                call(Where),        % Cuidado!!! Debería asegurarme que el where no falle!!!
                assert(ag_attr(Ag, Attr, NewValue)).

%% The Game (Environment)

run:- run_one_turn,!,
      run.



run_one_turn:-
      %sleep(aire) %cuidado! asegurarme que igual reciba mensajes.
      dynamic_env_update,

      display_env, nl,

      give_requested_percs, %give requested percepts
      %write('give_requested_percs finalizó con exito'), nl,

      post_perc_dynamic_env_update,

      time_to_think(TimeToThink),
      sleep(TimeToThink),
      excecute_available_actions.


dynamic_env_update:- retract(turn(PrevT)),
                     CurrentT is PrevT + 1,
                     assert(turn(CurrentT)),
                     %retractall(ag_attr(AgName, resting, 0)),
                     %forall(ag_attr(AgName, resting, true),
                     %(retract(ag_attr(AgName, resting_for, N)), PredN is N-1, assert(ag_attr(AgName, resting_for, PredN)))),
                     forall(ag_attr(AgName, climbing, true),
                     update_attr(AgName, climbing_for, CurrV, NewV, NewV is CurrV - 1)),
                     forall(ag_attr(AgName, unconscious_for, 0),
                     (update_attr(AgName, stamina, _CurrV, NewV, wake_up_stamina(NewV)))),
                     % Les da stamina a los agentes que se despiertan en este turno.
                     forall(ag_attr(AgName, unconscious, true),
                     update_attr(AgName, unconscious_for, CurrV, NewV, NewV is CurrV - 1)),
                     forall((building(hostel, BName, Pos, _Descr),
                             ag_attr(AgName, pos, Pos),
                             ag_attr(AgName, stamina, St),
                             ag_attr(AgName, max_stamina, MaxSt)),

                             (St = MaxSt,
                              ady_at_cardinal(Pos, _Card, AdyPos),
                              not(cell_land(AdyPos, forest)),
                              not(cell_land(AdyPos, water)),
                              update_attr(AgName, pos, _CurrPos, AdyPos, true),
                              %retractall(forbidden_entry(AgName, BName, _),
                              assert(j_agent_action(AgName, move_to(AdyPos))),
                              turn(Turn),
                              forbidden_entry_time(FET),
                              UntilTurn is Turn + FET,
                              assert(forbidden_entry(AgName, BName, UntilTurn))

                                 ;

                              (St < MaxSt, %podría quitar esta condición
                              hostel_recovery_rate(RR),
                              update_attr(AgName, stamina, _CurrSt, NewSt, NewSt is min(St+RR, MaxSt))))),

                      forall((forbidden_entry(AgName, BName, UntilTurn), turn(Turn), Turn > UntilTurn),
                             retract(forbidden_entry(AgName, BName, UntilTurn))).


post_perc_dynamic_env_update:- forall(ag_attr(AgName, attacked_by_possibly_empty, Attackers),
                                      update_attr(AgName, attacked_by_possibly_empty, Attackers, [], true)),
                               forall(ag_attr(AgName, harmed_by_possibly_empty, Attackers),
                                      update_attr(AgName, harmed_by_possibly_empty, Attackers, [], true)).
                               %retractall(ag_attr(AgName, attacked_by, _Attackers)),
                               %retractall(ag_attr(AgName, harmed_by, _Attackers)).


give_requested_percs:-
                   registered_agents(Ags),
                   forall((member(Ag, Ags), not(unconscious(Ag)), not(climbing(Ag)), perc_request_available(Ag)), assert(perc_request_from(Ag))),
                   % el not(unconscious(Ag)) hace que si un dado Ag está unconscious, entonces no se le recibirá la solicitud
                   % de percepción, y por lo tanto quedará bloqueado hasta que vuelva a estar consciente.
                   %
                   forall(perc_request_from(Ag), (generate_perc(Ag, Perc), retract(perc_request_from(Ag)), assert(perc_for(Ag, Perc)))),
                   % Version con handler
                   forall(perc_request_from(Ag),
                   (
                      if_fails_do(generate_perc(Ag, Perc), (write('fallo generate_perc for'), write(Ag), nl)),
                      retract(perc_request_from(Ag)),
                      assert(perc_for(Ag, Perc))
                   )),
                   %
                   %El retract(perc_request_from(Ag)) lo hago luego del generate_perc(Ag, Perc) por si este último falla, así no me olvido
                   %que todavía le debo la percepción al agente.
                   forall(perc_for(Ag, Perc), (give_percept(Perc, Ag), retract(perc_for(Ag, Perc)))).

% Considerar la alternativa de usar un hilo que constantemenre reciba solicitudes de percepciones
% y las responda. Usar semáforo para lograr atomicidad de la operación de update_game_state!!
% Con esto me evito el sleep(aire)!!!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Perceptions
%
% [Turn, AtSigh, Attrs, Inventory]
%
% Turn/Time
%
% AtSigh = [ [Pos, Land, Objects], ... ]
%
% where  Pos = [X,Y]
%        Land = plain / forest / water / mountain
%        Objects = [ [Object, Name, Description], ... ]
%
%        where Object = treasure / hostel / agent
%
% Attrs = [ [AttrName, Value/Description], ... ]
%
% Inventory = [ [Object, Name, Description], ... ]

generate_perc(Ag, [Turn, Vision, Attrs, Inventory]):-
                  turn(Turn),
                  generate_vision(Ag, Vision), %write(Vision), nl,
                  generate_attrs(Ag, Attrs), %write(Attrs), nl,
                  generate_inventory(Ag, Inventory). %write(Inventory), nl.



generate_vision(Ag, Vision):-
                    ag_attr(Ag, pos, AgPos),
                    ag_attr(Ag, dir, AgDir),
                    findall([Pos, Land, Objects], (pos_at_sight(AgPos, AgDir, Pos),
                                                   cell_land(Pos, Land),
                                                   cell_content_vision(Pos, Objects)),
                             Vision).

cell_content_vision(Pos, Content):- findall([ThingType, ThingName, VisibleDescr],
                                            (at([ThingType, ThingName], Pos), visible_descr([ThingType, ThingName], VisibleDescr)), Content).

% visible_descr([ThingType, ThingName], VisibleDescr))

visible_descr([agent, AgName], VisibleDescr):-
                          findall([AttrName, AttrVal], (visible_attr(agent, AttrName), ag_attr(AgName, AttrName, AttrVal)), VisibleDescr).

visible_descr([BType, BName], BDescr):-
                          building(BType, BName, _Pos, BDescr).

visible_descr([ObjType, ObjName], ObjDescr):-
                          object_at([ObjType, ObjName, ObjDescr], _Pos).


% visible_attr(Type, Attr)
%
% establece que el attributo Attr del tipo Type es visible para
% los agentes.
% Aclaración: si pueden existir distintos tipos de agentes con
% distintas capacidades de visión, entonces tengo que agragar un
% argumento AgentType al predicado indicando el tipo del agente
% que puede ver el atributo en cuestión (del tipo considerado).
% Sobre este argumento también debo aplicar la herencia.

% MMMM, con los ! no puedo llamarlo con el atributo sin instanciar.

visible_attr(agent, dir).

visible_attr(agent, unconscious). %:- !.

visible_attr(agent, previous_turn_action). %:- !.

visible_attr(agent, attacked_by). %:- !.

visible_attr(agent, harmed_by). %:- !.

visible_attr(Type, Attr):- is_a(Type, AncestorType),
                           visible_attr(AncestorType, Attr).



generate_attrs(Ag, Attrs):- findall([AttrName, Value], (sensable_attr(agent, AttrName), ag_attr(Ag, AttrName, Value)), Attrs).


% sensable_attr(AgType, AttrName)

sensable_attr(agent, pos).

sensable_attr(agent, dir).

sensable_attr(agent, stamina).

sensable_attr(agent, max_stamina).

sensable_attr(agent, fight_skill).

sensable_attr(Type, Attr):- is_a(Type, AncestorType),
                            sensable_attr(AncestorType, Attr).



generate_inventory(Ag, Inv):- findall(Object, has(Ag, Object), Inv).




excecute_available_actions:-
                            registered_agents(Ags),
                            forall((member(Ag, Ags), action_available(Action, Ag)), assert(action_from(Action, Ag))),
                            update_game_state.
                            % NO OLVIDARME DE QUITAR TODAS LAS ACCIONES.

% Esta forma de ejecutar las acciones, es decir, se juntan todas las disponibles
% y se ejecutan todos "a la vez", evita que el entorno cambie mientras que un
% agente está pensando su próxima acción (por supuesto, salvo que el agente se
% tarde mucho en pensar y comunique su acción en el próximo turno).
% Si por el contrario se implementara un esquema donde el entorno está en todo
% momento sministrando percepciones apenas son demandadas y recorriendo la
% lista de agentes ejecutando las acciones disponibles, entonces los agentes
% más rápidos tienen más chance de que el estado del mundo no haya cambiado
% desde que percibieron hasta que ejecutaron la acción, mientras que los más
% lentos tienen más chance de que sus acciones se ejecuten cuando el estado
% del mundo ya cambió (respecto a la percepción que motivó la acción) y tal
% vez ya no sea la acción más apropiada al momento en que es ejecutada.
% Aunque este enfoque es más realista, se aleja del esquema "por turnos"
% resultando más complicado. Además existe el riesgo de que una estrategia
% más reactiva y simple tenga ventaja sobre una más deliberativa, ya que la
% primera permitirá al agente actuar más cantidad de veces. No queremos eso!!!


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actions
%
% move_fwd
%
% turn(Dir),          where Dir = n / e / s / o.
%
% attack(AgName)      AgName es el nombre de la víctima
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% action/4
%
% action(Signature, From, Pre, Effects)
%
% Para especificación de acciones

action(attack(Ag2), AgFrom,

( % Pre
AgFrom \= Ag2,
not(climbing(AgFrom)),
not(unconscious(AgFrom)),
not(climbing(Ag2)), %VER
not(unconscious(Ag2)),
can_attack(AgFrom, Ag2)
),

( % Effects
solve_attack(AgFrom, Ag2),
stamina_cost(attack, C),
update_attr(AgFrom, stamina, CurrSt, NewSt, NewSt is CurrSt - C),
%assert(j_agent_attacked(Ag1, Ag2))
update_attr(AgFrom, last_action, _CurrVal, NewVal, (turn(Turn), NewVal = [attack(Ag2), Turn])),
update_attr(Ag2, attacked_by_possibly_empty, CurrV, NewV, append(CurrV, [AgFrom], NewV)),
assert(j_agent_action(AgFrom, attack(Ag2)))
% Por ahora, el ataque no cuesta energía a quien ataca
)).

% Aclaración: El solve attack no refleja los efectos del ataque hasta que se
% haga el commit. Esto es porque se asume que los ataques se resuelven en
% simultáneo.
%
% Cuastiones a Resolver
%
% Qué sucede si empatan? (la víctima no sufre daño,
% pero el atacante gana en skill? -> Ver como pongo el >).
%
% si un agente ejecuta move_fwd pero otro lo ataca y lo deja inconsciente,
% se mueve antes de quedar inconsciente o no? Definir!! (Si está agarrando un
% objeto da igual).

can_attack(Ag1, Ag2):-
                ag_attr(Ag1, pos, PosAg1),
                not(at([hostel, _BName], PosAg1)),
                ag_attr(Ag1, dir, DirAg1),
                ag_attr(Ag2, pos, PosAg2),
                not(at([hostel, _BName], PosAg2)),
                %ag_attr(Ag2, dir, DirAg2),
                ady_at_cardinal(PosAg1, DirAg1, FrontPos),
                next_90_clockwise(DirAg1, NextDirAg1),
                next_90_clockwise(PrevDirAg1, DirAg1),
                ady_at_cardinal(FrontPos, NextDirAg1, FrontRightPos),
                ady_at_cardinal(FrontPos, PrevDirAg1, FrontLeftPos),
                (PosAg2 = FrontPos ; PosAg2 = FrontRightPos ; PosAg2 = FrontLeftPos ; PosAg2 = PosAg1).



action(pickup(ObjName), AgFrom,

( %Pre
not(climbing(AgFrom)),
not(unconscious(AgFrom)),
%pickable(Obj),
ag_attr(AgFrom, pos, AgPos),
object_at([ObjType, ObjName, Descr], AgPos),
not((ag_attr(OtherAg, pos, AgPos), OtherAg \= AgFrom, conscious(OtherAg)))
)
,
( %Effects
assert(has(AgFrom, [ObjType, ObjName, Descr])),
retract(object_at([ObjType, ObjName, Descr], AgPos)),
%assert(j_agent_picked_up(AgFrom, ObjName))
stamina_cost(pickup, C),
update_attr(AgFrom, stamina, CurrSt, NewSt, NewSt is CurrSt - C),
update_attr(AgFrom, last_action, _CurrVal, NewVal, (turn(Turn), NewVal = [pickup(ObjName), Turn])),
assert(j_agent_action(AgFrom, pickup(ObjName)))
)
).

% Qué sucede si dos agentes hacen un pick del mismo objeto al mismo tiempo?
% Puedo tirar un dado para resolver quién se lo queda.
%
% También puedo pedir como pre que no haya otros agentes conscientes para agarrar
% el objeto (esto motivará una batalla para quedarse con el tesoro),
% o que no lo agarra ninguno.
% tal vez deba hacer algo parecido a attack!!!
%
% ESTA FUE LA POLÍTICA ADOPTADA

action(drop(ObjName), AgFrom,

( %Pre
%not(resting(AgFrom)),
% FALTA CONTROLAR QUE EL AGENTE TENGA EL OBJETO QUE DESEA SOLTAR!!!!
not(climbing(AgFrom)),
not(unconscious(AgFrom))
),

( %Effects
 retract(has(AgFrom, [ObjType, ObjName, ObjDescr])),
 %write(AgFrom), write(Obj),nl,
 ag_attr(AgFrom, pos, Pos),
 %write(Pos),nl,
 assert(object_at([ObjType, ObjName, ObjDescr], Pos)),

 stamina_cost(drop, C),
 update_attr(AgFrom, stamina, CurrSt, NewSt, NewSt is CurrSt - C),
 %write(ObjName),nl,
 %assert(j_agent_dropped(AgFrom, ObjName))
 update_attr(AgFrom, last_action, _CurrVal, NewVal, (turn(Turn), NewVal = [drop(ObjName), Turn])),
 assert(j_agent_action(AgFrom, drop(ObjName)))
)).



action(turn(Dir), AgFrom,

( % Pre
 %not(resting(AgFrom)),
 cadinal_dir(Dir),
 not(climbing(AgFrom)),
 not(unconscious(AgFrom))
),

( % Effects

 %assert(j_agent_turned_dir(AgFrom, Dir)),
 assert(j_agent_action(AgFrom, turn(Dir))),

 update_attr(AgFrom, dir, _CurrDir, Dir, true),
 stamina_cost(turn, C),
 update_attr(AgFrom, stamina, CurrSt, NewSt, NewSt is CurrSt - C),
 update_attr(AgFrom, last_action, _CurrVal, NewVal, (turn(Turn), NewVal = [turn(Dir), Turn])),
 update_attr(AgFrom, num_of_training_actions, CurrTrainigActions, NewTrainigActions, NewTrainigActions is CurrTrainigActions + 1)

)).

% Auxiliary preds

% cadinal_dir(Dir)

cadinal_dir(n).
cadinal_dir(e).
cadinal_dir(s).
cadinal_dir(w).


action(move_fwd, AgFrom,

( % Pre
 %not(resting(AgFrom)),
 not(climbing(AgFrom)),
 not(unconscious(AgFrom)),
 ag_attr(AgFrom, pos, Pos),
 ag_attr(AgFrom, dir, Dir),
 ady_at_cardinal(Pos, Dir, DestPos),
 not(cell_land(DestPos, water)),
 not(cell_land(DestPos, forest)),
 implies(building(_BType, BName, DestPos, _Descr), can_enter(AgFrom, BName))
),

( % Effects
 update_attr(AgFrom, pos, _CurrPos, DestPos, true),
 ( cell_land(DestPos, mountain),
   stamina_cost(move_fwd_mountain, CM),
   update_attr(AgFrom, stamina, CurrSt, NewSt, NewSt is CurrSt - CM),
   %set_resting(AgFrom)
   set_climbing(AgFrom)
    ;
   stamina_cost(move_fwd_plain, CP),
   update_attr(AgFrom, stamina, CurrSt, NewSt, NewSt is CurrSt - CP)
 ),
 implies(building(hostel, BName1, Pos, _Descr1),
        (turn(Turn),
         forbidden_entry_time(FET),
         UntilTurn is Turn + FET,
         assert(forbidden_entry(AgFrom, BName1, UntilTurn))
        )),
 %assert(j_agent_moved_to(AgFrom, DestPos))
 update_attr(AgFrom, last_action, _CurrVal, NewVal, (turn(Turn), NewVal = [move_fwd, Turn])),
 assert(j_agent_action(AgFrom, move_to(DestPos))),
 update_attr(AgFrom, num_of_training_actions, CurrTrainigActions, NewTrainigActions, NewTrainigActions is CurrTrainigActions + 1)
)).

% Actions auxiliary predicates

%resting(Ag):- ag_attr(Ag, resting, true).

%set_resting(Ag):- mountain_resting_time(RT),
%                  retract(ag_attr(Ag, resting_for, _)),
%                  assert(ag_attr(Ag, resting_for, RT)).


climbing(Ag):- ag_attr(Ag, climbing, true).

set_climbing(Ag):- update_attr(Ag, climbing_for, _CurrV, CT, climbing_time(CT)).

unconscious(Ag):- ag_attr(Ag, unconscious, true).

conscious(Ag):- ag_attr(Ag, unconscious, false).

can_enter(AgFrom, BName):- building(hostel, BName, _Pos, _Descr),
                           not(forbidden_entry(AgFrom, BName, _UntilTurn)).


% execute(Action)

execute(Action, AgFrom):- if_fails_do(action(Action, AgFrom, Pre, Effects), (write('unknown action: '), write(Action), nl)),
                          %call(Pre), !,
                          if_fails_do(Pre, (write('fallaron las Pre de la acción '), write(Action), nl)),
                          %call(Effects).
                          if_fails_do(Effects, (write('fallaron los Efectos de la acción '), write(Action), nl)),
                          !. % Este cut es para evitar multiplicidad de soluciones
                             % cuando tanto Pre como Effects se ejecutan con éxito.

execute(_Action, _AgFrom). % ¿Para que es esto?



% IMPLEMENTAR

update_game_state:- forall(retract(action_from(attack(Ag2), Ag)), execute(attack(Ag2), Ag)),
                    %forall(retract(update(Ag, Attr, Quantity)), commit_update(Ag, Attr, Quantity)),
                    commit_updates,
                    forall(retract(action_from(pickup(Obj), Ag)), execute(pickup(Obj), Ag)),
                    % En principio, debo ejecutar todos los pickup antes de los drop, sino un agente
                    % podría levantar un objeto en el mismo momento que otro agente lo está dejando.
                    % (Aunque debería adivinar que este lo tiene, y que lo va a dejar efectivamente,
                    % y tendría que estar después en el orden de which_agents).
                    forall(retract(action_from(Action, Ag)), execute(Action, Ag)),
                    set_unconscious.
                    % Al hacerlo al final hace que la victima de una ataque logre moverse antes de quedar
                    % inconsciente.

% solve_attack(From, To)
solve_attack(Ag1, Ag2):-
                   ag_attr(Ag1, fight_skill, SkillAg1),
                   if_fails_do(ag_attr(Ag2, fight_skill, SkillAg2), write(Ag2)),
                   dice_sides(Chance),
                   if_fails_do(dice(Chance, PlusAg1),write('2')),
                   dice(Chance, PlusAg2),
                   AttackPowerAg1 is SkillAg1 + PlusAg1,
                   ResistanceAg2 is SkillAg2 + PlusAg2,
                   AttackPowerAg1 > ResistanceAg2,
                   !,
                   HarmAg2 is AttackPowerAg1 - ResistanceAg2,
                   assert(update(Ag2, stamina, -HarmAg2)),
                   %fight_skill_reward(SkillReward),
                   %assert(update(Ag1, fight_skill, SkillReward)), %Cuidado!! skill points o skill in battles?
                   assert(update(Ag1, attacks_won, 1)),
                   update_attr(Ag2, harmed_by_possibly_empty, CurrV, NewV, append(CurrV, [Ag1], NewV)),
                   write(Ag1), write(' succesfully attacked '), write(Ag2).

solve_attack(Ag1, Ag2):- % assert(update(Ag2, skill, +, Skill)), % Le doy skill points si resiste?
                         write(Ag2), write(' resisted the attack from '), write(Ag1).

dice(NOfSides, Random):- Random is random(NOfSides).

%commit_update(Ag, Attr, Sign, Quantity). %:- primero definir la estructura de datos para almacenar info
                                          % de los agentes.

commit_updates:- forall(retract(update(Ag, Attr, Quantity)),
                        update_attr(Ag, Attr, CurrV, NewV, NewV is max(CurrV + Quantity, 0))).


set_unconscious:- forall((ag_attr(Ag, stamina, St), St =< 0, ag_attr(Ag, unconscious, false)),
                         (update_attr(Ag, unconscious_for, _CurrV, UT, unconscious_time(UT)),
                         drop_all_objects(Ag),
                         assert(j_agent_becomes_unconscious(Ag)))). % Cuando el agente queda inconsciente, suelta todos los objetos que lleva y estos quedan en el piso.

drop_all_objects(Ag):- ag_attr(Ag, pos, Pos), forall(has(Ag, Obj), (retract(has(Ag, Obj)), assert(object_at(Obj, Pos)))),
                       assert(j_drop_all(Ag)).

%handle_move(...)
%handle_pickup(...)
start :- 
    start_env,
    run.
