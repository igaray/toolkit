% Predicados auxiliares

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_ag(AgName, Perc):-

                  write('agent: '), write(AgName), nl,

                  Perc=[Turn, Vision, Attrs, Inv],
                  
                  write('turn: '), write(Turn), nl,
                  
                  member([stamina, St], Attrs),
                  
                  write('stamina: '), write(St), nl,
                  
                  write('Inventory: '), write(Inv), nl,
                  
                  objects_at_sight(Vision, ObjectsAtSight),
                  
                  write('at sight: '),
                  
                  forall(member([Pos, Obj], ObjectsAtSight), (write(Obj), write(' en '), write(Pos), nl)), nl.
                  
objects_at_sight(Vision, ObjectsAtSight):-
                         findall([Pos, Obj], (member([Pos, _Land, Objects], Vision), member(Obj, Objects)), ObjectsAtSight).
                         

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% if_fails_do(Goal, ExceptionHandler)
if_fails_do(Goal, _ExceptionHandler):- call(Goal), !.

if_fails_do(_Goal, ExceptionHandler):- call(ExceptionHandler), fail.

implies(Ant, Cons):- call(Ant), !,
                     call(Cons).

implies(_Ant, _Cons).


/*----------------------------------------------*/
nn_pred(1, 2).

nn_pred(PredM, M):- M > 2, PredM is M - 1.


nnleq(N, M):- nn_pred(PredM, M), nnleq(N, PredM).

nnleq(M, M).

/*----------------------------------------------*/

next(N,Nn):- not(var(N)), !,
             Nn is N+1.

next(N,Nn):- not(var(Nn)), !,
             N is Nn-1.


next_90_clockwise(n, e).
next_90_clockwise(e, s).
next_90_clockwise(s, w).
next_90_clockwise(w, n).


% ady_at_cardinal(Pos, Dir, PosAtDir)

ady_at_cardinal([F,C], n, [PredF, C]):- next(PredF, F).
ady_at_cardinal([F,C], e, [F, SuccC]):- next(C, SuccC).
ady_at_cardinal([F,C], s, [SuccF, C]):- next(F, SuccF).
ady_at_cardinal([F,C], w, [F, PredC]):- next(PredC, C).

