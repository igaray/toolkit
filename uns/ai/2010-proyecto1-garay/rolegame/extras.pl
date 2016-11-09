% extras.pl
% Inteligencia Artificial - 2005
% DCIC - Universidad Nacional del Sur
% Author: Telma Delladio Fedi
%
% Predicados auxiliares
/*----------------------------------------------*/
                             

% element_at(Pos, List, Elem)

element_at(1, [X|_Xs], X):- !.

element_at(Pos, [_|Xs], X):-
                Pos > 1,
                PrePos is Pos -1,
                element_at(PrePos, Xs, X).

display_env:- turn(T),
              write('turn: '), write(T),nl,
              display_grid.

display_grid:- tab(5), write('  '), forall(col(C), (write(C), write(' '))), nl,
               fila(F),
               tab(5),write(F),
               write(' '),mostrarFila(F),nl,
               fail.
          
display_grid.

%mostrar:- posAg(X,Y),
%          write('Agente en posición: '),
%          write(X),write('-'),write(Y),nl.

/*----------------------------------------------*/
mostrarFila(F):-
                col(C),
                queMuestro(F,C,Esto),
                write(Esto), tab(1), fail.

mostrarFila(_).

/*----------------------------------------------*/
%queMuestro(F,C,AgCh):- ag_attr(AgName, pos, [F,C]),
%                     ag_attr(AgName, dir, Dir),
%                     ag_char(Dir, AgCh),
%                     !.

queMuestro(F,C,BldCh):- at([hostel, _BldName], [F,C]),
                        to_write(hostel, BldCh),
                        !.
                     
queMuestro(F,C,AgCh):-  at(Thing, [F,C]),
                        Thing = [agent, AgName],
                        ag_attr(AgName, dir, Dir),
                        ag_char(Dir, AgCh),
                        !.
                        
queMuestro(F,C,ObjCh):- at(Thing, [F,C]),
                        Thing=[ObjType, _ObjName],
                        to_write(ObjType, ObjCh),
                        !.
                        
                     
queMuestro(F,C,X):- cell_land_to_write([F,C], X).

cell_land_to_write([F,C], XToWrite):- cell_land([F,C], X),
                                    to_write(X, XToWrite).
                                 
to_write(plain, p).
to_write(forest, f).
to_write(mountain, m).
to_write(treasure, t).
to_write(hostel, '[]').

ag_char(n, '^').

ag_char(e, ':o').

ag_char(s, 'ö').

ag_char(w, 'o:').
/*----------------------------------------------*/
%fila(F):- member(F,[1,2,3,4,5]).
%col(C):- member(C,[1,2,3,4,5]).

fila(F):- n_of_arrows(NofAs), nnleq(F, NofAs).

col(C):- n_of_columns(NofCs), nnleq(C, NofCs).

nn_pred(1, 2).

nn_pred(PredM, M):- M > 2, PredM is M - 1.


nnleq(N, M):- nn_pred(PredM, M), nnleq(N, PredM).

nnleq(M, M).


/*----------------------------------------------*/
%next(N,Nn):- N\=5, %not(var(N)), %creo que el not(var(N)) está de más!!!
%             !,
%             Nn is N+1.
%
%next(N,Nn):- Nn\=1, %not(var(Nn)),
%             !,
%             N is Nn-1.
%next(5,5):-!.
%next(1,1).

/*----------------------------------------------*/

next(N,Nn):- not(var(N)), !,
             Nn is N+1.

next(N,Nn):- not(var(Nn)), !,
             N is Nn-1.


next_90_clockwise(n, e).
next_90_clockwise(e, s).
next_90_clockwise(s, w).
next_90_clockwise(w, n).

pos_at_sight(AgPos, AgDir, Pos):- next_90_clockwise(PrevAgDir, AgDir),
                                  ady_at_cardinal(AgPos, PrevAgDir, AdyAgPos),
                                  vision_length(VisionLength),
                                  Pred2VisionLength is VisionLength -2,
                                  pos_at_Dir_of(PrevAgDir, AdyAgPos, Pred2VisionLength, PosAtLeft),
                                  pos_at_Dir_of(AgDir, PosAtLeft, VisionLength, Pos).
%unproudly coded by Betungo!

pos_at_sight(AgPos, AgDir, Pos):- vision_length(VisionLength),
                                  pos_at_Dir_of(AgDir, AgPos, VisionLength, Pos).

pos_at_sight(AgPos, AgDir, Pos):- next_90_clockwise(AgDir, NextAgDir),
                                  ady_at_cardinal(AgPos, NextAgDir, AdyAgPos),
                                  vision_length(VisionLength),
                                  Pred2VisionLength is VisionLength -2,
                                  pos_at_Dir_of(NextAgDir, AdyAgPos, Pred2VisionLength, PosAtRight),
                                  pos_at_Dir_of(AgDir, PosAtRight, VisionLength, Pos).
                                  

% pos_at_Dir_of(Dir, Pos, LengthBound, PosAtDir)
                                  
pos_at_Dir_of(_Dir, Pos, _LengthBound, Pos). %:- ady_at_cardinal(Pos, Dir, PosAtDir).

pos_at_Dir_of(Dir, Pos, LengthBound, PosAtDir):-
                                    LengthBound >= 1,
                                    PredLengthBound is LengthBound - 1,
                                    ady_at_cardinal(Pos, Dir, FirstPosAtDir),
                                    pos_at_Dir_of(Dir, FirstPosAtDir, PredLengthBound, PosAtDir).
                                    
% ady_at_cardinal(Pos, Dir, PosAtDir)

ady_at_cardinal([F,C], n, [PredF, C]):- next(PredF, F).
ady_at_cardinal([F,C], e, [F, SuccC]):- next(C, SuccC).
ady_at_cardinal([F,C], s, [SuccF, C]):- next(F, SuccF).
ady_at_cardinal([F,C], w, [F, PredC]):- next(PredC, C).


% if_fails_do(Goal, ExceptionHandler)
if_fails_do(Goal, _ExceptionHandler):- call(Goal), !.

if_fails_do(_Goal, ExceptionHandler):- call(ExceptionHandler), fail.

implies(Ant, Cons):- call(Ant), !,
                     call(Cons).

implies(_Ant, _Cons).

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

waterNESW(WPos, N, E, S, W):-
ady_at_cardinal(WPos, n, PosN),
( (cell_land(PosN, water) ; not(cell_land(PosN, _)) ),
  N = 1
  ;
  N = 0 ),
ady_at_cardinal(WPos, e, PosE),
( (cell_land(PosE, water) ; not(cell_land(PosE, _)) ),
  E = 1
  ;
  E = 0 ),
ady_at_cardinal(WPos, s, PosS),
( (cell_land(PosS, water) ; not(cell_land(PosS, _)) ),
  S = 1
  ;
  S = 0 ),
ady_at_cardinal(WPos, w, PosW),
( (cell_land(PosW, water) ; not(cell_land(PosW, _)) ),
  W = 1
  ;
  W = 0 ),
  !.


/*
:- dynamic ag_instance_name/1.

instance_id(A):- member(A, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

start_ag_instance:- ag_class_name(ACN),
                    instance_id(A),
                    AgInstanceName =.. [ACN, A],
                    register_me(AgInstanceName, Status),
                    write('REGISTRATION STATUS: '),
                    write(Status), nl, nl,
                    Status = connected,
                    assert(ag_instance_name(AgInstanceName)),
                    run.
*/