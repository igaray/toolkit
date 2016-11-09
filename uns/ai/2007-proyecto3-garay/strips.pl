%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STRIPS REGRESSION PLANNER

%-------------------------------------------------------------------------------
% Initial world description. 
estadoInicial([libre(a), 
               libre(b),
               libre(c),
               enMesa(a),
               enMesa(b),
               enMesa(c)]).
               
%-------------------------------------------------------------------------------
% plan(+Metas, -Plans). In go the goals, out comes the plan.

%-------------------------------------------------------------------------------
% solve(GL, W) is true if every element of goal list GL is true in world W.

solve(GoalSet, init) :-
    holdsall(GoalSet, init).

solve(GoalSet, do(A, W)) :-
    consistent(GoalSet),
    choose_goal(Goal, GoalSet),
    choose_action(Action, Goal),
    wp(Action, GoalSet, NewGoalSet),
    solve(NewGoalSet, W).

%-------------------------------------------------------------------------------
wp(A, [], P) :- 
    preconditions(A, P).

wp(A, [G|R], P1) :- 
    wp(A, R, P0),
    regress(G, A, P0, P1).
    
%-------------------------------------------------------------------------------
regress(G, A, P, P) :- 
    achieves(A, G).
regress(G, A, P, [G|P]) :- 
    not_on_add_list(A, G),
    not_on_delete_list(A, G).

%-------------------------------------------------------------------------------
% holds(C, S) means that C is true in situation S.
% The following is a representation of STRIPS in Situation Calculus, specifying 
% when the primitive relation condition C is true immediately after actian A is 
% performed in situation W.
    
holds(C, do(A, W)) :-
    preconditions(A, P), 
    holdsall(P, W),
    add_list(A, AL), 
    member(C, AL).
holds(C, do(A, W)) :-
    preconditions(A, P),
    holdsall(P, W),
    delete_list(A, DL),
    notin(C, DL),
    holds(C, W).
    
%-------------------------------------------------------------------------------
% holdsall(L, W) means all the conditions in the list L hold in world W.

holdsall([]).
holdsall([C|L], W) :-
    holds(C, W),
    holdsall(L, W).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BLOCKS WORLD

preconditions( apilar(A, B), [ libre(A), libre(B), enMesa(A) ] ).
preconditions( desapilar(A, B), [ libre(A), sobre(A, B) ] ).

add_list(      apilar(A, B), [ sobre(A, B) ] ).
add_list(      desapilar(A, B), [ libre(B), enMesa(A) ] ).

delete_list(   apilar(A, B), [ libre(B) ] ).
delete_list(   desapilar(A, B), [ sobre(A, B) ] ).

