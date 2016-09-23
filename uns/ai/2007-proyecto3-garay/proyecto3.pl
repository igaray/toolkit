%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STRIPS PLANNER

% plan(+Metas, -Plan)

estadoInicial([libre(a), 
               libre(b),
               libre(c),
               enMesa(a),
               enMesa(b),
               enMesa(c)]).

plan(Metas, Plan) :- 
    estadoInicial(EI),     
    achieve_all(Metas, EI, _).

%-------------------------------------------------------------------------------
% achieve_all(Gs, W1, W2) is true if W2 is the resulting world after achieving 
% every element of the list Gs of goals from the world W1.

% achieve_all(Goals, World1, World2) means that World2 is the resulting world 
% after achieving each goal in the list of Goals from World1.

achieve_all([], W0, W0).
achieve_all(Goals, W0, W2) :- 
    remove(G, Goals, Rem_Gs),
    achieve(G, W0, W1),
    achieve_all(Rem_Gs, W1, W2).
    
%-------------------------------------------------------------------------------
% achieve(G, W0, W1) is true if W1 is the resulting world after achieving goal 
% G from the world W0.

% achieve(Goal, World0, World1) means that World2 is the resulting world after
% achieving Goal from World1. The first clause is for goals that already hold 
% in the world W. The second clause is for derived relations, and the third 
% clause is for primitive relations.

achieve(G, W, W) :- 
    holds(G, W).
achieve(G, W0, W1) :-
    clause(G, B), 
    achieve_all(B, W0, W1).
achieve(G, W0, do(Action, W1)) :-
    achieves(Action, G), 
    preconditions(Action, Pre), 
    achieve_all(Pre, W0, W1).
    
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

% preconditions(Action, Pre) is true if Pre is the list of preconditions of
% Action. 

% add_list(A, L) is true if L is the list of primitive predicates added by 
% action A.

% delete_list(A, L) is true if L is the list of primitive predicates deleted by 
% action A.

%-------------------------------------------------------------------------------
% Accion: apilar(A, B)
% El bloque A se apila sobre el bloque B siempre y cuando ambos bloques esten
% libres y el bloque A se encuentre sobre la mesa.

preconditions( apilar(A, B), [ libre(A), libre(B), enMesa(A) ] ).
add_list(      apilar(A, B), [ sobre(A, B) ] ).
delete_list(   apilar(A, B), [ libre(B) ] ).

%-------------------------------------------------------------------------------
% Accion: desapilar(A, B)
% Desapila sobre la mesa el bloque A que se encuentre sobre el B siempre y 
% cuando el bloque A este sobre el bloque B y ademas este libre. 

preconditions( desapilar(A, B), [ libre(A), sobre(A, B) ] ).
add_list(      desapilar(A, B), [ libre(B), enMesa(A) ] ).
delete_list(   desapilar(A, B), [ sobre(A, B) ] ).

% sobre(A, B) es verdadero si el bloque A esta sobre el bloque B.

% libre(A) is verdader si el bloque esta libre.

% enMesa(A) es verdadero si el bloque A esta sobre la mesa.

% estadoInicial(L) representa el estado inicial. L es una lista de instancias
% de relaciones que se verifician en el estado inicial.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AUXILIAR PREDICATES 

%-------------------------------------------------------------------------------
% notin(C, DL) is true if C is not a member of list DL.

notin(_X, []).
notin(X, [H|T]) :- X \= H, notin(X, T).
