/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DELIVERY ROBOT WORLD DOMAIN SPECIFIC KNOWLEDGE

%-------------------------------------------------------------------------------
unsatisfiable(L) :-
    member(sitting_at(X1, Y1), L),
    member(sitting_at(X2, Y2), L),
    X1 == X2,
    \+ (Y1 = Y2).
unsatisfiable(L) :-
    member(sitting_at(X1, _), L),
    member(carrying(_, Y2), L),
    X1 == Y2.
unsatisfiable(L) :-
    member(carrying(X1, Y1), L),
    member(carrying(X2, Y2), L),
    Y1 == Y2,
    \+ (X1 = X2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINITION OF DELIVERY ROBOT WORLD IN STRIPS NOTATION 

% ACTIONS

% move(Ag, Pos, Pos_1) is the action of Ag moving from Pos to Pos_1

preconditions(move(Ag, Pos, Pos_1), [autonomous(Ag), adjacent(Pos, Pos_1), sitting_at(Ag, Pos)]).
achieves(move(Ag, Pos, Pos_1), sitting_at(Ag, Pos_1)).
deletes(move(Ag, Pos, Pos_1), sitting_at(Ag, Pos)).

% pickup(Ag, Obj, Pos) is the action of agent Ag picking up Obj.
preconditions(pickup(Ag, Obj, Pos), [autonomous(Ag), Ag \= Obj, sitting_at(Obj, Pos), at(Ag, Pos) ]).
achieves(pickup(Ag, Obj, Pos), carrying(Ag, Obj)).
deletes(pickup(Ag, Obj, Pos), sitting_at(Obj, Pos)).

% putdown(Ag,Obj,Pos)
preconditions(putdown(Ag, Obj, Pos), [autonomous(Ag), Ag \= Obj, at(Ag, Pos), carrying(Ag, Obj)]).
achieves(putdown(Ag, Obj, Pos), sitting_at(Obj, Pos)).
deletes(putdown(Ag, Obj, Pos), carrying(Ag, Obj)).

% unlock(Ag,Door)
preconditions(unlock(Ag,Door), [autonomous(Ag), blocks(Door, P_1,_), opens(Key, Door), carrying(Ag, Key), at(Ag, P_1)]).
achieves(unlock(Ag, Door), unlocked(Door)).

% PRIMITIVE RELATIONS

primitive(carrying(_, _)).
primitive(sitting_at(_, _)).
primitive(unlocked(_)).

% DERIVED RELATIONS

at(Obj, Pos) <-
    [sitting_at(Obj, Pos)].
at(Obj, Pos) <-
    [autonomous(Ag), Ag \= Obj, carrying(Ag, Obj), at(Ag, Pos)].

adjacent(o109, o103) <- [].
adjacent(o103, o109) <- [].
adjacent(o109, lng) <- [].
adjacent(lng, o109) <- [].
adjacent(o109, o111) <- [].
adjacent(o111, o109) <- [].
adjacent(o103, mail) <- [].
adjacent(mail, o103) <- [].
adjacent(lab2, o109) <- [].
adjacent(P_1, P_2) <- [blocks(Door, P_1, P_2), unlocked(Door)].
blocks(door1, o103, lab2) <- [].
opens(k1,door1) <- [].
autonomous(rob) <- [].

% INITIAL SITUATION
holds(sitting_at(rob, o109), init).
holds(sitting_at(parcel, lng), init).
holds(sitting_at(k1, mail), init).

achieves(init, X) :-
    holds(X, init).
    
*/    

