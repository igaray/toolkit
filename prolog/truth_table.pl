% Author: Unknown
% Modified: Iñaki Garay, April 2005

% Truth Table Maker
% Given a boolean expression, calculates all possible truth assignments to the 
% variables in the expression and the respective value of the expression for 
% each assignment.
% To use, call tt(Expression), where Expression consists of the boolean 
% operators 'and', 'or' and 'not', and variables are lowercase letters. 
% examples:
% ?- tt(p and q).
% ?- tt(p and not q).
% ?- tt(p and not (q or r)).
% ?- tt((p or not q) and (s and not r)).

:- op(1000, xfy, 'and').
:- op(1000, xfy, 'or').
:- op(900,  fy,  'not'). 

%-------------------------------------------------------------------------------

% Boolean constants in expression */
find_vars(N, V, V) :- 
    member(N, [0, 1]),
    !.    
    
find_vars(X,Vin,Vout) :- 
    atom(X), 
    (
        member(X, Vin) -> Vout = Vin ;
        Vout = [X|Vin]
    ).

find_vars(X and Y, Vin, Vout) :- 
    find_vars(X, Vin, Vtemp),
    find_vars(Y, Vtemp, Vout).
    
find_vars(X or Y, Vin, Vout) :-  
    find_vars(X, Vin, Vtemp),
    find_vars(Y, Vtemp, Vout).
    
find_vars(not X, Vin, Vout) :-
    find_vars(X, Vin, Vout).

%-------------------------------------------------------------------------------

initial_assign([], []).
initial_assign([_|R], [0|S]) :- 
    initial_assign(R, S).

%-------------------------------------------------------------------------------

successor(A, S) :- 
    reverse(A,R),
    next(R, N),
    reverse(N, S).

%-------------------------------------------------------------------------------

next([0|R], [1|R]).
next([1|R], [0|S]) :- 
    next(R, S).

%-------------------------------------------------------------------------------

truth_value(N, _, _, N) :- 
    member(N, [0,1]).

truth_value(X, Vars, A, Val) :- 
    atom(X),
    lookup(X, Vars, A, Val).
    
truth_value(X and Y, Vars, A, Val) :- 
    truth_value(X, Vars, A, VX),
    truth_value(Y, Vars, A, VY),
    boole_and(VX, VY, Val).
    
truth_value(X or Y, Vars, A, Val) :-  
    truth_value(X, Vars, A, VX),
    truth_value(Y, Vars, A, VY),
    boole_or(VX, VY, Val).
    
truth_value(not X, Vars, A, Val) :- 
    truth_value(X, Vars, A, VX),
    boole_not(VX, Val).
    
%-------------------------------------------------------------------------------

lookup(X, [X|_], [V|_], V).
lookup(X, [_|Vars], [_|A], V) :- 
    lookup(X, Vars, A, V).

%-------------------------------------------------------------------------------

tt(E) :- 
    find_vars(E, [], V),
    reverse(V, Vars),
    initial_assign(Vars, A),
    write(' '), 
    write(Vars),
    write(' | '), 
    write(E), 
    nl,
    write('--------------------------------------------------------------------------------'), 
    nl,
    write_row(E, Vars, A),
    write('--------------------------------------------------------------------------------'), 
    nl.

%-------------------------------------------------------------------------------
write_row(E, Vars, A) :- 
    write(' '), 
    write(A), 
    write(' | '), 
    truth_value(E, Vars, A, V), 
    write(V), 
    nl,
    (
        successor(A, N) -> write_row(E, Vars, N) ; 
        true
    ).


%-------------------------------------------------------------------------------

boole_and( 0, 0, 0).      
boole_and( 0, 1, 0).      
boole_and( 1, 0, 0).      
boole_and( 1, 1, 1).      

%-------------------------------------------------------------------------------

boole_or(  0, 0, 0).      
boole_or(  0, 1, 1).
boole_or(  1, 0, 1).
boole_or(  1, 1, 1).

%-------------------------------------------------------------------------------

boole_not( 0, 1).
boole_not( 1, 0).


