% Reflective Arithmetic Predicates
% Numbers are represented in s^n(0) notation.

generate(0).
generate(s(N)):-
    generate(N),
    natural_number(N).

natural_number(0).
natural_number(s(N)) :- 
    natural_number(N).

natural_number_to_integer(0, 0).
natural_number_to_integer(s(N), M) :- 
    natural_number_to_integer(N, M1),
    M is M1 + 1.

integer_to_natural_number(0, 0).
integer_to_natural_number(M, s(N)) :- 
    M1 is M - 1,
    integer_to_natural_number(M1, N).

% add1/2

add1(N, s(N)).

% addition/3
% 0 + X = X for all X
% (X + 1) + Y = (Z + 1) for all X, Y, Z such that X + Y = Z

addition(0,X,X).
addition(s(X), Y, s(Z)) :- 
    addition(X, Y, Z).

% multiplication/3
% 0 * X = 0 for all X 
% (X + 1) * Y = Z for all X, Y, Z such that Z = (X * Y) + Y

multiplication(0, _, 0).
multiplication(s(X), Y, Z) :- 
    multiplication(X, Y, XY), 
    addition(XY, Y, Z).

% 0 ^ (X + 1) = 0
% (X + 1) ^ 0 = 1
% X ^ (N + 1) = Y for all N, X, Y such that Y = (X ^ N) * X

exp(s(X), 0, 0).
exp(0, s(X), s(0)).
exp(s(N), X, Y) :- 
    exp(N, X, Z), 
    multiplication(Z, X, Y).

% 0! = 1
% (X + 1)! = X! * (X + 1) for all X

factorial(0, s(0)).
factorial(s(X), Y) :- 
    factorial(X,Z), 
    multiplication(s(X), Z, Y). 

% 0 <= X for all X
% X + 1 <= Y + 1 for all X,Y such that X <= Y

less_or_equal(0,X) :- 
    natural_number(X).
less_or_equal(s(X), s(Y)) :- 
    less_or_equal(X, Y).

minimum(N1,N2,N1) :- 
    less_or_equal(N1, N2).
minimum(N1,N2,N2) :- 
    less_or_equal(N2, N1). 

