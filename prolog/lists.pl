:- module(lists,
        [ member       /2, % ?Elem, +List
          nonmember    /2, % +Elem, +List
          append       /3, % ?List1, ?List2, ?List1AndList2
          prefix       /2, % ?Prefix, ?List
          suffix       /2, % ?Suffix, ?List
          sublist      /2, % ?SubList, ?List
          select       /3, % ?Elem, ?List1, ?List2
          select_first /3, %
          no_doubles   /2, %
          nextto       /3, % ?X, ?Y, ?List
          delete       /3, % 
          nth0         /3, % 
          nth1         /3, % 
          last         /2, % +List, -Element
          reverse      /2, % +List, -Reversed
          permutation  /2, % ?List, ?Permutation
          shift_right  /2, % +List, -List
          shift_left   /2, % +List, -List
          flatten      /2, % +Nested, -Flat
          sumlist      /2, % +List, -Sum
          numlist      /3  % +Low, +High, -List
        ]).

%*******************************************************************************

% member(?Elem, +List) if Elem is a member of List.

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% member can be seen as a special subcase of sublist:

% member(X,Xs) :- sublist([X],Xs).

% member can be defined using append:

% member(X,Ys) :- append(As,[X|Xs],Ys).

%*******************************************************************************

% nonmember(+Elem, +List) if Elem is not a member of List.

nonmember(_, []).
nonmember(X, [Y|Ys]) :-
    X \= Y,
    nonmember(X, Ys).

%*******************************************************************************

% append(?List1, ?List2, ?List1AndList2) if List1AndList2 is the concatenation 
% of List1 and List2.

append([], L, L).
append([H|T], L, [H|R]) :-
    append(T, L, R).

%*******************************************************************************

% prefix(?Prefix, ?List) if Prefix is a prefix of List.

% (1) straightfoward:

% prefix(Xs, Ys) :- append(Xs, As, Ys).

% (2) tail-recursive:

prefix([], _).
prefix([_|Xs], [_|Ys]) :- prefix(Xs, Ys).

%*******************************************************************************

% suffix(?Suffix, ?List) if Suffix is a suffix of List.

% (1) straightfoward:

% suffix(Xs, Ys) :- append(As, Xs, Ys).

% (2) tail-recursive:

suffix(Xs, Xs).
suffix(Xs, [_|Ys]) :- suffix(Xs, Ys).

%*******************************************************************************

% sublist(?Sublist, ?List) if Sublist is a sublist of List.

sublist([], _).
sublist([X|T], [X|Ys]) :-
    sublist(T, Ys),
    !.
sublist([X|Xs], [_|Ys]) :- 
    sublist([X|Xs], Ys).

% (1) suffix of a prefix:

% sublist(Xs, Ys) :-
%    prefix(Ps, Ys),
%    suffix(Xs, Ps).

% (2) prefix of a suffix:

% sublist(Xs,Ys) :-
%     prefix(Xs, Ss),
%     suffix(sS, Xs).

% (3) recursive definition of sublist:

% sublist(Xs, Ys)     :- prefix(Xs, Ys).
% sublist(Xs, [Y|Ys]) :- sublist(Xs, Ys).

% (4) prefix of a suffix using append:

% sublist(Xs,AsXsBs) :-
%     append(As, XsBs, AsXsBs),
%     append(Xs, Bs, XsBs).

% (5) suffix of a prefix using append:

% sublist(Xs,AsXsBs) :-
%     append(AsXs, Bs, AsXsBs),
%     append(As, Xs, AsXs).

%*******************************************************************************

% length([], 0).
% length([X|Xs], s(N)) :- length(Xs, N).

%*******************************************************************************

% select(?Elem, ?List1, ?List2) if List2 is the result of removing one 
% occurence of X from List1

select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :- select(X, Ys, Zs).

%*******************************************************************************

% select_first(X, Xs, Ys) if Ys is the list obtained by removing the first 
% occurence of X from the list Xs.

select_first(X, [X|Xs], Xs).
select_first(X, [Y|Ys], [Y|Zs]) :-
    X \= Y,
    select_first(X, Ys, Zs).

%*******************************************************************************

% no_doubles(?Xs, ?Ys) :- if Ys is the list obtained by removing duplicate 
% elements from the list Xs.

% (1) 

no_doubles([], []).
no_doubles([X|Xs], Ys) :-
    member(X, Xs),
    no_doubles(Xs, Ys).
no_doubles([X|Xs], [X|Ys]) :-
    nonmember(X, Xs),
    no_doubles(Xs, Ys).

% (2) inefficient for large lists

% no_doubles([X|Xs], Ys) :- member(X, Xs), no_doubles(Xs, Ys).
% no_doubles([X|Xs], [X|Ys]) :- delete(X, Xs, Xs1), no_doubles(Xs1, Ys).

%*******************************************************************************

% nextto(?X, ?Y, ?List) if Y follows X in List.

nextto(X, Y, [X,Y|_]).
nextto(X, Y, [_|Zs]) :-
    nextto(X, Y, Zs).

% nextto(X, Y, Zs) :- append(_, [X,Y|Ys], Zs).

%*******************************************************************************

% delete(?List1, ?Elem, ?List2) if List1, with all occurences of Elem deleted 
% results in List2.

delete([], _, []) :- !.
delete([Elem|Tail], Elem, Result) :-
    !,
    delete(Tail, Elem, Result).
delete([Head|Tail], Elem, [Head|Rest]) :-
    delete(Tail, Elem, Rest).

% delete([X|Xs], X, Ys) :- delete(Xs,X,Ys).
% delete([X|Xs], Z, [X|Ys]) :-
%     X \= Z,
%     delete(Xs,Z,Ys).
% delete([], X, []).

%*******************************************************************************

% nth0(?Index, ?List, ?Elem)
% is true when Elem is the Index'th element of List.  Counting starts
% at 0.  [This is a faster version of the original SWI-Prolog predicate.]

nth0(Index, List, Elem) :-
    integer(Index),
    !,
    Index >= 0,
    nth0_det(Index, List, Elem).    %% take nth deterministically
nth0(Index, List, Elem) :-
    var(Index),
    !,
    nth_gen(List, Elem, 0, Index).  %% match

nth0_det(0, [Elem|_], Elem) :- !.
nth0_det(1, [_,Elem|_], Elem) :- !.
nth0_det(2, [_,_,Elem|_], Elem) :- !.
nth0_det(3, [_,_,_,Elem|_], Elem) :- !.
nth0_det(4, [_,_,_,_,Elem|_], Elem) :- !.
nth0_det(5, [_,_,_,_,_,Elem|_], Elem) :- !.
nth0_det(N, [_,_,_,_,_,_   |Tail], Elem) :-
    M is N - 6,
    nth0_det(M, Tail, Elem).

nth_gen([Elem|_], Elem, Base, Base).
nth_gen([_|Tail], Elem, N, Base) :-
    succ(N, M),
    nth_gen(Tail, Elem, M, Base).

%*******************************************************************************

% nth1(?Index, ?List, ?Elem)
% Is true when Elem is the Index'th element of List.  Counting starts
% at 1.  [This is a faster version of the original SWI-Prolog predicate.]

nth1(Index1, List, Elem) :-
    integer(Index1),
    !,
    Index0 is Index1 - 1,
    nth0_det(Index0, List, Elem).   %% take nth deterministically
nth1(Index, List, Elem) :-
    var(Index), !,
    nth_gen(List, Elem, 1, Index).  %% match

%*******************************************************************************

% last(?List, ?Elem)
%
% Succeeds if `Last' unifies with the last element of `List'.

last([X|Xs], Last) :-
    last_(Xs, X, Last).

last_([], Last, Last).
last_([X|Xs], _, Last) :-
    last_(Xs, X, Last).

% last(X,Xs) :- append(As,[X],Xs).

%*******************************************************************************

% reverse(?List1, ?List2)
%
% Is true when the elements of List2 are in reverse order compared to List1.

reverse(Xs, Ys) :- reverse(Xs, [], Ys, Ys).

reverse([], Ys, Ys, []).
reverse([X|Xs], Rs, Ys, [_|Bound]) :-
    reverse(Xs, [X|Rs], Ys, Bound).

% (1) naive

% reverse([],[]).
% reverse([X|Xs],Zs) :- reverse(Xs,Ys), append(Ys,[X],Zs).

% (2) with an accumulator

% reverse(Xs,Ys) :- reverse(Xs,[],Ys).
% reverse([X|Xs],Acc,Ys) :- reverse(Xs,[X|Acc],Ys).
% reverse([],Ys,Ys).

%*******************************************************************************

shift_right([], []).
shift_right([E1|[]], [E1|[]]).
shift_right([E|A], [E2, E|B]) :- 
    shift_right(A, [E2|B]).

%*******************************************************************************
    
shift_left([], []).
shift_left([E1|[]], [E1|[]]).
shift_left([E, E2|A], [E2|B]) :- 
    shift_left([E|A], B).

%*******************************************************************************

% permutation(?Xs, ?Ys)
%       
% permutation(Xs, Ys) is true when Xs is a permutation of Ys. This
% can solve for Ys given Xs or Xs given Ys, or even enumerate Xs
% and Ys together.

permutation(Xs, Ys) :- permutation(Xs, Ys, Ys).

permutation([], [], []).
permutation([X|Xs], Ys1, [_|Bound]) :-
    permutation(Xs, Ys, Bound),
    select(X, Ys1, Ys).

%*******************************************************************************

% flatten(+List1, ?List2)
%
% Is true when Lis2 is a non nested version of List1.

flatten(List, FlatList) :-
    flatten(List, [], FlatList0),
    !,
    FlatList = FlatList0.

flatten(Var, Tl, [Var|Tl]) :-
    var(Var),
    !.
flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :-
    flatten(Hd, FlatHeadTail, List),
    flatten(Tl, Tail, FlatHeadTail).
flatten(Atom, Tl, [Atom|Tl]).

%*******************************************************************************

% sumlist(+List, -Sum)
%       
% Sum is the result of adding all numbers in List.

sumlist(Xs, Sum) :- sumlist(Xs, 0, Sum).

sumlist([], Sum, Sum).
sumlist([X|Xs], Sum0, Sum) :-
    Sum1 is Sum0 + X,
    sumlist(Xs, Sum1, Sum).

%*******************************************************************************

% numlist(+Low, +High, -List)
%       
% List is a list [Low, Low+1, ... High]

numlist(L, U, Ns) :-
    integer(L),
    integer(U),
    L =< U,
    numlist_(L, U, Ns).

numlist_(L, U, [L|Ns]) :-
    (   L =:= U
    ->  Ns = []
    ;   M is L + 1,
        numlist_(M, U, Ns)
    ).

%*******************************************************************************
