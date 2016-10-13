% call from prolog as:
% swipl -f prolog.pl -g "process_file('data.pl','tmp.txt')",halt -t "halt(1)"

:- op(1101, xfx, -<).    % Defeasible Rules
:- op(1101, xfx, <-).    % Strict Rules
:- op(190,  fx,  ~).     % Strong negation
:- op(191,  fx,  not).   % Default negation
:- op(192,  fx,  '?').   % To avoid requiring parentheses when asking for an explanation

built_in(P) :-
    predicate_property(P, built_in).

process_file(Input_File, Output_File) :-
    open(Input_File,  read,   In),
    open(Output_File, write, Out),
    read(In, First_Term),
    process(First_Term, In, Out),
    close(In),
    close(Out).

process(end_of_file, _Ini, _Out) :- !.
process(Term, In, Out) :-
    output_term(Term, Out),
    read(In, Next_Term),
    process(Next_Term, In, Out).

output_term(Term, _Out) :-
    functor(Term, ':-', 1).
output_term(Term, Out) :-
    functor(Term, ':-', 2),
    arg(1, Term, Head),
    arg(2, Term, Body),
    body_to_list(Body, List),
    list_to_set(List, Set),
    sort(Set, Sorted),
    output_head(Out, Head),
    output_body(Out, Sorted).
output_term(Term, Out) :-
    output_fact(Out, Term).

output_fact(Out, Fact) :-
    functor(Fact, Functor, Arity),
    write(Out, Functor), write(Out, '/'), write(Out, Arity), write(Out, ',\n').

% This clause is for dealing with predicates from another module. e.g user:prolog_exception_hook(...)
output_head(Out, Head) :-
    functor(Head, ':', 2),
    arg(1, Head, Module),
    arg(2, Head, True_Head),
    functor(True_Head, Functor, Arity),
    write(Out, Module), write(Out, ':'), write(Out, Functor), write(Out, '/'), write(Out, Arity), write(Out, ',').
output_head(Out, Head) :-
    functor(Head, Head_Functor, Head_Arity),
    write(Out, Head_Functor), write(Out, '/'), write(Out, Head_Arity), write(Out, ',').

output_body(Out, []) :- 
    write(Out, '\n').
output_body(Out, [[Functor, Arity] | Body]) :-
    write(Out, Functor), write(Out, '/'), write(Out, Arity), write(Out, ','),
    output_body(Out, Body).

body_to_list(Body, List) :-
    body_to_list(Body, [], List).

body_to_list(Term, Acc0, Body_List) :-
    functor(Term, ';', 2),
    arg(1, Term, Clause1),
    arg(2, Term, Clause2),
    body_to_list(Clause1, Acc0, Acc1),
    body_to_list(Clause2, Acc1, Body_List).
body_to_list(Term, Acc0, Body_List) :-
    functor(Term, ',', 2),
    arg(1, Term, Clause1),
    arg(2, Term, Clause2),
    body_to_list(Clause1, Acc0, Acc1),
    body_to_list(Clause2, Acc1, Body_List).
body_to_list(Term, Acc, Acc) :-
    built_in(Term).
body_to_list(Term, Acc, [[Functor,Arity] | Acc]) :- 
    functor(Term, Functor, Arity).

