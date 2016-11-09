%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                       AUXILIARY PREDICATES                                       %
%                                                                                                  %
% Important observation: these auxiliary predicates may be used by either the environment          %
% simulator or the agent. Therefore, none of them access information by means of the dynamic       %
% predicates.                                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% next_position( +Current_Position, +Direction, -Next_Position )                                   %
% Given a current position and a direction, calculates what the next position will be.             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
next_position( [F, C], Direction, [F1, C1] ) :-
    (
        (
            Direction = adelante,
            F1 is F - 1,
            C1 = C
        );
        (
            Direction = atras,
            F1 is F + 1,
            C1 = C
        );
        (
            Direction = derecha,
            F1 = F,
            C1 is C + 1
        );
        (
            Direction = izquierda,
            F1 = F,
            C1 is C - 1
        );
        (
            Direction = apagar,
            F1 = F,
            C1 = C
        );
        (
            Direction = aspirar,
            F1 = F,
            C1 = C
        );
        (
            Direction = none,
            F1 = F,
            C1 = C
        )
    ).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prev_position( +Direction, +Current_Position, -Prev_Position )                                   %
% Given a direction and a current position, calculates the previous position.                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prev_position( Direction, [F, C], [F1, C1] ) :-
    (
        (
            Direction = adelante,
            F1 is F - 1,
            C1 = C
        );
        (
            Direction = atras,
            F1 is F + 1,
            C1 = C
        );
        (
            Direction = derecha,
            F1 = F,
            C1 is C + 1
        );
        (
            Direction = izquierda,
            F1 = F,
            C1 is C - 1
        );
        (
            Direction = apagar,
            F1 = F,
            C1 = C
        );
        (
            Direction = aspirar,
            F1 = F,
            C1 = C
        );
        (
            Direction = none,
            F1 = F,
            C1 = C
        )
    ).    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% direction( +Current_Position, +Next_Position, -Direction )                                       %
% Given a current position and a next position, calculates what direction must be taken to arrive  %
% at the next position starting from current position.                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
direction( [F1, C1], [F2, C2], Direction ) :- 
    (
        (
            F2 is F1 - 1,
            C2 = C1,
            Direction = adelante
        );
        (
            F2 is F1 + 1,
            C2 = C1,
            Direction = atras
        );
        (
            F2 = F1,
            C2 is C1 - 1,
            Direction = izquierda
        );
        (
            F2 = F1,
            C2 is C1 + 1,
            Direction = derecha
        );
        (
            F2 = F1,
            C2 = C1,
            Direction = aspirar
        );
        (
            F2 = F1,
            C2 = C1,
            Direction = apagar
        ),
        (
            F2 = F1,
            C2 = C1,
            Direction = none
        )
    ).
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replace/2                                                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace(X, Y) :- retract(X), !, assert(Y).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replace/2                                                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace_all(X, Y) :- retractall(X), !, assert(Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                        OUTPUT PREDICATES                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
position_to_string( [F, C], String ) :-
    number_to_string(F, Fs),
    number_to_string(C, Cs),
    string_concat( '[', Fs,   S1 ),
    string_concat( S1,  ',', S2 ),
    string_concat( S2,  Cs,   S3 ),
    string_concat( S3,  ']', String).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
number_to_string( N, Ns ) :-
    N < 10,
    string_concat( '0', N, Ns ).
number_to_string( N, Ns ) :-
    string_concat( N, '', Ns ).    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
string_concatenation([], '').
string_concatenation([S], S1) :-
    string_concat(S, '', S1).
string_concatenation([S1, S2|T], S3) :-
    string_concat(S1, S2, Temp),
    string_concatenation([Temp|T], S3).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_thought( String, init, Current_Position, Next_Position, Action, Thought ) :-
    NL = '*',
    position_to_string( Current_Position, CPS ),
    position_to_string( Next_Position, NPS ),
    string_to_atom(Action, AS),
    string_concatenation(
        [String, NL, 
        'Percept: init', NL, 
        'Current position: ', CPS, NL, 
        'Next position: ', NPS, NL, 
        'Action: ', AS, NL],
        Thought).
update_thought( String, [Choco, Sucio, Alfombra, Energia], Current_Position, Next_Position, Action, Thought ) :-
    string_to_list(NL, [10]),
    position_to_string( Current_Position, CPS ),
    position_to_string( Next_Position, NPS ),
    string_to_atom(Action, AS),
    string_concatenation(
        [String, NL, 
        'Percept: ', '[', Choco, ',', Sucio, ',', Alfombra, ',', Energia, ']', NL, 
        'Current position: ', CPS, NL, 
        'Next position: ', NPS, NL, 
        'Action: ', AS, NL],
        Thought).
