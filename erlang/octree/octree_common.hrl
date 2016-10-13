%--------------------------------------------------------------------------------------------------%
base_case(Xmin, Ymin, Zmin, Xmax, Ymax, Zmax) ->
    (Xmin == Xmax) andalso (Ymin == Ymax) andalso (Zmin == Zmax).
    
%--------------------------------------------------------------------------------------------------%
% X, Y, Z are the coordinates where we are inserting
% Xmin, YMin, Zmin are the coordinates of the lower-top-left point
% Xmax, Ymax, Zmax are the coordinates of the upper-bottom-right point
% (X, Y, Y) is checked to see into which octant it falls. Depending on the result, the Data elements
% of the other children are recovered and compared. 
% If they are all the same term and also equal to the Data being inserted, return true.
% If the children are inner nodes or the Data is different, return false. 
merge_case(Children) ->
    CD1 = element(8, element(1, Children)), 
    CD2 = element(8, element(2, Children)), 
    CD3 = element(8, element(3, Children)), 
    CD4 = element(8, element(4, Children)), 
    CD5 = element(8, element(5, Children)), 
    CD6 = element(8, element(6, Children)), 
    CD7 = element(8, element(7, Children)), 
    CD8 = element(8, element(8, Children)),
    (CD1 =:= CD2) andalso 
    (CD2 =:= CD3) andalso 
    (CD3 =:= CD4) andalso 
    (CD4 =:= CD5) andalso 
    (CD5 =:= CD6) andalso 
    (CD6 =:= CD7) andalso 
    (CD7 =:= CD8).

%--------------------------------------------------------------------------------------------------%
within(X, Y, Z, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax) -> 
    (Xmin =< X) andalso 
    (X =< Xmax) andalso 
    (Ymin =< Y) andalso 
    (Y =< Ymax) andalso 
    (Zmin =< Z) andalso 
    (Z =< Zmax).

%--------------------------------------------------------------------------------------------------%
% TODO optimize this to minimize comparisons.
in_octant(X, Y, Z, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax) ->
    W1 = within(X, Y, Z, 
        Xmin, Ymin, Zmin, 
        ((Xmax + Xmin) div 2), ((Ymax + Ymin) div 2), ((Zmax + Zmin) div 2)),
    W2 = within(X, Y, Z, 
        ((Xmax + Xmin) div 2) + 1, Ymin, Zmin, 
        Xmax, ((Ymax + Ymin) div 2), ((Zmax + Zmin) div 2)),
    W3 = within(X, Y, Z, 
        Xmin, ((Ymax + Ymin) div 2) + 1, Zmin, 
        ((Xmax + Xmin) div 2), Ymax, ((Zmax + Zmin) div 2)),
    W4 = within(X, Y, Z, 
        ((Xmax + Xmin) div 2) + 1, ((Ymax + Ymin) div 2) + 1, Zmin, 
        Xmax, Ymax, ((Zmax + Zmin) div 2)),
    W5 = within(X, Y, Z, 
        Xmin, Ymin, ((Zmax + Zmin) div 2) + 1, 
        ((Xmax + Xmin) div 2), ((Ymax + Ymin) div 2), Zmax),
    W6 = within(X, Y, Z, 
        ((Xmax + Xmin) div 2) + 1, Ymin, ((Zmax + Zmin) div 2) + 1, 
        Xmax, ((Ymax + Ymin) div 2), Zmax),
    W7 = within(X, Y, Z, 
        Xmin, ((Ymax + Ymin) div 2) + 1, ((Zmax + Zmin) div 2) + 1, 
        ((Xmax + Xmin) div 2), Ymax, Zmax),
    W8 = within(X, Y, Z, 
        ((Xmax + Xmin) div 2) + 1, ((Ymax + Ymin) div 2) + 1, ((Zmax + Zmin) div 2) + 1, 
        Xmax, Ymax, Zmax),
    if
        W1 -> 1; % lower_top_left
        W2 -> 2; % lower_top_right
        W3 -> 3; % lower_bottom_left
        W4 -> 4; % lower_bottom_right
        W5 -> 5; % upper_top_left
        W6 -> 6; % upper_top_right
        W7 -> 7; % upper_bottom_left
        W8 -> 8  % upper_bottom_right
    end.

