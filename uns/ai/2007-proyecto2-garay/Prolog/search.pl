% A* SEARCH

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% astar_search/4
astar_search(Position, Solution, Cost, Frontier_List) :-
    astar_search([node(Position, [], 0)], [], Solution1, Cost, Frontier_List),
    reverse(Solution1, [Position|Solution]).
astar_search(_Position, [], 0, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% astar_search/5
astar_search(Frontier, _Visited, [Position|Path], Path_Cost, [Frontier_Positions]) :-
    astar_select(node(Position, Path, Path_Cost), Frontier, _Frontier1),
    is_goal(Position),
    frontier_positions(Frontier, Frontier_Positions).

astar_search(Frontier, Visited, Solution, Cost, [Frontier_Positions|Frontier_List]) :-
    astar_select(node(Position, Path, Path_Cost), Frontier, Frontier1),
    neighbours(node(Position, Path, Path_Cost), Frontier, Visited, Neighbours),
    add_paths(Neighbours, node(Position, Path, Path_Cost), New_Frontier_Elements),
    astar_add_to_frontier(New_Frontier_Elements, Frontier1, New_Frontier),
    frontier_positions(New_Frontier, Frontier_Positions),
    astar_search(New_Frontier, [node(Position, Path, Path_Cost)|Visited], Solution, Cost, Frontier_List).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% astar_select/3
astar_select(Node, [Node|Frontier], Frontier).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% astar_add_to_frontier/3
astar_add_to_frontier(Neighbours, Frontier1, Frontier3) :-
    append(Frontier1, Neighbours, Frontier2),
    sort_by_f(Frontier2, Frontier3).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sort_by_f/2
sort_by_f(Frontier1, Frontier2) :-
    quicksort(Frontier1, Frontier2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quicksort/2
quicksort([], []).
quicksort([X|Xs], Ys) :-
    partition(Xs, X, Littles, Bigs),
    quicksort(Littles, Ls),
    quicksort(Bigs, Bs),
    append(Ls, [X|Bs], Ys).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partition/4
partition([], _Y, [], []).
partition([X|Xs], Y, [X|Ls], Bs) :-
    f(X, FX),
    f(Y, FY),
    FX =< FY,
    partition(Xs, Y, Ls, Bs).
partition([X|Xs], Y, Ls, [X|Bs]) :-
    f(X, FX),
    f(Y, FY),
    FX > FY,
    partition(Xs, Y, Ls, Bs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% f/2
f(node(Position, _Path, Path_Cost),F) :-
    heuristic(Position, H),
    F is Path_Cost + H.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% heuristic/2
heuristic([R1, C1], H) :-
    is_goal([R2, C2]),
    H is (abs(R2 - R1) + abs(C2 - C1)).



% Search Graph Predicates:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_goal([X, Y]) is true if [X, Y] corresponds to a position on the map with grass and a castle.
is_goal([R, C]) :-
    mapa([R, C], pasto, castillo).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% neigbours/4 defines all valid neighbours not in the Frontier or Visited lists.
% neighbours(+Node, +Frontier, +Visited, -Neighbours)
% Neighbours is a list of the valid neighbours of Node
neighbours(node(Position, Path, Path_Cost), Frontier, Visited, Neighbours) :-
    findall(
        node(Neighbour_Positions, [Position|Path], Path_Cost),
        (
            arc(Position, Neighbour_Positions),
            not(member(node(Neighbour_Positions, _, _), Visited)),
            not(member(node(Neighbour_Positions, _, _), Frontier))
        ),
        Neighbours
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add_paths/3
% add paths prepares the neighbours for addition to the frontier by adding the cost of the path to the current node to the Path_Cost component of theeir node/3 structure.
add_paths([], _Frontier_Element, []).
add_paths([node(Neighbour, _, _)|Rest_Of_Neighbours], node(Position, Path, Path_Cost), [node(Neighbour, [Position|Path], New_Path_Cost)|Rest_Of_Frontier]) :-
    cost(Neighbour, Cost),
    New_Path_Cost is Path_Cost + Cost,
    add_paths(Rest_Of_Neighbours, node(Position, Path, Path_Cost), Rest_Of_Frontier).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% arc([R1, C1], [R2, C2]) is true for all [R2, C2] corresponding to a position on
% the map adjacent to [R1, C2], that is either water with a healthy bridge or non-forest
% The procedure predicates are ordered in such a way that neighbours will be
% generated in the order North, East, South, West

% To the north
arc([R1, C1], [R2, C2]) :-
    R1 > 0,
    R2 is R1 - 1,
    C2 is C1,
    mapa([R2, C2], Terrain, Contents),
    (
     Terrain = agua, Contents = puente(sano) ;
     Terrain \= agua, Terrain \= bosque
    ).

% To the east
arc([R1, C1], [R2, C2]) :-
    max_column(C),
    C1 < C,
    R2 is R1,
    C2 is C1 + 1,
    mapa([R2, C2], Terrain, Contents),
    (
     Terrain = agua, Contents = puente(sano) ;
     Terrain \= agua, Terrain \= bosque
    ).

% To the south
arc([R1, C1], [R2, C2]) :-
    max_row(R),
    R1 < R,
    R2 is R1 + 1,
    C2 is C1,
    mapa([R2, C2],Terrain,Contents),
    (
     Terrain = agua, Contents = puente(sano) ;
     Terrain \= agua, Terrain \= bosque
    ).

% to the west
arc([R1, C1], [R2, C2]) :-
    C1 > 0,
    R2 is R1,
    C2 is C1 - 1,
    mapa([R2, C2],Terrain,Contents),
    (
     Terrain = agua, Contents = puente(sano) ;
     Terrain \= agua, Terrain \= bosque
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cost/2 defines the cost of crossing the node corresponding to Position
cost(Position, 1) :-
    mapa(Position, pasto, _Contents).
cost(Position, 2) :-
    mapa(Position, agua, _Contents).
cost(Position, 3) :-
    mapa(Position, mont, _Contents).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% frontier_positions(+Frontier, -Frontier_Positions) produces a list of
% Positions from a Frontier, stripping the Positions of the Path and Path_Cost
% components and the surrounding node/3 bookkeeping structure.
frontier_positions([], []).
frontier_positions([node(N, _P, _PC)|RF], [N|RFP]) :-
    frontier_positions(RF, RFP).
