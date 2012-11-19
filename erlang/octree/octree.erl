-module(octree).

-export([
        new/7,
        lookup/4,
        insert/5
    ]).

-include("octree_common.hrl").
    
% Node structures:
% {leaf,  Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Data}.
% {inner, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, {Octant1, Octant2, Octant3, Octant4, Octant5, Octant6, Octant7, Octant8}}.

%--------------------------------------------------------------------------------------------------%
new(Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Data) when Xmin =< Xmax, Ymin =< Ymax, Zmin =< Zmax ->
    {leaf, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Data}.

%--------------------------------------------------------------------------------------------------%
lookup({Node, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Data_Children}, X, Y, Z) ->
    case within(X, Y, Z, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax) of
        true ->
            lookup1({Node, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Data_Children}, X, Y, Z);
        false ->
            {error, out_of_bounds, {X, Y, Z, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax}}
    end.
   
lookup1({leaf, _Xmin, _Ymin, _Zmin, _Xmax, _Ymax, _Zmax, Data}, _X, _Y, _Z) ->
    Data;
lookup1({inner, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Children}, X, Y, Z) ->
    Octant = in_octant(X, Y, Z, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax),
    lookup1(element(Octant, Children), X, Y, Z).

%--------------------------------------------------------------------------------------------------%
% First argument is the octree, following X, Y, Z arguments are the coordinates we are inserting 
% Data into.
% Xmin, YMin, Zmin are the coordinates of the lower-top-left point of the octree.
% Xmax, Ymax, Zmax are the coordinates of the upper-bottom-right point of the octree.
% If the node is an inner node, checked whether the children can be merged, if they cannot, then 
% recurse down the tree. 
% If the node is a leaf, then check whether the base case of maximum resolution has been reached, 
% i.e. the leaf node is a point and cannot be split. 
% If so, return a new leaf node with the new data. 
% If not, then check (X,Y,Z) to see into which octant it falls, and insert into that one. 
insert({Node, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Node_Data}, X, Y, Z, Data) ->
    case within(X, Y, Z, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax) of
        true ->
            insert1({Node, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Node_Data}, X, Y, Z, Data);
        false ->
            {error, out_of_bounds, [X, Y, Z, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax]}
    end.

insert1({inner, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Children}, X, Y, Z, Data) ->
    Octant       = in_octant(X, Y, Z, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax),
    New_Child    = insert1(element(Octant, Children), X, Y, Z, Data),
    New_Children = setelement(Octant, Children, New_Child),
    case merge_case(New_Children) of
        true ->
            % we can split the node further, but all other children besides the one in which we must 
            % insert are leaf nodes and have the same data as we are inserting, so just return a 
            % leaf node with the data
            new(Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Data);
        false ->
            {inner, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, New_Children}
    end;
insert1({leaf, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Node_Data}, X, Y, Z, Data) ->
    case base_case(Xmin, Ymin, Zmin, Xmax, Ymax, Zmax) of
        true ->
            % base case, can go no further down. return new octree with data changed
            new(Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, Data);
        false -> 
            % can split the node further, create children, set 7 of their data to be this node's 
            % data, and insert the data element in the new octree
            New_Children = 
                {
                new(Xmin, Ymin, Zmin, ((Xmax + Xmin) div 2), ((Ymax + Ymin) div 2), ((Zmax + Zmin) div 2), Node_Data),
                new(((Xmax + Xmin) div 2) + 1, Ymin, Zmin, Xmax, ((Ymax + Ymin) div 2), ((Zmax + Zmin) div 2), Node_Data),
                new(Xmin, ((Ymax + Ymin) div 2) + 1, Zmin, ((Xmax + Xmin) div 2), Ymax, ((Zmax + Zmin) div 2), Node_Data),
                new(((Xmax + Xmin) div 2) + 1, ((Ymax + Ymin) div 2) + 1, Zmin, Xmax, Ymax, ((Zmax + Zmin) div 2), Node_Data),
                new(Xmin, Ymin, ((Zmax + Zmin) div 2) + 1, ((Xmax + Xmin) div 2), ((Ymax + Ymin) div 2), Zmax, Node_Data),
                new(((Xmax + Xmin) div 2) + 1, Ymin, ((Zmax + Zmin) div 2) + 1, Xmax, ((Ymax + Ymin) div 2), Zmax, Node_Data),
                new(Xmin, ((Ymax + Ymin) div 2) + 1, ((Zmax + Zmin) div 2) + 1, ((Xmax + Xmin) div 2), Ymax, Zmax, Node_Data),
                new(((Xmax + Xmin) div 2) + 1, ((Ymax + Ymin) div 2) + 1, ((Zmax + Zmin) div 2) + 1, Xmax, Ymax, Zmax, Node_Data)
                },
            insert1({inner, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax, New_Children}, X, Y, Z, Data)
    end.
