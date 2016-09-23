%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%% Configuración de la Comarca
/*

       1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20
     1 #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
     2 #  -  -  -  -  -  -  #  h  -  -  -  -  -  -  -  -  -  t  #
     3 #  -  #  -  -  -  -  #  -  -  -  -  -  -  -  -  -  #  #  #
     4 #  -  #  #  #  #  -  -  #  ^  -  #  #  #  #  -  -  -  -  #
     5 #  -  #  -  -  #  -  -  -  -  -  -  -  -  #  t  #  #  -  #
     6 #  -  -  -  -  #  -  -  ^  -  -  -  -  -  -  #  #  -  -  #
     7 #  -  -  #  #  #  -  ^  ^  -  -  -  -  -  -  -  -  -  -  #
     8 #  -  -  #  -  -  -  ^  -  -  -  -  -  W  W  W  W  W  W  W
     9 #  -  -  -  -  -  ^  -  -  -  -  -  -  W  t  -  ^  #  t  #
    10 #  -  -  W  -  -  -  -  -  -  -  -  h  W  -  #  ^  #  -  #
    11 W  -  W  W  W  W  -  -  -  -  -  -  W  W  -  #  ^  -  -  #
    12 #  -  -  #  h  W  W  -  #  #  -  -  W  -  -  ^  #  -  #  #
    13 #  -  -  -  -  -  W  -  #  -  -  -  -  -  #  -  -  -  -  #
    14 #  -  -  -  -  ^  ^  ^  -  -  -  -  W  -  -  -  #  -  t  #
    15 #  #  #  #  #  #  W  #  #  #  #  #  W  #  #  #  #  #  #  #
*/


n_of_arrows(15).

n_of_columns(20).

% cell_land(Pos, Land)

% Land:
%
% plain
% water
% mountain
% forest


cell_land([1,1], forest).
cell_land([1,2], forest).
cell_land([1,3], water).
cell_land([1,4], forest).
cell_land([1,5], forest).
cell_land([1,6], forest).
cell_land([1,7], forest).
cell_land([1,8], forest).
cell_land([1,9], forest).
cell_land([1,10], forest).
cell_land([1,11], forest).
cell_land([1,12], forest).
cell_land([1,13], forest).
cell_land([1,14], forest).
cell_land([1,15], forest).
cell_land([1,16], forest).
cell_land([1,17], forest).
cell_land([1,18], forest).
cell_land([1,19], forest).
cell_land([1,20], forest).

cell_land([2,1], forest).
cell_land([2,2], plain).
cell_land([2,3], plain).
cell_land([2,4], plain).
cell_land([2,5], mountain).
cell_land([2,6], plain).
cell_land([2,7], plain).
cell_land([2,8], forest).
cell_land([2,9], plain).
cell_land([2,10], plain).
cell_land([2,11], plain).
cell_land([2,12], plain).
cell_land([2,13], plain).
cell_land([2,14], plain).
cell_land([2,15], plain).
cell_land([2,16], plain).
cell_land([2,17], plain).
cell_land([2,18], plain).
cell_land([2,19], plain).
cell_land([2,20], forest).



cell_land([3,1], forest).
cell_land([3,2], plain).
cell_land([3,3], water).
cell_land([3,4], plain).
cell_land([3,5], plain).
cell_land([3,6], plain).
cell_land([3,7], plain).
cell_land([3,8], forest).
cell_land([3,9], plain).
cell_land([3,10], plain).
cell_land([3,11], plain).
cell_land([3,12], plain).
cell_land([3,13], plain).
cell_land([3,14], plain).
cell_land([3,15], plain).
cell_land([3,16], plain).
cell_land([3,17], plain).
cell_land([3,18], forest).
cell_land([3,19], forest).
cell_land([3,20], forest).


cell_land([4,1], forest).
cell_land([4,2], plain).
cell_land([4,3], water).
cell_land([4,4], water).
cell_land([4,5], water).
cell_land([4,6], water).
cell_land([4,7], plain).
cell_land([4,8], plain).
cell_land([4,9], forest).
cell_land([4,10], mountain).
cell_land([4,11], plain).
cell_land([4,12], forest).
cell_land([4,13], forest).
cell_land([4,14], forest).
cell_land([4,15], forest).
cell_land([4,16], plain).
cell_land([4,17], plain).
cell_land([4,18], plain).
cell_land([4,19], plain).
cell_land([4,20], forest).


cell_land([5,1], forest).
cell_land([5,2], plain).
cell_land([5,3], forest).
cell_land([5,4], plain).
cell_land([5,5], plain).
cell_land([5,6], water).
cell_land([5,7], plain).
cell_land([5,8], plain).
cell_land([5,9], plain).
cell_land([5,10], plain).
cell_land([5,11], plain).
cell_land([5,12], plain).
cell_land([5,13], plain).
cell_land([5,14], plain).
cell_land([5,15], forest).
cell_land([5,16], plain).
cell_land([5,17], forest).
cell_land([5,18], forest).
cell_land([5,19], plain).
cell_land([5,20], forest).


cell_land([6,1], forest).
cell_land([6,2], plain).
cell_land([6,3], plain).
cell_land([6,4], plain).
cell_land([6,5], plain).
cell_land([6,6], water).
cell_land([6,7], plain).
cell_land([6,8], plain).
cell_land([6,9], mountain).
cell_land([6,10], plain).
cell_land([6,11], plain).
cell_land([6,12], plain).
cell_land([6,13], plain).
cell_land([6,14], plain).
cell_land([6,15], plain).
cell_land([6,16], forest).
cell_land([6,17], forest).
cell_land([6,18], plain).
cell_land([6,19], plain).
cell_land([6,20], forest).

cell_land([7,1], forest).
cell_land([7,2], plain).
cell_land([7,3], plain).
cell_land([7,4], water).
cell_land([7,5], water).
cell_land([7,6], water).
cell_land([7,7], plain).
cell_land([7,8], mountain).
cell_land([7,9], mountain).
cell_land([7,10], plain).
cell_land([7,11], plain).
cell_land([7,12], plain).
cell_land([7,13], plain).
cell_land([7,14], plain).
cell_land([7,15], plain).
cell_land([7,16], plain).
cell_land([7,17], plain).
cell_land([7,18], plain).
cell_land([7,19], plain).
cell_land([7,20], forest).

cell_land([8,1], forest).
cell_land([8,2], plain).
cell_land([8,3], plain).
cell_land([8,4], water).
cell_land([8,5], plain).
cell_land([8,6], plain).
cell_land([8,7], plain).
cell_land([8,8], mountain).
cell_land([8,9], plain).
cell_land([8,10], plain).
cell_land([8,11], plain).
cell_land([8,12], plain).
cell_land([8,13], plain).
cell_land([8,14], water).
cell_land([8,15], water).
cell_land([8,16], water).
cell_land([8,17], water).
cell_land([8,18], water).
cell_land([8,19], water).
cell_land([8,20], water).

cell_land([9,1], forest).
cell_land([9,2], plain).
cell_land([9,3], plain).
cell_land([9,4], plain).
cell_land([9,5], plain).
cell_land([9,6], plain).
cell_land([9,7], mountain).
cell_land([9,8], plain).
cell_land([9,9], plain).
cell_land([9,10], plain).
cell_land([9,11], plain).
cell_land([9,12], plain).
cell_land([9,13], plain).
cell_land([9,14], water).
cell_land([9,15], plain).
cell_land([9,16], plain).
cell_land([9,17], mountain).
cell_land([9,18], forest).
cell_land([9,19], plain).
cell_land([9,20], forest).


cell_land([10,1], forest).
cell_land([10,2], plain).
cell_land([10,3], plain).
cell_land([10,4], water).
cell_land([10,5], plain).
cell_land([10,6], plain).
cell_land([10,7], plain).
cell_land([10,8], plain).
cell_land([10,9], plain).
cell_land([10,10], plain).
cell_land([10,11], plain).
cell_land([10,12], plain).
cell_land([10,13], plain).
cell_land([10,14], water).
cell_land([10,15], plain).
cell_land([10,16], forest).
cell_land([10,17], mountain).
cell_land([10,18], forest).
cell_land([10,19], plain).
cell_land([10,20], forest).

cell_land([11,1], water).
cell_land([11,2], plain).
cell_land([11,3], water).
cell_land([11,4], water).
cell_land([11,5], water).
cell_land([11,6], water).
cell_land([11,7], plain).
cell_land([11,8], plain).
cell_land([11,9], plain).
cell_land([11,10], plain).
cell_land([11,11], plain).
cell_land([11,12], plain).
cell_land([11,13], water).
cell_land([11,14], water).
cell_land([11,15], plain).
cell_land([11,16], forest).
cell_land([11,17], mountain).
cell_land([11,18], plain).
cell_land([11,19], plain).
cell_land([11,20], forest).


cell_land([12,1], forest).
cell_land([12,2], plain).
cell_land([12,3], plain).
cell_land([12,4], forest).
cell_land([12,5], plain).
cell_land([12,6], water).
cell_land([12,7], water).
cell_land([12,8], plain).
cell_land([12,9], forest).
cell_land([12,10], forest).
cell_land([12,11], plain).
cell_land([12,12], plain).
cell_land([12,13], water).
cell_land([12,14], plain).
cell_land([12,15], plain).
cell_land([12,16], mountain).
cell_land([12,17], forest).
cell_land([12,18], plain).
cell_land([12,19], forest).
cell_land([12,20], forest).

cell_land([13,1], forest).
cell_land([13,2], plain).
cell_land([13,3], plain).
cell_land([13,4], plain).
cell_land([13,5], plain).
cell_land([13,6], plain).
cell_land([13,7], water).
cell_land([13,8], plain).
cell_land([13,9], forest).
cell_land([13,10], plain).
cell_land([13,11], plain).
cell_land([13,12], plain).
cell_land([13,13], plain).
cell_land([13,14], plain).
cell_land([13,15], forest).
cell_land([13,16], plain).
cell_land([13,17], plain).
cell_land([13,18], plain).
cell_land([13,19], plain).
cell_land([13,20], forest).

cell_land([14,1], forest).
cell_land([14,2], plain).
cell_land([14,3], plain).
cell_land([14,4], plain).
cell_land([14,5], plain).
cell_land([14,6], mountain).
cell_land([14,7], mountain).
cell_land([14,8], mountain).
cell_land([14,9], plain).
cell_land([14,10], plain).
cell_land([14,11], plain).
cell_land([14,12], plain).
cell_land([14,13], water).
cell_land([14,14], plain).
cell_land([14,15], plain).
cell_land([14,16], plain).
cell_land([14,17], forest).
cell_land([14,18], plain).
cell_land([14,19], plain).
cell_land([14,20], forest).

cell_land([15,1], forest).
cell_land([15,2], forest).
cell_land([15,3], forest).
cell_land([15,4], forest).
cell_land([15,5], forest).
cell_land([15,6], forest).
cell_land([15,7], water).
cell_land([15,8], forest).
cell_land([15,9], forest).
cell_land([15,10], forest).
cell_land([15,11], forest).
cell_land([15,12], forest).
cell_land([15,13], water).
cell_land([15,14], forest).
cell_land([15,15], forest).
cell_land([15,16], forest).
cell_land([15,17], forest).
cell_land([15,18], forest).
cell_land([15,19], forest).
cell_land([15,20], forest).



% object_at(Object, Pos)

% Object/Content
%
% [ObjType, ObjName, ObjDescription]

:- dynamic object_at/2.


object_at([treasure, t1, [[val, 100]]], [5,7]).

object_at([treasure, t2, [[val, 100]]], [5,4]).
object_at([treasure, t3, [[val, 100]]], [5,4]).
%object_at([treasure, t4, [[val, 100]]], [,2]).
%object_at([treasure, t5, [[val, 100]]], [2,2]).
%object_at([treasure, t13, [[val, 100]]], [2,2]).
%object_at([treasure, t14, [[val, 100]]], [2,2]).
%object_at([treasure, t15, [[val, 100]]], [2,2]).
%object_at([treasure, t16, [[val, 100]]], [2,2]).
%object_at([treasure, tj, [[val, 100]]], [2,2]).
%object_at([treasure, ti, [[val, 100]]], [2,2]).
%object_at([treasure, th, [[val, 100]]], [2,2]).
%object_at([treasure, tg, [[val, 100]]], [2,2]).
%object_at([treasure, tc, [[val, 100]]], [2,2]).
%object_at([treasure, td, [[val, 100]]], [2,2]).
%object_at([treasure, te, [[val, 100]]], [2,2]).
%object_at([treasure, tf, [[val, 100]]], [2,2]).


object_at([treasure, t4, [[val, 100]]], [7,9]).

%object_at([treasure, t7, [[val, 100]]], [5,7]).
%object_at([treasure, t8, [[val, 100]]], [5,7]).

object_at([treasure, t5, [[val, 100]]], [2,9]).

object_at([treasure, t6, [[val, 100]]], [9,3]).

object_at([treasure, t7, [[val, 100]]], [2,19]).

object_at([treasure, t8, [[val, 100]]], [5,16]).

object_at([treasure, t9, [[val, 100]]], [9,15]).

object_at([treasure, ta, [[val, 100]]], [9,19]).

object_at([treasure, tb, [[val, 100]]], [9,19]).

object_at([treasure, tc, [[val, 100]]], [14,19]).


object_at([treasure, td, [[val, 100]]], [13,10]).


% building(BType, BName, BPos, BDescr).

building(hostel, h1, [12,5], []).

building(hostel, h2, [2,9], []).

building(hostel, h3, [10,13], []).
