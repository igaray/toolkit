-module(glyph).

-export([pairs/2]).

manhattan_distance({X1, Y1}, {X2, Y2}) ->
  abs(X2 - X1) + abs(Y2 - Y1).

pairs(N, M) ->
  C =
    [ {X, Y}
    || X <- lists:seq(0, N - 1), Y <- lists:seq(0, M - 1)
    ],
  AllPairs = 
    [ {A, B}
    || {X1, Y1} = A <- C,
       {X2, Y2} = B <- C,
       A =/= B,
       (X1 =/= X2 andalso Y1 =/= Y2) orelse 1 =:= manhattan_distance(A, B)
    ],
  OrderedPairs =
    [ list_to_tuple(lists:sort([A, B]))
    || {A,B} <- AllPairs
    ],
  ordsets:from_list(lists:sort(OrderedPairs)).
