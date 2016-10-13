binary_join(Bin, Separator) ->
  JoinedRev = binary_join_rec(Bin, [], Separator),
  Joined = lists:reverse(JoinedRev),
  iolist_to_binary(Joined).

binary_join_rec([H], Acc, _Separator) ->
  [H | Acc];
binary_join_rec([H | T], Acc, Separator) ->
  binary_join_rec(T, [Separator, H | Acc], Separator).