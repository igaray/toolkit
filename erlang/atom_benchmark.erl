-module(atom_benchmark).

-export([dyn/0,
         static/0]).

-define(ITERS, 200000).
-define(BINS, [<<"aaa">>, <<"bbb">>, <<"ccc">>, <<"ddd">>, <<"eee">>]).

%% Measures performance of calling erlang:binary_to_atom/2 and
%% using predefined function heads to convert binaries to atoms.

%% Testing rationale:
%%  Each pass measures average time to convert a binary to an atom over 1m calls.
%%  Each test consists of 50 passes. Lowest and highest results are discarded
%%  and the average of the remaining values are reported. 

dyn() ->
    BMark = fun() -> {Time, ok} = timer:tc(fun() -> do_dyn(?ITERS) end),
                     Time / ?ITERS end,
    Results = [BMark() || _ <- lists:seq(1, 50)],
    R1 = Results -- [lists:min(Results)],
    R2 = R1 -- [lists:max(R1)],
    lists:sum(R2) / length(R2).

static() ->
    BMark = fun() -> {Time, ok} = timer:tc(fun() -> do_static(?ITERS) end),
                     Time / ?ITERS end,
    Results = [BMark() || _ <- lists:seq(1, 50)],
    R1 = Results -- [lists:min(Results)],
    R2 = R1 -- [lists:max(R1)],
    lists:sum(R2) / length(R2).

do_dyn(0) ->
    ok;
do_dyn(Iter) ->
    [erlang:binary_to_atom(B, unicode) || B <- ?BINS],
    do_dyn(Iter - 1).

do_static(0) ->
    ok;
do_static(Iter) ->
    [convert(B) || B <- ?BINS],
    do_static(Iter - 1).

convert(<<"aaa">>) -> aaa;
convert(<<"bbb">>) -> bbb;
convert(<<"ccc">>) -> ccc;
convert(<<"ddd">>) -> ddd;
convert(<<"eee">>) -> eee.
