%%%-------------------------------------------------------------------
%% @doc mnesia_test public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_test_app).

-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]).
-export([install/1]).
-export([add/2]).

%%====================================================================
%% Table Definitions
%%====================================================================

-record(mnesia_test_table1_v1,
  { field1
  , field2
  }).

%%====================================================================
%% API
%%====================================================================

start(normal, []) ->
  Tables = [mnesia_test_table1],
  mnesia:wait_for_tables(Tables, 5000),
  mnesia_test_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%--------------------------------------------------------------------
install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  rpc:multicall(Nodes, application, start, [mnesia]),

  Table1Opts =
    [ {attributes, record_info(fields, mnesia_test_table1)}
    , {disk_copies, Nodes}
    ],
  mnesia:create_table(mnesia_test_table1, Table1Opts),
  rpc:multicall(Nodes, application, stop, [mnesia]).

%%--------------------------------------------------------------------
add(Field1, Field2) ->
  F =
    fun() ->
      Entity = #mnesia_test_table1{ field1=Field1
                                  , field2=Field2
                                  },
      mnesia:write(Entity)
  end,
  mnesia:activity(transaction, F).
