-module(jamdb_sybase_procedure_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test callbacks
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% test cases
-export([return_ok/1]).
-export([return_error/1]).
-export([no_return/1]).
-export([one_result_set/1]).
-export([multi_result_set/1]).
-export([with_input_params/1]).
-export([with_output_params/1]).
-export([with_input_and_output_params/1]).

-define(ConnOpts, [
    {host, "jamdb-sybase-dev.erlangbureau.dp.ua"},
    {port, 5000},
    {user, "jamdbtest"},
    {password, "jamdbtest"},
    {database, "jamdbtest"}
]).

%% Common Test callbacks
all() ->
    [
        return_ok,
        return_error,
        no_return,
        one_result_set,
        multi_result_set,
        with_input_params,
        with_output_params,
        with_input_and_output_params
    ].

init_per_suite(Config) ->
    {ok, ConnRef} = jamdb_sybase:start(?ConnOpts),
    [{conn_ref, ConnRef}|Config].

end_per_suite(Config) ->
    ConnRef = ?config(conn_ref, Config),
    ok = jamdb_sybase:stop(ConnRef),
    Config.

init_per_testcase(Case, Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = lists:concat(["create procedure ", Case, " ", procedure_desc(Case)]),
    {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query),
    Config.

end_per_testcase(Case, Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = lists:concat(["drop procedure ", Case]),
    {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query),
    ok.

%% test cases
return_ok(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "exec return_ok",
    {ok, [
        {procedure_result, 0, []}
    ]} = jamdb_sybase:sql_query(ConnRef, Query).

return_error(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "exec return_error",
    {ok, [
        {procedure_result, 22, []}
    ]} = jamdb_sybase:sql_query(ConnRef, Query).

no_return(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "exec no_return",
    {ok, [
        {procedure_result, 0, []}
    ]} = jamdb_sybase:sql_query(ConnRef, Query).

one_result_set(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "exec one_result_set",
    {ok, [
        {result_set, [<<"one">>], [], [ [1] ]},
        {procedure_result, 0, []}
    ]} = jamdb_sybase:sql_query(ConnRef, Query).

multi_result_set(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "exec multi_result_set",
    {ok,[
        {result_set, [<<"one">>], [], [ [1] ]},
        {result_set, [<<"two">>], [], [ [2] ]},
        {result_set, [<<"b">>],   [], [ [<<"b">>] ]},
        {procedure_result, 0, []}
    ]} = jamdb_sybase:sql_query(ConnRef, Query).

with_input_params(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "declare @i1 int declare @i2 varchar(5) "
        "select @i1 = -3, @i2 = 'abc_abc' "
        "exec with_input_params @i1, @i2",
    {ok,[
        {result_set, [<<"i1">>,<<"i2">>], [], [[-3, <<"abc_a">>]]},
        {procedure_result, 0, []}
    ]} = jamdb_sybase:sql_query(ConnRef, Query).

with_output_params(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "declare @o1 int "
        "declare @o2 varchar(5) "
        "select @o1 = 2, @o2 = 'def' "
        "exec with_output_params @o1 out, @o2 out",
    {ok,[
        {procedure_result, 0, [100, <<"text">>]}
    ]} = jamdb_sybase:sql_query(ConnRef, Query).

with_input_and_output_params(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "declare @i1 int declare @o1 int "
        "declare @i2 varchar(5) declare @o2 varchar(5) "
        "select @i1 = 1, @o1 = 2, @i2 = 'abc', @o2 = 'def' "
        "exec with_input_and_output_params @i1, @o1 out, @i2, @o2 out",
    {ok,[
        {procedure_result, 0, [1, <<"abc">>]}
    ]} = jamdb_sybase:sql_query(ConnRef, Query).

procedure_desc(return_ok) ->
    "as "
    "begin "
        "return 0 "
    "end";
procedure_desc(return_error) ->
    "as "
    "begin "
        "return 22 "
    "end";
procedure_desc(no_return) ->
    "as "
    "begin "
        "set rowcount 5 "
    "end";
procedure_desc(one_result_set) ->
    "as "
    "begin "
        "select 1 as one "
    "end";
procedure_desc(multi_result_set) ->
    "as "
    "begin "
        "select 1 as one "
        "select 2 as two "
        "select 'b' as b "
    "end";
procedure_desc(with_input_params) ->
    "( "
        "@i1 int, "
        "@i2 varchar(5) "
    ") as "
    "begin "
        "select @i1 as i1, @i2 as i2 "
    "end";
procedure_desc(with_output_params) ->
    "( "
        "@o1 int output, "
        "@o2 varchar(5) output"
    ") as "
    "begin "
        "select @o1 = 100, @o2 = 'text' "
    "end";
procedure_desc(with_input_and_output_params) ->
    "( "
        "@i1 int, "
        "@o1 int output, "
        "@i2 varchar(5), "
        "@o2 varchar(5) output "
    ") as "
    "begin "
        "select @o1 = @i1, @o2 = @i2 "
    "end".


%run_testcase(ConRef, Table, Key, SrcValue) ->
%    {ok, [{affected_rows,1}]}                       = insert(ConRef, Table, Key, SrcValue),
%    {ok, [{result_set,[Key],[],[[ResultValue]]}]}   = select(ConRef, Table),
%    {ok, [{affected_rows,1}]}                       = delete(ConRef, Table),
%    {ok, ResultValue}.

%execute(ConnRef, Tab, Key, Value) ->
%    Table = atom_to_binary(Tab, utf8),
%    Query1 = [<<"insert into ", Table/binary, "(", Key/binary, ") "
%                                "VALUES( ">>, arg, <<" )">>],
%    Query2 = format_query(Query1, [Value]),
%    jamdb_sybase:sql_query(ConnRef, Query2).
