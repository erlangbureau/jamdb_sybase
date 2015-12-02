-module(prepared_statement_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

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
        {group, basic_operations},
        {group, signed_integer_datatypes},
        {group, unsigned_integer_datatypes},
        {group, numeric_datatypes},
        {group, float_datatypes},
        {group, datetime_datatypes},
        {group, characters_datatypes},
        {group, binary_datatypes},
        {group, long_characters_datatypes},
        {group, long_binary_datatypes},
        {group, clob_datatypes}
        %{group, procedure_operations}
    ].

groups() ->
    [
        {basic_operations, [sequence], [
            select_without_params,
            select_with_params
            %select_with_order_by
        ]},
        {signed_integer_datatypes, [sequence], [
            signed_bigint,
            signed_nullable_bigint,
            signed_int,
            signed_nullable_int,
            signed_smallint,
            signed_nullable_smallint
        ]},
        {unsigned_integer_datatypes, [sequence], [
            unsigned_bigint,
            unsigned_nullable_bigint,
            unsigned_int,
            unsigned_nullable_int,
            unsigned_smallint,
            unsigned_nullable_smallint,
            unsigned_tinyint,
            unsigned_nullable_tinyint,
            bit
        ]},
        {numeric_datatypes, [sequence], [
            integer_numeric,
            integer_nullable_numeric,
            fractional_numeric,
            fractional_nullable_numeric
        ]},
        {float_datatypes, [sequence], [
            float,
            nullable_float,
            real,
            nullable_real
        ]},
        {money_datatypes, [sequence], [
            money,
            nulable_money,
            smallmoney,
            nulable_smallmoney
        ]},
        {datetime_datatypes, [sequence], [
            datetime,
            nulable_datetime,
            smalldatetime,
            nulable_smalldatetime,
            date,
            nulable_date,
            time,
            nulable_time
        ]},
        {characters_datatypes, [sequence], [
            char,
            nullable_char,
            nchar,
            nulable_nchar,
            varchar,
            nulable_varchar,
            nvarchar,
            nulable_nvarchar
        ]},
        {binary_datatypes, [sequence], [
            binary,
            nulable_binary,
            varbinary,
            nulable_varbinary
        ]},
        {long_characters_datatypes, [sequence], [
            widetable_char,
            widetable_nullable_char,
            widetable_nchar,
            widetable_nullable_nchar,
            widetable_varchar,
            widetable_nulable_varchar,
            widetable_nvarchar,
            widetable_nulable_nvarchar
        ]},
        {long_binary_datatypes, [sequence], [
            widetable_binary,
            widetable_nulable_binary,
            widetable_varbinary,
            widetable_nulable_varbinary
        ]},
        {clob_datatypes, [sequence], [
            text,
            nulable_text,
            unitext,
            nulable_unitext
        ]},
        {procedure_operations, [sequence], [
            return_ok,
            return_error,
            no_return,
            one_result_set,
            multi_result_set,
            with_input_params,
            with_output_params,
            with_input_and_output_params
        ]}
    ].

init_per_suite(Config) ->
    {ok, ConnRef} = jamdb_sybase:start(?ConnOpts),
    [{conn_ref, ConnRef}|Config].

end_per_suite(Config) ->
    ConnRef = ?config(conn_ref, Config),
    ok = jamdb_sybase:stop(ConnRef),
    Config.

init_per_group(Name, Config) ->
    InitType = case Name of
        basic_operations            -> basic;
        signed_integer_datatypes    -> datatypes;
        unsigned_integer_datatypes  -> datatypes;
        numeric_datatypes           -> datatypes;
        float_datatypes             -> datatypes;
        datetime_datatypes          -> datatypes;
        characters_datatypes        -> datatypes;
        long_characters_datatypes   -> datatypes;
        binary_datatypes            -> datatypes;
        long_binary_datatypes       -> datatypes;
        clob_datatypes              -> datatypes;
        procedure_operations        -> procedure
    end,
    [{init_type, InitType}|Config].

end_per_group(_Name, Config) ->
    lists:keydelete(init_type, 1, Config).

init_per_testcase(Case, Config) ->
    ConnRef = ?config(conn_ref, Config),
    case ?config(init_type, Config) of
        datatypes ->
            Query = lists:concat(["create table ", Case, "( ", table_desc(Case), " )"]),
            {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query),
            ok = jamdb_sybase:prepare(ConnRef, insert_p, lists:concat(["insert into ", Case, " VALUES(?)"])),
            ok = jamdb_sybase:prepare(ConnRef, select_p, lists:concat(["select * from ", Case])),
            ok = jamdb_sybase:prepare(ConnRef, delete_p, lists:concat(["delete from ", Case]));
        procedure ->
            Query = lists:concat(["create procedure ", Case, " ", procedure_desc(Case)]),
            {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query);
        _ ->
            nothing
    end,
    Config.

end_per_testcase(Case, Config) ->
    ConnRef = ?config(conn_ref, Config),
    case ?config(init_type, Config) of
        datatypes ->
            Query = lists:concat(["drop table ", Case]),
            {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query),
            ok = jamdb_sybase:unprepare(ConnRef, insert_p),
            ok = jamdb_sybase:unprepare(ConnRef, select_p),
            ok = jamdb_sybase:unprepare(ConnRef, delete_p);
        procedure ->
            Query = lists:concat(["drop procedure ", Case]),
            {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query);
        _ ->
            nothing
    end,
    ok.

%% test cases
select_without_params(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "select * from sysusers where uid=1",
    ok = jamdb_sybase:prepare(ConnRef, test_statement, Query),
    Result = [
        {result_set, 
            [<<"suid">>,<<"uid">>,<<"gid">>,<<"name">>, <<"environ">>], 
            [],
            [[4,1,0,<<"dbo">>,null]]},
        {procedure_result,undefined,[]}
    ],
    {ok, Result} = jamdb_sybase:execute(ConnRef, test_statement, []).

select_with_params(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "select * from sysusers where uid=?",
    ok = jamdb_sybase:prepare(ConnRef, test_statement2, Query),
    Result = [
        {result_set, 
            [<<"suid">>,<<"uid">>,<<"gid">>,<<"name">>, <<"environ">>], 
            [],
            [[4,1,0,<<"dbo">>,null]]},
        {procedure_result,undefined,[]}
    ],
    {ok, Result} = jamdb_sybase:execute(ConnRef, test_statement2, [1]).

signed_bigint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {-9223372036854775808, -9223372036854775808}
    ],
    run_testcases(ConnRef, TestCases).

signed_nullable_bigint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {-9223372036854775808, -9223372036854775808}
    ],
    run_testcases(ConnRef, TestCases).

signed_int(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {-2147483648, -2147483648}
    ],
    run_testcases(ConnRef, TestCases).

signed_nullable_int(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {-2147483648, -2147483648}
    ],
    run_testcases(ConnRef, TestCases).


unsigned_bigint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {18446744073709551615, 18446744073709551615}
    ],
    run_testcases(ConnRef, TestCases).

signed_smallint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {-32768, -32768}
    ],
    run_testcases(ConnRef, TestCases).

signed_nullable_smallint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {-32768, -32768}
    ],
    run_testcases(ConnRef, TestCases).

unsigned_nullable_bigint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {18446744073709551615, 18446744073709551615}
    ],
    run_testcases(ConnRef, TestCases).

unsigned_int(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {4294967295, 4294967295}
    ],
    run_testcases(ConnRef, TestCases).

unsigned_nullable_int(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {4294967295, 4294967295}
    ],
    run_testcases(ConnRef, TestCases).

unsigned_smallint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {65535, 65535}
    ],
    run_testcases(ConnRef, TestCases).

unsigned_nullable_smallint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {65535, 65535}
    ],
    run_testcases(ConnRef, TestCases).

unsigned_tinyint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {255, 255}
    ],
    run_testcases(ConnRef, TestCases).

unsigned_nullable_tinyint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {255, 255}
    ],
    run_testcases(ConnRef, TestCases).

bit(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {0, 0},
        %% case 2
        {1, 1}
    ],
    run_testcases(ConnRef, TestCases).

integer_numeric(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        { 0,  0},
        %% case 2
        { 1,  1},
        %% case 3
        {-1, -1},
        %% case 4
        { 999999999999999999999999999999999999,  999999999999999999999999999999999999},
        %% case 5
        {-999999999999999999999999999999999999, -999999999999999999999999999999999999}
    ],
    run_testcases(ConnRef, TestCases).

integer_nullable_numeric(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        { 0,  0},
        %% case 3
        { 1,  1},
        %% case 4
        {-1, -1},
        %% case 5
        { 999999999999999999999999999999999999,  999999999999999999999999999999999999},
        %% case 6
        {-999999999999999999999999999999999999, -999999999999999999999999999999999999}
    ],
    run_testcases(ConnRef, TestCases).

fractional_numeric(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {{0, 0, -2}, {0, 0, -2}},
        %% case 2
        {{0, 100, -2}, {0, 100, -2}},
        %% case 3
        {{1, 100, -2}, {1, 100, -2}},
        %% case 4
        {{0, 99999999999999999999999999999999999999, -2}, {0, 99999999999999999999999999999999999999, -2}},
        %% case 5
        {{1, 99999999999999999999999999999999999999, -2}, {1, 99999999999999999999999999999999999999, -2}}
    ],
    run_testcases(ConnRef, TestCases).

fractional_nullable_numeric(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {{0, 0, -2}, {0, 0, -2}},
        %% case 3
        {{0, 100, -2}, {0, 100, -2}},
        %% case 4
        {{1, 100, -2}, {1, 100, -2}},
        %% case 5
        {{0, 99999999999999999999999999999999999999, -2}, {0, 99999999999999999999999999999999999999, -2}},
        %% case 6
        {{1, 99999999999999999999999999999999999999, -2}, {1, 99999999999999999999999999999999999999, -2}}
    ],
    run_testcases(ConnRef, TestCases).

float(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {3.14, 3.14},
        %% case 2
        {-3.14, -3.14}
    ],
    run_testcases(ConnRef, TestCases).

nullable_float(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {3.14, 3.14},
        %% case 3
        {-3.14, -3.14}
    ],
    run_testcases(ConnRef, TestCases).

real(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {3.140000104904175, 3.140000104904175},
        %% case 2
        {-3.140000104904175, -3.140000104904175}
    ],
    run_testcases(ConnRef, TestCases).

nullable_real(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {3.140000104904175, 3.140000104904175},
        %% case 3
        {-3.140000104904175, -3.140000104904175}
    ],
    run_testcases(ConnRef, TestCases).

datetime(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {{{2013,8,1},{16,0,0}}, {{2013,8,1},{16,0,0}}}
    ],
    run_testcases(ConnRef, TestCases).

nulable_datetime(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {{{2013,8,1},{16,0,0}}, {{2013,8,1},{16,0,0}}}
    ],
    run_testcases(ConnRef, TestCases).

smalldatetime(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {{{2013,8,1},{16,0,0}}, {{2013,8,1},{16,0,0}}}
    ],
    run_testcases(ConnRef, TestCases).

nulable_smalldatetime(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {{{2013,8,1},{16,0,0}}, {{2013,8,1},{16,0,0}}}
    ],
    run_testcases(ConnRef, TestCases).

date(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {{2013,8,1}, {2013,8,1} }
    ],
    run_testcases(ConnRef, TestCases).

nulable_date(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {{2013,8,1}, {2013,8,1} },
        %% case 3
        {{2013,8,1}, {2013,8,1} }
    ],
    run_testcases(ConnRef, TestCases).

time(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {{16,0,0}, {16,0,0} },
        %% case 2
        {{16,0,0}, {16,0,0} }
    ],
    run_testcases(ConnRef, TestCases).

nulable_time(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {{16,0,0}, {16,0,0} },
        %% case 3
        {{16,0,0}, {16,0,0} }
    ],
    run_testcases(ConnRef, TestCases).

char(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<"          ">>},
        %% case 2
        {<<"a">>, <<"a         ">>}
    ],
    run_testcases(ConnRef, TestCases).

nullable_char(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<"          ">>},
        %% case 3
        {<<"a">>, <<"a         ">>}
    ],
    run_testcases(ConnRef, TestCases).

nchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<"                              ">>},
        %% case 2
        {<<"a">>, <<"a                             ">>}
    ],
    run_testcases(ConnRef, TestCases).

nulable_nchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, TestCases).

varchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, TestCases).

nulable_varchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, TestCases).

nvarchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, TestCases).

nulable_nvarchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, TestCases).

binary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ",0,0,0,0,0,0,0,0,0>>},
        %% case 2
        {<<"a">>, <<"a",0,0,0,0,0,0,0,0,0>>}
    ],
    run_testcases(ConnRef, TestCases).


nulable_binary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, TestCases).

varbinary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, TestCases).

nulable_varbinary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, TestCases).

widetable_char(Config) ->
    ConnRef = ?config(conn_ref, Config),
    RValue1 = << <<" ">> || _ <- lists:seq(1,1000)>>,
    Value2 = << <<"abcd_">> || _ <- lists:seq(1,200)>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, RValue1},
        %% case 2
        {Value2, Value2}
    ],
    run_testcases(ConnRef, TestCases).

widetable_nullable_char(Config) ->
    ConnRef = ?config(conn_ref, Config),
    RValue1 = << <<" ">> || _ <- lists:seq(1,1000)>>,
    Value2 = << <<"abcd_">> || _ <- lists:seq(1,200)>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, RValue1},
        %% case 3
        {Value2, Value2}
    ],
    run_testcases(ConnRef, TestCases).

widetable_nchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    RValue2 = << <<" ">> || _ <- lists:seq(1,3000)>>,
    Value3 = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary, RValue2:2000/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, RValue2},
        %% case 2
        {Value3, Value3}
    ],
    run_testcases(ConnRef, TestCases).

widetable_nullable_nchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Value4 = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"abcd">>, <<"abcd">>},
        %% case 4
        {Value4, Value4}
    ],
    run_testcases(ConnRef, TestCases).

widetable_varchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"abcd">>, <<"abcd">>},
        %% case 3
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

widetable_nulable_varchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"abcd">>, <<"abcd">>},
        %% case 4
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

widetable_nvarchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"abcd">>, <<"abcd">>},
        %% case 3
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

widetable_nulable_nvarchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"abcd">>, <<"abcd">>},
        %% case 4
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

widetable_binary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    RValue1 = <<" ", << <<0>> || _ <- lists:seq(1,999)>>/binary>>,
    RValue2 = <<"abcd_", << <<0>> || _ <- lists:seq(1,995)>>/binary>>,
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, RValue1},
        %% case 2
        {<<"abcd_">>, RValue2},
        %% case 3
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

widetable_nulable_binary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"abcd">>, <<"abcd">>},
        %% case 4
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

widetable_varbinary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"abcd">>, <<"abcd">>},
        %% case 3
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

widetable_nulable_varbinary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"abcd">>, <<"abcd">>},
        %% case 4
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

text(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"abcd">>, <<"abcd">>},
        %% case 3
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).


nulable_text(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"abcd">>, <<"abcd">>},
        %% case 4
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

unitext(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"abcd">>, <<"abcd">>},
        %% case 3
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).

nulable_unitext(Config) ->
    ConnRef = ?config(conn_ref, Config),
    LongValue = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"abcd">>, <<"abcd">>},
        %% case 4
        {LongValue, LongValue}
    ],
    run_testcases(ConnRef, TestCases).


%money
%nulable_money
%smallmoney
%nulable_smallmoney



%% internal
table_desc(unsigned_bigint) ->
    "U_BIG unsigned bigint not null";
table_desc(signed_bigint) ->
    "S_BIG bigint not null";
table_desc(unsigned_nullable_bigint) ->
    "U_N_BIG unsigned bigint null";
table_desc(signed_nullable_bigint) ->
    "S_N_BIG bigint null";
table_desc(unsigned_int) ->
    "U_INT unsigned int not null";
table_desc(signed_int) ->
    "S_INT int not null";
table_desc(unsigned_nullable_int) ->
    "U_N_INT unsigned int null";
table_desc(signed_nullable_int) ->
    "S_N_INT int null";
table_desc(unsigned_smallint) ->
    "U_SMALL unsigned smallint not null";
table_desc(signed_smallint) ->
    "S_SMALL smallint not null";
table_desc(unsigned_nullable_smallint) ->
    "U_N_SMALL unsigned smallint null";
table_desc(signed_nullable_smallint) ->
    "S_N_SMALL smallint null";
table_desc(unsigned_tinyint) ->
    "U_TINY tinyint not null";
table_desc(unsigned_nullable_tinyint) ->
    "U_N_TINY tinyint null";
table_desc(bit) ->
    "BIT bit not null";
table_desc(float) ->
    "FLOAT float not null";
table_desc(nullable_float) ->
    "N_FLOAT float null";
table_desc(real) ->
    "REAL real not null";
table_desc(nullable_real) ->
    "N_REAL real null";
table_desc(integer_numeric) ->
    "I_NUMERIC numeric(38,0)";
table_desc(integer_nullable_numeric) ->
    "I_N_NUMERIC numeric(38,0) null";
table_desc(fractional_numeric) ->
    "F_NUMERIC numeric(38,2)";
table_desc(fractional_nullable_numeric) ->
    "F_N_NUMERIC numeric(38,2) null";
table_desc(char) ->
    "CHAR char(10) not null";
table_desc(nullable_char) ->
    "N_CHAR char(10) null";
table_desc(nchar) ->
    "NCHAR nchar(10) not null";
table_desc(nulable_nchar) ->
    "N_NCHAR nchar(10) null";
table_desc(varchar) ->
    "VARCHAR varchar(10) not null";
table_desc(nulable_varchar) ->
    "N_VARCHAR varchar(10) null";
table_desc(nvarchar) ->
    "NVARCHAR nvarchar(10) not null";
table_desc(nulable_nvarchar) ->
    "N_NVARCHAR nvarchar(10) null";
table_desc(binary) ->
    "BINARY binary(10) not null";
table_desc(nulable_binary) ->
    "N_BINARY binary(10) null";
table_desc(varbinary) ->
    "VARBINARY varbinary(10) not null";
table_desc(nulable_varbinary) ->
    "N_VARBINARY varbinary(10) null";
table_desc(widetable_char) ->
    "W_CHAR char(1000) not null";
table_desc(widetable_nullable_char) ->
    "W_N_CHAR char(1000) null";
table_desc(widetable_nchar) ->
    "W_NCHAR nchar(1000) not null";
table_desc(widetable_nullable_nchar) ->
    "W_N_NCHAR nchar(1000) null";
table_desc(widetable_varchar) ->
    "W_VARCHAR varchar(1000) not null";
table_desc(widetable_nulable_varchar) ->
    "W_N_VARCHAR varchar(1000) null";
table_desc(widetable_nvarchar) ->
    "W_NVARCHAR nvarchar(1000) not null";
table_desc(widetable_nulable_nvarchar) ->
    "W_N_NVARCHAR nvarchar(1000) null";
table_desc(widetable_binary) ->
    "W_BINARY binary(1000) not null";
table_desc(widetable_nulable_binary) ->
    "W_N_BINARY binary(1000) null";
table_desc(widetable_varbinary) ->
    "W_VARBINARY varbinary(1000) not null";
table_desc(widetable_nulable_varbinary) ->
    "W_N_VARBINARY varbinary(1000) null";
table_desc(text) ->
    "TEXT text not null";
table_desc(nulable_text) ->
    "N_TEXT text null";
table_desc(unitext) ->
    "UNITEXT unitext not null";
table_desc(nulable_unitext) ->
    "N_UNITEXT unitext null";
table_desc(datetime) ->
    "DATETIME datetime not null";
table_desc(nulable_datetime) ->
    "N_DATETIME datetime null";
table_desc(smalldatetime) ->
    "SMALLDATETIME smalldatetime not null";
table_desc(nulable_smalldatetime) ->
    "N_SMALLDATETIME smalldatetime null";
table_desc(date) ->
    "DATE date not null";
table_desc(nulable_date) ->
    "N_DATE date null";
table_desc(time) ->
    "TIME time not null";
table_desc(nulable_time) ->
    "N_TIME time null";
table_desc(money) ->
    "MONEY money not null";
table_desc(nulable_money) ->
    "MONEY money null";
table_desc(smallmoney) ->
    "SMALLMONEY smallmoney not null";
table_desc(nulable_smallmoney) ->
    "N_SMALLMONEY smallmoney null".

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

run_testcases(ConnRef, Cases) ->
    [begin
        {ok, RValue} = run_testcase(ConnRef, SValue)
    end || {SValue, RValue} <- Cases].

run_testcase(ConRef, SrcValue) ->
%    {ok, [{affected_rows,1}]}                       = insert(ConRef, SrcValue), %% TODO
%    {ok, [{result_set,[_Key],[],[[ResultValue]]}]}  = select(ConRef),           %% TODO
%    {ok, [{affected_rows,1}]}                       = delete(ConRef),           %% TODO
    {ok, [{procedure_result,undefined,[]}]}                                         = insert(ConRef, SrcValue),
    {ok, [{result_set,[_Key],[],[[ResultValue]]}, {procedure_result,undefined,[]}]} = select(ConRef),
    {ok,[{procedure_result,undefined,[]}]}                                          = delete(ConRef),
    io:format("SourceValue:~p~n", [SrcValue]),
    io:format("ResultValue:~p~n", [ResultValue]),
    {ok, ResultValue}.

insert(ConnRef, Value) ->
    jamdb_sybase:execute(ConnRef, insert_p, [Value]).

select(ConnRef) ->
    jamdb_sybase:execute(ConnRef, select_p, []).

delete(ConnRef) ->
    jamdb_sybase:execute(ConnRef, delete_p, []).
