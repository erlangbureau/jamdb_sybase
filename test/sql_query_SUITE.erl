-module(sql_query_SUITE).

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
        {group, clob_datatypes},
        {group, procedure_operations}
    ].

groups() ->
    [
        {basic_operations, [sequence], [
            select,
            select_with_order_by,
            multi_select
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
        binary_datatypes            -> datatypes;
        long_characters_datatypes   -> datatypes;
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
            {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query);
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
            {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query);
        procedure ->
            Query = lists:concat(["drop procedure ", Case]),
            {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query);
        _ ->
            nothing
    end,
    ok.

%% test cases
select(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "select 1 one, 2 two, 3 three",
    Result = [{result_set, [<<"one">>, <<"two">>, <<"three">>], [], [ [1,2,3] ]}],
    {ok, Result} = jamdb_sybase:sql_query(ConnRef, Query).

select_with_order_by(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "select 1 one, 2 two, 3 three order by one",
    Result = [{result_set, [<<"one">>, <<"two">>, <<"three">>], [{orderby,[1]}], [ [1,2,3] ]}],
    {ok, Result} = jamdb_sybase:sql_query(ConnRef, Query).

multi_select(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = "select 1 one, 2 two, 3 three ",
    Result = {result_set, [<<"one">>, <<"two">>, <<"three">>], [], [ [1,2,3] ]},
    {ok, [Result, Result]} = jamdb_sybase:sql_query(ConnRef, Query ++ Query).

signed_bigint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = signed_bigint,
    Key = <<"S_BIG">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {-9223372036854775808, -9223372036854775808}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

signed_nullable_bigint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = signed_nullable_bigint,
    Key = <<"S_N_BIG">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {-9223372036854775808, -9223372036854775808}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

signed_int(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = signed_int,
    Key = <<"S_INT">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {-2147483648, -2147483648}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

signed_nullable_int(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = signed_nullable_int,
    Key = <<"S_N_INT">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {-2147483648, -2147483648}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

signed_nullable_smallint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = signed_nullable_smallint,
    Key = <<"S_N_SMALL">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {-32768, -32768}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

unsigned_bigint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = unsigned_bigint,
    Key = <<"U_BIG">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {18446744073709551615, 18446744073709551615}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

unsigned_nullable_bigint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = unsigned_nullable_bigint,
    Key = <<"U_N_BIG">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {18446744073709551615, 18446744073709551615}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

unsigned_int(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = unsigned_int,
    Key = <<"U_INT">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {4294967295, 4294967295}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

unsigned_nullable_int(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = unsigned_nullable_int,
    Key = <<"U_N_INT">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {4294967295, 4294967295}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

unsigned_smallint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = unsigned_smallint,
    Key = <<"U_SMALL">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {65535, 65535}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

signed_smallint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = signed_smallint,
    Key = <<"S_SMALL">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {-32768, -32768}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

unsigned_nullable_smallint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = unsigned_nullable_smallint,
    Key = <<"U_N_SMALL">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {65535, 65535}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

unsigned_tinyint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = unsigned_tinyint,
    Key = <<"U_TINY">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {255, 255}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

unsigned_nullable_tinyint(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = unsigned_nullable_tinyint,
    Key = <<"U_N_TINY">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {255, 255}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

bit(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = bit,
    Key = <<"BIT">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {0, 0},
        %% case 2
        {1, 1}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

integer_numeric(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = integer_numeric,
    Key = <<"I_NUMERIC">>,
    TestCases = [
        %% source value, result value
        %% case 1
        { {decimal, "0"},   0},
        %% case 2
        { {decimal, "1"},   1},
        %% case 3
        { {decimal, "-1"}, -1},
        %% case 4
        { {decimal, "999999999999999999999999999999999999"}, 999999999999999999999999999999999999},
        %% case 5
        {{decimal, "-999999999999999999999999999999999999"}, -999999999999999999999999999999999999}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

integer_nullable_numeric(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = integer_nullable_numeric,
    Key = <<"I_N_NUMERIC">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        { {decimal, "0"},   0},
        %% case 3
        { {decimal, "1"},   1},
        %% case 4
        { {decimal, "-1"}, -1},
        %% case 5
        { {decimal, "999999999999999999999999999999999999"}, 999999999999999999999999999999999999},
        %% case 6
        {{decimal, "-999999999999999999999999999999999999"}, -999999999999999999999999999999999999}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

fractional_numeric(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = fractional_numeric,
    Key = <<"F_NUMERIC">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {{decimal, "0"},  {0, 0, -2}},
        %% case 2
        {{decimal, "1"},  {0, 100, -2}},
        %% case 3
        {{decimal, "-1"}, {1, 100, -2}},
        %% case 4
        {{decimal, "999999999999999999999999999999999999.99"},  {0, 99999999999999999999999999999999999999, -2}},
        %% case 5
        {{decimal, "-999999999999999999999999999999999999.99"}, {1, 99999999999999999999999999999999999999, -2}}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

fractional_nullable_numeric(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = fractional_nullable_numeric,
    Key = <<"F_N_NUMERIC">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {{decimal, "0"}, {0, 0, -2}},
        %% case 3
        {{decimal, "1"}, {0, 100, -2}},
        %% case 4
        {{decimal, "-1"}, {1, 100, -2}},
        %% case 5
        {{decimal, "999999999999999999999999999999999999.99"},  {0, 99999999999999999999999999999999999999, -2}},
        %% case 6
        {{decimal, "-999999999999999999999999999999999999.99"}, {1, 99999999999999999999999999999999999999, -2}}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

float(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = float,
    Key = <<"FLOAT">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {3.14, 3.14},
        %% case 2
        {-3.14, -3.14}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nullable_float(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nullable_float,
    Key = <<"N_FLOAT">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {3.14, 3.14},
        %% case 3
        {-3.14, -3.14}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

real(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = real,
    Key = <<"REAL">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {3.140000104904175, 3.140000104904175},
        %% case 2
        {-3.140000104904175, -3.140000104904175}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nullable_real(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nullable_real,
    Key = <<"N_REAL">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {3.140000104904175, 3.140000104904175},
        %% case 3
        {-3.140000104904175, -3.140000104904175}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

datetime(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = datetime,
    Key = <<"DATETIME">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {"2013-08-01 16:00:00", {{2013,8,1},{16,0,0}}}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_datetime(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_datetime,
    Key = <<"N_DATETIME">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {"2013-08-01 16:00:00", {{2013,8,1},{16,0,0}}}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

smalldatetime(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = smalldatetime,
    Key = <<"SMALLDATETIME">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {"2013-08-01 16:00:00", {{2013,8,1},{16,0,0}}}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_smalldatetime(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_smalldatetime,
    Key = <<"N_SMALLDATETIME">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {"2013-08-01 16:00:00", {{2013,8,1},{16,0,0}}}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

date(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = date,
    Key = <<"DATE">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {"2013-08-01", {2013,8,1} }
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_date(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_date,
    Key = <<"N_DATE">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {"2013-08-01", {2013,8,1} },
        %% case 3
        {"2013-08-01 16:00:00", {2013,8,1} }
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

time(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = time,
    Key = <<"TIME">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {"16:00:00", {16,0,0} },
        %% case 2
        {"2013-08-01 16:00:00", {16,0,0} }
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_time(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_time,
    Key = <<"N_TIME">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {"16:00:00", {16,0,0} },
        %% case 3
        {"2013-08-01 16:00:00", {16,0,0} }
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

char(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = char,
    Key = <<"CHAR">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<"          ">>},
        %% case 2
        {<<"a">>, <<"a         ">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nullable_char(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nullable_char,
    Key = <<"N_CHAR">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<"          ">>},
        %% case 3
        {<<"a">>, <<"a         ">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nchar,
    Key = <<"NCHAR">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<"                              ">>},
        %% case 2
        {<<"a">>, <<"a                             ">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_nchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_nchar,
    Key = <<"N_NCHAR">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

varchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = varchar,
    Key = <<"VARCHAR">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_varchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_varchar,
    Key = <<"N_VARCHAR">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nvarchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nvarchar,
    Key = <<"NVARCHAR">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_nvarchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_nvarchar,
    Key = <<"N_NVARCHAR">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

binary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = binary,
    Key = <<"BINARY">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ",0,0,0,0,0,0,0,0,0>>},
        %% case 2
        {<<"a">>, <<"a",0,0,0,0,0,0,0,0,0>>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).


nulable_binary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_binary,
    Key = <<"N_BINARY">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

varbinary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = varbinary,
    Key = <<"VARBINARY">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, <<" ">>},
        %% case 2
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_varbinary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_varbinary,
    Key = <<"N_VARBINARY">>,
    TestCases = [
        %% source value, result value
        %% case 1
        {null, null},
        %% case 2
        {<<"">>, <<" ">>},
        %% case 3
        {<<"a">>, <<"a">>}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_char(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_char,
    Key = <<"W_CHAR">>,
    RValue1 = << <<" ">> || _ <- lists:seq(1,1000)>>,
    Value2 = << <<"abcd_">> || _ <- lists:seq(1,200)>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, RValue1},
        %% case 2
        {Value2, Value2}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_nullable_char(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_nullable_char,
    Key = <<"W_N_CHAR">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_nchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_nchar,
    Key = <<"W_NCHAR">>,
    RValue2 = << <<" ">> || _ <- lists:seq(1,3000)>>,
    Value3 = << << <<"abcd_">> || _ <- lists:seq(1,200)>>/binary, RValue2:2000/binary>>,
    TestCases = [
        %% source value, result value
        %% case 1
        {<<"">>, RValue2},
        %% case 2
        {Value3, Value3}
    ],
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_nullable_nchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_nullable_nchar,
    Key = <<"W_N_NCHAR">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_varchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_varchar,
    Key = <<"W_VARCHAR">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_nulable_varchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_nulable_varchar,
    Key = <<"W_N_VARCHAR">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_nvarchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_nvarchar,
    Key = <<"W_NVARCHAR">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_nulable_nvarchar(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_nulable_nvarchar,
    Key = <<"W_N_NVARCHAR">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_binary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_binary,
    Key = <<"W_BINARY">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_nulable_binary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_nulable_binary,
    Key = <<"W_N_BINARY">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_varbinary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_varbinary,
    Key = <<"W_VARBINARY">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

widetable_nulable_varbinary(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = widetable_nulable_varbinary,
    Key = <<"W_N_VARBINARY">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

text(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = text,
    Key = <<"TEXT">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_text(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_text,
    Key = <<"N_TEXT">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

unitext(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = unitext,
    Key = <<"UNITEXT">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

nulable_unitext(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = nulable_unitext,
    Key = <<"N_UNITEXT">>,
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
    run_testcases(ConnRef, Table, Key, TestCases).

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


%% internal
table_desc(signed_bigint) ->
    "S_BIG bigint not null";
table_desc(signed_nullable_bigint) ->
    "S_N_BIG bigint null";
table_desc(signed_int) ->
    "S_INT int not null";
table_desc(signed_nullable_int) ->
    "S_N_INT int null";
table_desc(signed_smallint) ->
    "S_SMALL smallint not null";
table_desc(signed_nullable_smallint) ->
    "S_N_SMALL smallint null";
table_desc(unsigned_bigint) ->
    "U_BIG unsigned bigint not null";
table_desc(unsigned_nullable_bigint) ->
    "U_N_BIG unsigned bigint null";
table_desc(unsigned_int) ->
    "U_INT unsigned int not null";
table_desc(unsigned_nullable_int) ->
    "U_N_INT unsigned int null";
table_desc(unsigned_smallint) ->
    "U_SMALL unsigned smallint not null";
table_desc(unsigned_nullable_smallint) ->
    "U_N_SMALL unsigned smallint null";
table_desc(unsigned_tinyint) ->
    "U_TINY tinyint not null";
table_desc(unsigned_nullable_tinyint) ->
    "U_N_TINY tinyint null";
table_desc(bit) ->
    "BIT bit not null";
table_desc(integer_numeric) ->
    "I_NUMERIC numeric(38,0)";
table_desc(integer_nullable_numeric) ->
    "I_N_NUMERIC numeric(38,0) null";
table_desc(fractional_numeric) ->
    "F_NUMERIC numeric(38,2)";
table_desc(fractional_nullable_numeric) ->
    "F_N_NUMERIC numeric(38,2) null";
table_desc(float) ->
    "FLOAT float not null";
table_desc(nullable_float) ->
    "N_FLOAT float null";
table_desc(real) ->
    "REAL real not null";
table_desc(nullable_real) ->
    "N_REAL real null";
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

run_testcases(ConnRef, Table, Key, Cases) ->
    [begin
        {ok, RValue} = run_testcase(ConnRef, Table, Key, SValue)
    end || {SValue, RValue} <- Cases].

run_testcase(ConRef, Table, Key, SrcValue) ->
    {ok, [{affected_rows,1}]}                       = insert(ConRef, Table, Key, SrcValue),
    {ok, [{result_set,[Key],[],[[ResultValue]]}]}   = select(ConRef, Table),
    {ok, [{affected_rows,1}]}                       = delete(ConRef, Table),
    io:format("SourceValue:~p~n", [SrcValue]),
    io:format("ResultValue:~p~n", [ResultValue]),
    {ok, ResultValue}.

insert(ConnRef, Tab, Key, Value) ->
    Table = atom_to_binary(Tab, utf8),
    Query1 = [<<"insert into ", Table/binary, "(", Key/binary, ") "
                                "VALUES( ">>, arg, <<" )">>],
    Query2 = format_query(Query1, [Value]),
    jamdb_sybase:sql_query(ConnRef, Query2).

select(ConnRef, Table) ->
    Query = lists:concat(["select * from ", Table]),
    jamdb_sybase:sql_query(ConnRef, Query).

delete(ConnRef, Table) ->
    Query = lists:concat(["delete from ", Table]),
    jamdb_sybase:sql_query(ConnRef, Query).

%% Temporary wrapper
format_query(Query, Args) ->
    format_query(Query, Args, <<>>).

format_query([QueryPart|RestQuery], Args, Result) when is_binary(QueryPart) ->
    Result2 = <<Result/binary, QueryPart/binary>>,
    format_query(RestQuery, Args, Result2);
format_query([arg|RestQuery], [FirstArg|RestArgs], Result)  ->
    IsChar = is_binary(FirstArg) orelse is_list(FirstArg),
    Result2 = case IsChar of
        true ->
            <<Result/binary, "'", (encode_data(FirstArg))/binary, "'">>;
        false ->
            <<Result/binary, (encode_data(FirstArg))/binary>>
    end,
    format_query(RestQuery, RestArgs, Result2);
format_query([], [], Result) ->
    Result.

escape(Binary) ->
    escape(Binary, <<>>).
    
escape(<<"'", Binary/binary>>, Result) ->
    escape(Binary, <<Result/binary, "''">>);
escape(<<X, Binary/binary>>, Result) ->
    escape(Binary, <<Result/binary, X>>);
escape(<<>>, Result) ->
    Result.

encode_data(Data) when is_binary(Data) ->
    escape(Data);
encode_data(Data) when is_list(Data) ->
    Binary = unicode:characters_to_binary(Data),
    encode_data(Binary);
encode_data({{Y, M1, D}, {H, M2, S}}) ->
    DateFormat = "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
    List = lists:flatten(io_lib:format(DateFormat, [Y, M1, D, H, M2, S])),
    encode_data(List);
encode_data(Data) when is_atom(Data) ->
    Binary = atom_to_binary(Data, unicode),
    encode_data(Binary);
encode_data(Data) when is_integer(Data) ->
    List = integer_to_list(Data),
    encode_data(List);
encode_data(Data) when is_float(Data) ->
    List = float_to_list(Data),
    encode_data(List);
encode_data({_, List}) when is_list(List) ->
    encode_data(List).
