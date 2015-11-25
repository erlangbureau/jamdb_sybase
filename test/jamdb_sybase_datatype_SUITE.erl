-module(jamdb_sybase_datatype_SUITE).

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
        unsigned_bigint,
        unsigned_nullable_bigint,
        signed_bigint,
        signed_nullable_bigint,
        unsigned_int,
        unsigned_nullable_int,
        signed_int,
        signed_nullable_int,
        unsigned_smallint,
        unsigned_nullable_smallint,
        signed_smallint,
        signed_nullable_smallint,
        unsigned_tinyint,
        unsigned_nullable_tinyint,
        float,
        nullable_float,
        real,
        nullable_real,
        integer_numeric,
        integer_nullable_numeric,
        fractional_numeric,
        fractional_nullable_numeric,
        char,
        nullable_char,
        nchar,
        nulable_nchar,
        varchar,
        nulable_varchar,
        nvarchar,
        nulable_nvarchar,
        binary,
        nulable_binary,
        varbinary,
        nulable_varbinary,
        widetable_char,
        widetable_nullable_char,
        widetable_nchar,
        widetable_nullable_nchar,
        widetable_varchar,
        widetable_nulable_varchar,
        widetable_nvarchar,
        widetable_nulable_nvarchar,
        widetable_binary,
        widetable_nulable_binary,
        widetable_varbinary,
        widetable_nulable_varbinary,
        text,
        nulable_text,
        datetime,
        nulable_datetime,
        smalldatetime,
        nulable_smalldatetime,
        date,
        nulable_date,
        time,
        nulable_time
%        %money,
%        %nulable_money,
%        %smallmoney,
%        %nulable_smallmoney,
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
    Query = lists:concat(["create table ", Case, "( ", table_desc(Case), " )"]),
    {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query),
    Config.

end_per_testcase(Case, Config) ->
    ConnRef = ?config(conn_ref, Config),
    Query = lists:concat(["drop table ", Case]),
    {ok, [{affected_rows,0}]} = jamdb_sybase:sql_query(ConnRef, Query),
    ok.

%% test cases
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

integer_numeric(Config) ->
    ConnRef = ?config(conn_ref, Config),
    Table = integer_numeric,
    Key = <<"I_NUMERIC">>,
    TestCases = [
        %% source value, result value
        %% case 1
        { {decimal, "1"}, 1},
        %% case 2
        { {decimal, "999999999999999999999999999999999999"}, 999999999999999999999999999999999999},
        %% case 3
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
        { {decimal, "1"}, 1},
        %% case 3
        { {decimal, "999999999999999999999999999999999999"}, 999999999999999999999999999999999999},
        %% case 4
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
        {{decimal, "1"}, {decimal, 100, 2}},
        %% case 2
        {{decimal, "999999999999999999999999999999999999.99"}, {decimal, 99999999999999999999999999999999999999, 2} },
        %% case 3
        {{decimal, "-999999999999999999999999999999999999.99"}, {decimal, -99999999999999999999999999999999999999, 2} }
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
        {{decimal, "1"}, {decimal, 100, 2}},
        %% case 3
        {{decimal, "999999999999999999999999999999999999.99"}, {decimal, 99999999999999999999999999999999999999, 2} },
        %% case 4
        {{decimal, "-999999999999999999999999999999999999.99"}, {decimal, -99999999999999999999999999999999999999, 2} }
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

run_testcases(ConnRef, Table, Key, Cases) ->
    [begin
        {ok, RValue} = run_testcase(ConnRef, Table, Key, SValue)
    end || {SValue, RValue} <- Cases].

run_testcase(ConRef, Table, Key, SrcValue) ->
    {ok, [{affected_rows,1}]}                       = insert(ConRef, Table, Key, SrcValue),
    {ok, [{result_set,[Key],[],[[ResultValue]]}]}   = select(ConRef, Table),
    {ok, [{affected_rows,1}]}                       = delete(ConRef, Table),
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
