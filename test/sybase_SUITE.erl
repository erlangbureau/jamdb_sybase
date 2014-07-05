-module(sybase_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([all/0]).

%% Tests
-export([create_tables/1, drop_tables/1]).
-export([simple_select/1, empty_select/1]).
-export([insert_integer_types/1, select_integer_types/1]).
-export([insert_char_types/1, select_char_types/1]).
-export([insert_text_type/1, select_text_type/1]).
-export([insert_float_types/1, select_float_types/1]).
%-export([insert_money_types/1, select_money_types/1]).
-export([insert_time_types/1, select_time_types/1]).
-export([insert_widetable/1, select_widetable/1]).
-export([creaate_procedure/1, execute_procedure/1, drop_procedure/1]).

-define(Host, "127.0.0.1").
-define(Port, 5000).
-define(Login, "test").
-define(Password, "TestTest").
-define(Database, "TEST").

%% Common Test
all() ->
	[
        create_tables, 
        simple_select,
        empty_select,
        insert_integer_types,
        insert_char_types,
        insert_text_type,
        insert_float_types,
        insert_widetable,
        %insert_money_types,
        insert_time_types,
        %insert_blob_types,
        select_integer_types,
        select_char_types,
        select_text_type,
        select_float_types,
        select_widetable,
        %select_money_types,
        select_time_types,
        %select_blob_types,
        creaate_procedure,
        execute_procedure,
        drop_procedure,
        drop_tables
    ].

%% tests
create_tables(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {[{affected_rows,0}], 
        "create table ERL_DRV_int_null_tests( "
            "U_BIG unsigned bigint null, "
            "S_BIG bigint null, "
            "U_INT unsigned int null, "
            "S_INT int null, "
            "U_SMALL unsigned smallint null, "
            "S_SMALL smallint null, "
            "U_TINY tinyint null, "
            "NUM numeric(38,2) null"
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_int_not_null_tests( "
            "U_BIG unsigned bigint not null, "
            "S_BIG bigint not null, "
            "U_INT unsigned int not null, "
            "S_INT int not null, "
            "U_SMALL unsigned smallint not null, "
            "S_SMALL smallint not null, "
            "U_TINY tinyint not null, "
            "NUM numeric(38,2)"
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_char_null_tests( "
            "CHAR char(10) null, "
            "NCHAR nchar(10) null, "
            "VARCHAR varchar(10) null, "
            "NVARCHAR nvarchar(10) null, "
            "BINARY binary(10) null, "
            "VARBINARY varbinary(10) null "
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_char_not_null_tests( "
            "CHAR char(10) not null, "
            "NCHAR nchar(10) not null, "
            "VARCHAR varchar(10) not null, "
            "NVARCHAR nvarchar(10) not null, "
            "BINARY binary(10) not null, "
            "VARBINARY varbinary(10) not null"
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_widetable_null_tests( "
            "CHAR char(1000) null, "
            "NCHAR nchar(1000) null, "
            "VARCHAR varchar(1000) null, "
            "NVARCHAR nvarchar(1000) null, "
            "BINARY binary(1000) null, "
            "VARBINARY varbinary(1000) null "
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_widetable_not_null_tests( "
            "CHAR char(1000) not null, "
            "NCHAR nchar(1000) not null, "
            "VARCHAR varchar(1000) not null, "
            "NVARCHAR nvarchar(1000) not null, "
            "BINARY binary(1000) not null, "
            "VARBINARY varbinary(1000) not null"
        ")"
        },
       {[{affected_rows,0}], 
        "create table ERL_DRV_text_null_tests( "
            "TEXT text null "
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_text_not_null_tests( "
            "TEXT text not null "
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_float_null_tests( "
            "FLOAT float null,"
            "REAL real null"
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_float_not_null_tests( "
            "FLOAT float not null,"
            "REAL real not null"
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_money_null_tests( "
            "MONEY money null, "
            "SMALLMONEY smallmoney null"
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_money_not_null_tests( "
            "MONEY money not null, "
            "SMALLMONEY smallmoney not null"
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_time_null_tests( "
            "DATETIME datetime null, "
            "SMALLDATETIME smalldatetime null, "
            "DATE date null, "
            "TIME time null"
        ")"
        },
        {[{affected_rows,0}], 
        "create table ERL_DRV_time_not_null_tests( "
            "DATETIME datetime not null, "
            "SMALLDATETIME smalldatetime not null, "
            "DATE date not null, "
            "TIME time not null"
        ")"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

drop_tables(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {[{affected_rows,0}], "drop table ERL_DRV_int_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_int_not_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_float_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_float_not_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_char_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_char_not_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_text_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_text_not_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_time_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_time_not_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_money_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_money_not_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_widetable_null_tests"},
        {[{affected_rows,0}], "drop table ERL_DRV_widetable_not_null_tests"}
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

simple_select(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set,[<<>>,<<>>,<<>>],[],[[1,<<"test">>,undefined]]}],
            <<"select 1, 'test', null">>
        },
        {   
            [{result_set,[<<>>,<<>>,<<>>],[], [[1,<<"test">>,undefined]]}],
            <<"select 1, 'test', null">>
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

empty_select(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set,[<<"U_BIG">>,<<"S_BIG">>,<<"U_INT">>,
                                <<"S_INT">>,<<"U_SMALL">>,<<"S_SMALL">>,
                                <<"U_TINY">>,<<"NUM">>],[],[]}],
            <<"select U_BIG, S_BIG, U_INT, S_INT, U_SMALL, S_SMALL, U_TINY, NUM from ERL_DRV_int_null_tests">>
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

insert_integer_types(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_int_null_tests(U_BIG, S_BIG, U_INT, "
                    "S_INT, U_SMALL, S_SMALL, U_TINY, NUM) "
                "VALUES(null, null, null, null, null, null, null, null)"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_int_null_tests(U_BIG, S_BIG, U_INT, "
                    "S_INT, U_SMALL, S_SMALL, U_TINY, NUM) "
                "VALUES(18446744073709551615, -9223372036854775808, "
                        "4294967295, -2147483648, 65535, -32768, 255, "
                        "999999999999999999999999999999999999.99)"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_int_not_null_tests(U_BIG, S_BIG, U_INT, "
                    "S_INT, U_SMALL, S_SMALL, U_TINY, NUM) "
                "VALUES(18446744073709551615, -9223372036854775808, "
                        "4294967295, -2147483648, 65535, -32768, 255, "
                        "999999999999999999999999999999999999.99)"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

select_integer_types(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set, [<<"U_BIG">>, <<"S_BIG">>, <<"U_INT">>, <<"S_INT">>, 
                        <<"U_SMALL">>,<<"S_SMALL">>, <<"U_TINY">>, <<"NUM">>], [],
                [
                    [undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined],
                    [18446744073709551615, -9223372036854775808, 
                        4294967295, -2147483648, 65535, -32768, 255, 
                        {numeric, 99999999999999999999999999999999999999, 2}]
                ]}],
            "select U_BIG, S_BIG, U_INT, S_INT, U_SMALL, S_SMALL, U_TINY, NUM "
                "from ERL_DRV_int_null_tests"
        },
        {   
            [{result_set, [<<"U_BIG">>, <<"S_BIG">>, <<"U_INT">>, <<"S_INT">>, 
                            <<"U_SMALL">>, <<"S_SMALL">>, <<"U_TINY">>, <<"NUM">>], [],
                [
                    [18446744073709551615, -9223372036854775808, 
                        4294967295, -2147483648, 65535, -32768, 255, 
                        {numeric, 99999999999999999999999999999999999999, 2}]
                ]}],
            "select U_BIG, S_BIG, U_INT, S_INT, U_SMALL, S_SMALL, U_TINY, NUM "
                "from ERL_DRV_int_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

insert_char_types(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_char_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES(null, null, null, null, null, null)"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_char_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES('a', 't', 'q', 'z', 'test1', 'test2')"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_char_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES('', '', '', '', '', '')"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_char_not_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES('a', 't', 'q', 'z', 'test1', 'test2')"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_char_not_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES('', '', '', '', '', '')"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

select_char_types(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set,[<<"CHAR">>, <<"NCHAR">>, <<"VARCHAR">>, <<"NVARCHAR">>, <<"BINARY">>,<<"VARBINARY">>], [],
                [
                    [undefined, undefined, undefined, undefined, undefined, undefined],
                    [<<"a         ">>, <<"t">>, <<"q">>, <<"z">>, <<"test1">>, <<"test2">>],
                    [<<"          ">>, <<" ">>, <<" ">>, <<" ">>, <<" ">>, <<" ">>]
                ]}],
            "select CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY "
                "from ERL_DRV_char_null_tests"
        },
        {   
            [{result_set,[<<"CHAR">>, <<"NCHAR">>, <<"VARCHAR">>, <<"NVARCHAR">>, <<"BINARY">>,<<"VARBINARY">>], [],
                [
                    [<<"a         ">>, <<"t                             ">>, <<"q">>, <<"z">>, <<"test1",0,0,0,0,0>>, <<"test2">>],
                    [<<"          ">>, <<"                              ">>, <<" ">>, <<" ">>, <<" ",0,0,0,0,0,0,0,0,0>>, <<" ">>]
                ]}],
            "select CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY "
                "from ERL_DRV_char_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

insert_widetable(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    LongAsciiString = << <<"a">> || _ <- lists:seq(1,1000)>>,
    Tests = [
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_widetable_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES(null, null, null, null, null, null)"
        },
        {   
            [{affected_rows,1}],
            ["insert into ERL_DRV_widetable_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES('", LongAsciiString, "', '", LongAsciiString, "', '", LongAsciiString, ",', '", LongAsciiString, "', '", LongAsciiString, "', '", LongAsciiString, "')"]
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_widetable_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES('', '', '', '', '', '')"
        },
        {   
            [{affected_rows,1}],
            ["insert into ERL_DRV_widetable_not_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES('", LongAsciiString, "', '", LongAsciiString, "', '", LongAsciiString, ",', '", LongAsciiString, "', '", LongAsciiString, "', '", LongAsciiString, "')"]
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_widetable_not_null_tests"
                    "(CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY) "
                "VALUES('', '', '', '', '', '')"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

select_widetable(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    LongAsciiString = << <<"a">> || _ <- lists:seq(1,1000)>>,
    LongEmptyString = << <<" ">> || _ <- lists:seq(1,1000)>>,
    NLongAsciiString = <<LongAsciiString/binary, LongEmptyString/binary, LongEmptyString/binary>>,
    NEmptyString = << <<" ">> || _ <- lists:seq(1,3000)>>,
    LongEmptyBinary = <<" ", << <<0>> || _ <- lists:seq(1,999)>>/binary>>,
    Tests = [
        {   
            [{result_set,[<<"CHAR">>, <<"NCHAR">>, <<"VARCHAR">>, <<"NVARCHAR">>, <<"BINARY">>,<<"VARBINARY">>], [],
                [
                    [undefined, undefined, undefined, undefined, undefined, undefined],
                    [LongAsciiString, LongAsciiString, LongAsciiString, LongAsciiString, LongAsciiString, LongAsciiString],
                    [LongEmptyString, <<" ">>, <<" ">>, <<" ">>, <<" ">>, <<" ">>]
                ]}],
            "select CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY "
                "from ERL_DRV_widetable_null_tests"
        },
        {   
            [{result_set,[<<"CHAR">>, <<"NCHAR">>, <<"VARCHAR">>, <<"NVARCHAR">>, <<"BINARY">>,<<"VARBINARY">>], [],
                [
                    [LongAsciiString, NLongAsciiString, LongAsciiString, LongAsciiString, LongAsciiString, LongAsciiString],
                    [LongEmptyString, NEmptyString, <<" ">>, <<" ">>, LongEmptyBinary, <<" ">>]
                ]}],
            "select CHAR, NCHAR, VARCHAR, NVARCHAR, BINARY, VARBINARY "
                "from ERL_DRV_widetable_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

insert_text_type(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_text_null_tests"
                    "(TEXT) "
                "VALUES(null)"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_text_null_tests"
                    "(TEXT) "
                "VALUES('a')"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_text_null_tests"
                    "(TEXT) "
                "VALUES('test test test')"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_text_null_tests"
                    "(TEXT) "
                "VALUES('')"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_text_not_null_tests"
                    "(TEXT) "
                "VALUES('a')"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_text_not_null_tests"
                    "(TEXT) "
                "VALUES('test test test')"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_text_not_null_tests"
                    "(TEXT) "
                "VALUES('')"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

select_text_type(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set,[<<"TEXT">>], [],
                [
                    [undefined],
                    [<<"a">>],
                    [<<"test test test">>],
                    [<<" ">>]
                ]}],
            "select TEXT "
                "from ERL_DRV_text_null_tests"
        },
        {   
            [{result_set,[<<"TEXT">>], [],
                [
                    [<<"a">>],
                    [<<"test test test">>],
                    [<<" ">>]
                ]}],
            "select TEXT "
                "from ERL_DRV_text_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

insert_float_types(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {[{affected_rows,1}], "insert into ERL_DRV_float_null_tests(FLOAT, REAL) VALUES(null, null)"},
        {[{affected_rows,1}], "insert into ERL_DRV_float_null_tests(FLOAT, REAL) VALUES(3.14, 3.14)"},
        {[{affected_rows,1}], "insert into ERL_DRV_float_not_null_tests(FLOAT, REAL) VALUES(-3.14, -3.14)"}
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

select_float_types(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set, [<<"FLOAT">>,<<"REAL">>], [], [ 
                [undefined, undefined], 
                [3.14, 3.140000104904175] 
            ]}],
            "select FLOAT, REAL from ERL_DRV_float_null_tests"
        },
        {   
            [{result_set, [<<"FLOAT">>,<<"REAL">>], [], [ 
                [-3.14, -3.140000104904175] 
            ]}],
            "select FLOAT, REAL from ERL_DRV_float_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

%insert_money_types(_Config) ->
%    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
%    Tests = [
%        {1, "insert into ERL_DRV_money_null_tests(SMALLMONEY, MONEY) "
%                "VALUES(null, null)"
%        },
%        {1, "insert into ERL_DRV_money_null_tests(SMALLMONEY, MONEY) "
%                "VALUES(3.14, 3.14)"
%        },
%        {1, "insert into ERL_DRV_money_not_null_tests(SMALLMONEY, MONEY) "
%                "VALUES(3.14, 3.14)"
%        }
%    ],
%    _ = [{ResultSets, Query} = begin
%            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
%            {RS, Query}
%        end || {ResultSets, Query} <- Tests],
%    {ok, _State2} = jamdb_sybase:close(State),
%    ok.
%
%select_money_types(_Config) ->
%    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
%    Tests = [
%        {   
%            [{result_set, [<<"SMALLMONEY">>, <<"MONEY">>],
%                        [
%                            [null, null],
%                            [3.14,3.140000104904175]
%                        ]}],
%            "select SMALLMONEY, MONEY from ERL_DRV_money_null_tests"
%        },
%        {   
%            [{result_set, [<<"SMALLMONEY">>, <<"MONEY">>],
%                        [
%                            [3.14,3.140000104904175]
%                        ]}],
%            "select SMALLMONEY, MONEY from ERL_DRV_money_not_null_tests"
%        }
%    ],
%    _ = [{ResultSets, Query} = begin
%            {result, RS, _} = jamdb_sybase:sql_query(State, Query),
%            {RS, Query}
%        end || {ResultSets, Query} <- Tests],
%    {ok, _State2} = jamdb_sybase:close(State),
%    ok.

insert_time_types(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_time_null_tests(DATETIME, SMALLDATETIME, "
                    "DATE, TIME) "
                "VALUES(null, null, null, null)"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_time_null_tests(DATETIME, SMALLDATETIME, "
                    "DATE, TIME) "
                "VALUES('2013-08-01 16:00:00', '2013-08-01 16:00:00', "
                    "'2013-08-01', '16:00:00')"
        },
        {   
            [{affected_rows,1}],
            "insert into ERL_DRV_time_not_null_tests(DATETIME, SMALLDATETIME, "
                    "DATE, TIME) "
                "VALUES('2013-08-01 16:00:00', '2013-08-01 16:00:00', "
                    "'2013-08-01', '16:00:00')"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

select_time_types(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set,[<<"DATETIME">>, <<"SMALLDATETIME">>, 
                            <<"DATE">>, <<"TIME">>], [],
                        [
                            [undefined, undefined, undefined, undefined],
                            [{{2013,8,1},{16,0,0}}, {{2013,8,1},{16,0,0}}, 
                                {date, {2013,8,1}}, {time, {16,0,0}}]
                        ]
            }],
            "select DATETIME, SMALLDATETIME, DATE, TIME "
                "from ERL_DRV_time_null_tests"
        },
        {   
            [{result_set,[<<"DATETIME">>, <<"SMALLDATETIME">>, 
                            <<"DATE">>, <<"TIME">>], [],
                        [
                            [{{2013,8,1},{16,0,0}}, {{2013,8,1},{16,0,0}}, 
                                {date, {2013,8,1}}, {time, {16,0,0}}]
                        ]}],
            "select DATETIME, SMALLDATETIME, DATE, TIME "
                "from ERL_DRV_time_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.


creaate_procedure(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{affected_rows,0}],
            "create procedure erl_drv_ok_procedure as "
                "begin "
                    "select 1 "
                    "select 'a' "
                    "select 'b' "
                "end"
        },
        {   
            [{affected_rows,0}],
            "create procedure erl_drv_err_procedure as "
                "begin "
                    "select 1 "
                    "select 'a' "
                    "select 'b' "
                    "select 'c' "
                    "return 22 "
                "end"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

execute_procedure(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [
                {result_set,[<<>>], [], [[1]]},
                {result_set,[<<>>], [], [[<<"a">>]]},
                {result_set,[<<>>], [], [[<<"b">>]]},
                {procedure_result,0,[]}
            ],
            "exec erl_drv_ok_procedure"
        },
        {   
            [
                {result_set,[<<>>], [], [[1]]},
                {result_set,[<<>>], [], [[<<"a">>]]},
                {result_set,[<<>>], [], [[<<"b">>]]},
                {result_set,[<<>>], [], [[<<"c">>]]},
                {procedure_result,22,[]}
            ],
            "exec erl_drv_err_procedure"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.

drop_procedure(_Config) ->
    {ok, State} = jamdb_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{affected_rows,0}],
            "drop procedure erl_drv_ok_procedure"
        },
        {   
            [{affected_rows,0}],
            "drop procedure erl_drv_err_procedure"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = jamdb_sybase:sql_query(State, Query),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = jamdb_sybase:close(State),
    ok.
