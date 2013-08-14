-module(sybase_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([all/0]).

%% Tests
-export([create_tables/1, drop_tables/1]).
-export([simple_select/1]).
-export([insert_integer_types/1, select_integer_types/1]).
-export([insert_float_types/1, select_float_types/1]).
-export([insert_characters_types/1, select_characters_types/1]).
-export([insert_time_types/1, select_time_types/1]).
-export([creaate_procedure/1, execute_procedure/1, drop_procedure/1]).

-define(Host, "127.0.0.1").
-define(Port, 4100).
-define(Login, "test").
-define(Password, "TestTest").
-define(Database, "TEST").

%% Common Test
all() ->
	[
        create_tables, 
        simple_select,
        insert_integer_types,
        insert_float_types,
        insert_characters_types,
        insert_time_types,
        select_integer_types,
        select_float_types,
        select_characters_types,
        select_time_types,
        creaate_procedure,
        execute_procedure,
        drop_procedure,
        drop_tables
    ].

%% tests
create_tables(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {0, 
        "create table ERLANG_DRV_int_null_tests( "
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
        {0, 
        "create table ERLANG_DRV_int_not_null_tests( "
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
        {0, 
        "create table ERLANG_DRV_flt_null_tests( "
            "FLOAT float null,"
            "REAL real null"
        ")"
        },
        {0, 
        "create table ERLANG_DRV_flt_not_null_tests( "
            "FLOAT float not null,"
            "REAL real not null"
        ")"
        },
        {0, 
        "create table ERLANG_DRV_char_null_tests( "
            "CHAR char(10) null, "
            "VARCHAR varchar(10) null, "
            "TEXT text null, "
            "BINARY binary(10) null, "
            "VARBINARY varbinary(10) null "
        ")"
        },
        {0, 
        "create table ERLANG_DRV_char_not_null_tests( "
            "CHAR char(10) not null, "
            "VARCHAR varchar(10) not null, "
            "TEXT text not null, "
            "BINARY binary(10) not null, "
            "VARBINARY varbinary(10) not null"
        ")"
        },
        {0, 
        "create table ERLANG_DRV_time_null_tests( "
            "DATETIME datetime null, "
            "SMALLDATETIME smalldatetime null, "
            "DATE date null, "
            "TIME time null"
        ")"
        },
        {0, 
        "create table ERLANG_DRV_time_not_null_tests( "
            "DATETIME datetime not null, "
            "SMALLDATETIME smalldatetime not null, "
            "DATE date not null, "
            "TIME time not null"
        ")"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

drop_tables(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {0, "drop table ERLANG_DRV_int_null_tests"},
        {0, "drop table ERLANG_DRV_int_not_null_tests"},
        {0, "drop table ERLANG_DRV_flt_null_tests"},
        {0, "drop table ERLANG_DRV_flt_not_null_tests"},
        {0, "drop table ERLANG_DRV_char_null_tests"},
        {0, "drop table ERLANG_DRV_char_not_null_tests"},
        {0, "drop table ERLANG_DRV_time_null_tests"},
        {0, "drop table ERLANG_DRV_time_not_null_tests"}
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

simple_select(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set,[<<>>,<<>>,<<>>],[[1,<<"test">>,null]]}], 
            <<"select 1, 'test', null">>
        },
        {   
            [{result_set,[<<>>,<<>>,<<>>],[[1,<<"test">>,null]]}], 
            <<"select 1, 'test', null">>
        }
    ],
    _ = [{ResultSets, Query} = begin
            {result, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

insert_integer_types(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            1,
            "insert into ERLANG_DRV_int_null_tests(U_BIG, S_BIG, U_INT, S_INT, U_SMALL, S_SMALL, U_TINY, NUM) VALUES(null, null, null, null, null, null, null, null)"
        },
        {   
            1,
            "insert into ERLANG_DRV_int_null_tests(U_BIG, S_BIG, U_INT, S_INT, U_SMALL, S_SMALL, U_TINY, NUM) "
                "VALUES(18446744073709551615, -9223372036854775808, 4294967295, -2147483648, 65535, -32768, 255, 999999999999999999999999999999999999.99)"
        },
        {   
            1,
            "insert into ERLANG_DRV_int_not_null_tests(U_BIG, S_BIG, U_INT, S_INT, U_SMALL, S_SMALL, U_TINY, NUM) "
                "VALUES(18446744073709551615, -9223372036854775808, 4294967295, -2147483648, 65535, -32768, 255, 999999999999999999999999999999999999.99)"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

select_integer_types(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set, [<<"U_BIG">>,<<"S_BIG">>,<<"U_INT">>, <<"S_INT">>,<<"U_SMALL">>,<<"S_SMALL">>, <<"U_TINY">>, <<"NUM">>],
                        [
                            [null, null, null, null, null, null, null, null],
                            [18446744073709551615, -9223372036854775808, 4294967295, -2147483648, 65535, -32768, 255, {decimal, 99999999999999999999999999999999999999, 2}]
                        ]}],
            "select U_BIG, S_BIG, U_INT, S_INT, U_SMALL, S_SMALL, U_TINY, NUM from ERLANG_DRV_int_null_tests"
        },
        {   
            [{result_set, [<<"U_BIG">>,<<"S_BIG">>,<<"U_INT">>, <<"S_INT">>,<<"U_SMALL">>,<<"S_SMALL">>, <<"U_TINY">>, <<"NUM">>],
                        [
                            [18446744073709551615, -9223372036854775808, 4294967295, -2147483648, 65535, -32768, 255, {decimal, 99999999999999999999999999999999999999, 2}]
                        ]}],
            "select U_BIG, S_BIG, U_INT, S_INT, U_SMALL, S_SMALL, U_TINY, NUM from ERLANG_DRV_int_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {result, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

insert_float_types(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            1,
            "insert into ERLANG_DRV_flt_null_tests(FLOAT, REAL) VALUES(null, null)"
        },
        {   
            1,
            "insert into ERLANG_DRV_flt_null_tests(FLOAT, REAL) VALUES(3.14, 3.14)"
        },
        {   
            1,
            "insert into ERLANG_DRV_flt_not_null_tests(FLOAT, REAL) VALUES(3.14, 3.14)"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

select_float_types(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set, [<<"FLOAT">>,<<"REAL">>],
                        [
                            [null, null],
                            [3.14,3.140000104904175]
                        ]}],
            "select FLOAT, REAL from ERLANG_DRV_flt_null_tests"
        },
        {   
            [{result_set, [<<"FLOAT">>,<<"REAL">>],
                        [
                            [3.14,3.140000104904175]
                        ]}],
            "select FLOAT, REAL from ERLANG_DRV_flt_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {result, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

insert_characters_types(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            1,
            "insert into ERLANG_DRV_char_null_tests(CHAR, VARCHAR, TEXT, BINARY, VARBINARY) VALUES(null, null, null, null, null)"
        },
        {   
            1,
            "insert into ERLANG_DRV_char_null_tests(CHAR, VARCHAR, TEXT, BINARY, VARBINARY) VALUES('asdf', 'qwerty', 'asdasdadasdasd', 'test1', 'test2')"
        },
        {   
            1,
            "insert into ERLANG_DRV_char_null_tests(CHAR, VARCHAR, TEXT, BINARY, VARBINARY) VALUES('', '', '', '', '')"
        },
        {   
            1,
            "insert into ERLANG_DRV_char_not_null_tests(CHAR, VARCHAR, TEXT, BINARY, VARBINARY) VALUES('asdf', 'qwerty', 'asdasdadasdasd', 'test1', 'test2')"
        },
        {   
            1,
            "insert into ERLANG_DRV_char_not_null_tests(CHAR, VARCHAR, TEXT, BINARY, VARBINARY) VALUES('', '', '', '', '')"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

select_characters_types(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set,[<<"CHAR">>,<<"VARCHAR">>,<<"TEXT">>, <<"BINARY">>,<<"VARBINARY">>],
                        [
                            [null, null, null, null, null],
                            [<<"asdf">>, <<"qwerty">>, <<"asdasdadasdasd">>, <<"test1">>, <<"test2">>],
                            [<<" ">>,<<" ">>,<<" ">>,<<" ">>,<<" ">>]
                        ]}],
            "select CHAR, VARCHAR, TEXT, BINARY, VARBINARY from ERLANG_DRV_char_null_tests"
        },
        {   
            [{result_set,[<<"CHAR">>,<<"VARCHAR">>,<<"TEXT">>, <<"BINARY">>,<<"VARBINARY">>],
                        [
                            [<<"asdf      ">>, <<"qwerty">>, <<"asdasdadasdasd">>, <<"test1",0,0,0,0,0>>, <<"test2">>],
                            [<<"          ">>, <<" ">>, <<" ">>, <<" ",0,0,0,0,0,0,0,0,0>>, <<" ">>]
                        ]}],
            "select CHAR, VARCHAR, TEXT, BINARY, VARBINARY from ERLANG_DRV_char_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {result, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

insert_time_types(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            1,
            "insert into ERLANG_DRV_time_null_tests(DATETIME, SMALLDATETIME, DATE, TIME) VALUES(null, null, null, null)"
        },
        {   
            1,
            "insert into ERLANG_DRV_time_null_tests(DATETIME, SMALLDATETIME, DATE, TIME) VALUES('2013-08-01 16:00:00', '2013-08-01 16:00:00', '2013-08-01', '16:00:00')"
        },
        {   
            1,
            "insert into ERLANG_DRV_time_not_null_tests(DATETIME, SMALLDATETIME, DATE, TIME) VALUES('2013-08-01 16:00:00', '2013-08-01 16:00:00', '2013-08-01', '16:00:00')"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

select_time_types(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [{result_set,[<<"DATETIME">>, <<"SMALLDATETIME">>, <<"DATE">>, <<"TIME">>],
                        [
                            [null, null, null, null],
                            [{{2013,8,1},{16,0,0}}, {{2013,8,1},{16,0,0}}, {2013,8,1}, {16,0,0}]
                        ]}],
            "select DATETIME, SMALLDATETIME, DATE, TIME from ERLANG_DRV_time_null_tests"
        },
        {   
            [{result_set,[<<"DATETIME">>, <<"SMALLDATETIME">>, <<"DATE">>, <<"TIME">>],
                        [
                            [{{2013,8,1},{16,0,0}}, {{2013,8,1},{16,0,0}}, {2013,8,1}, {16,0,0}]
                        ]}],
            "select DATETIME, SMALLDATETIME, DATE, TIME from ERLANG_DRV_time_not_null_tests"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {result, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.


creaate_procedure(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            0,
            "create procedure erl_drv_procedure as begin select 1 select 'b' select 'a' end"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

execute_procedure(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            [
                {result_set,[<<>>], [[1]]},
                {result_set,[<<>>], [[<<"b">>]]},
                {result_set,[<<>>], [[<<"a">>]]}
            ],
            "exec erl_drv_procedure"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {result, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.

drop_procedure(_Config) ->
    {ok, State} = spacejam_sybase:connect(?Host, ?Port, ?Login, ?Password, ?Database),
    Tests = [
        {   
            0,
            "drop procedure erl_drv_procedure"
        }
    ],
    _ = [{ResultSets, Query} = begin
            {ok, RS, _} = spacejam_sybase:sql_query(Query, State),
            {RS, Query}
        end || {ResultSets, Query} <- Tests],
    {ok, _State2} = spacejam_sybase:close(State),
    ok.
