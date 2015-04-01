-module(jamdb_sybase_conn).

%% API
-export([connect/5]).   %% deprecated
-export([close/1]).     %% deprecated
-export([connect/1, connect/2, disconnect/1, disconnect/2]).
-export([sql_query/2, sql_query/3]).
%-export([prepare/3, unprepare/2]).
%-export([execute/3]).

-include("TDS_5_0.hrl").
-include("jamdb_sybase.hrl").
-include("jamdb_sybase_defaults.hrl").

-define(ENCODER, jamdb_sybase_tds_encoder).
-define(DECODER, jamdb_sybase_tds_decoder).

-record(sybclient, {
    socket = undefined,
    conn_state = disconnected :: disconnected | connected | auth_negotiate,
    packet_size :: non_neg_integer(),
    tds_ver,
    server = {<<"Unknown">>, <<0,0,0,0>>},
    req_capabilities = [],
    resp_capabilities = [],
    env = []
}).

-opaque state() :: #sybclient{}.
-type error_type() :: socket | remote | local.
-type empty_result() :: {ok, state()} | {error, error_type(), binary(), state()}.
-type affected_rows() :: {affected_rows, non_neg_integer()}.
-type columns() :: list().  %% TODO
-type metainfo() :: list(). %%TODO
-type rows() :: list().  %% TODO
-type result_set() :: {result_set, columns(), metainfo(), rows()}.
-type return_status() :: non_neg_integer().
-type out_params() :: list().  %% TODO
-type procedure_result() :: 
        {proc_result, return_status(), out_params()}.
-type result() :: 
        affected_rows() | 
        result_set() | 
        procedure_result().
-type query_reult() :: {ok, [result()], state()}.
-type env() :: 
        {host, string()} |
        {port, string()} |
        {user, string()} |
        {password, string()} |
        {database, string()} |
        {app_name, string()} |
        {lib_name, string()} |
        {language, string()} |
        {packet_size, non_neg_integer()}.

-export_type([state/0]).

%% API

%% Deprecated
-spec connect(string(), inet:port_number(), string(), string(), string())
    -> empty_result().
connect(Host, Port, User, Password, Database) ->
    Env = [
        {host, Host},
        {port, Port},
        {user, User},
        {password, Password},
        {database, Database}
    ],
    connect(Env).

-spec connect([env()], timeout()) -> empty_result().
connect(Opts) ->
    connect(Opts, ?DEF_TIMEOUT).

-spec connect([env()]) -> empty_result().
connect(Opts, Timeout) ->
    Host = proplists:get_value(host, Opts, ?DEF_HOST), 
    Port = proplists:get_value(port, Opts, ?DEF_PORT),
    Database = proplists:get_value(database, Opts, ?DEF_DATABASE),
    PacketSize = proplists:get_value(packet_size, Opts, ?DEF_PACKET_SIZE),
    GenTcpOpts = [binary, {active, false}, {packet, raw}], 
    {ok, Socket} = gen_tcp:connect(Host, Port, GenTcpOpts, Timeout), %% TODO handle timeout
    State = #sybclient{
                socket        = Socket, 
                packet_size   = PacketSize,
                env           = Opts
    },
    {ok, State2} = send_auth_req(State),
    case handle_empty_resp(State2, Timeout) of
        {ok, State3 = #sybclient{conn_state = auth_negotiate}} ->
            %%TODO Negotiate
            {error, local, <<"Auth Negotiate not implemented">>, State3};
        {ok, State3 = #sybclient{conn_state = connected}} ->
            {ok, State4} = send_query_req(State3, ["use ", Database]),
            handle_empty_resp(State4, Timeout);
        Error ->
            Error
    end.

%% Deprecated
-spec close(state()) -> {ok, state()}.
close(State = #sybclient{conn_state = connected, socket=Socket}) ->
    Data = ?ENCODER:encode_token_logout(),
    {ok, State2} = send(State, ?TDS_PKT_QUERY, Data),
    {ok, State3} = handle_empty_resp(State2, ?DEF_TIMEOUT),
    ok = gen_tcp:close(Socket),
    {ok, State3#sybclient{socket=undefined, conn_state = disconnected}}.

-spec disconnect(state()) -> {ok, [env()]}.
disconnect(State) ->
    disconnect(State, ?DEF_TIMEOUT).

-spec disconnect(state(), timeout()) -> {ok, [env()]}.
disconnect(#sybclient{conn_state=connected, socket=Socket, env=Env}, 0) ->
    ok = gen_tcp:close(Socket),
    {ok, Env};
disconnect(State = #sybclient{conn_state=connected, socket=Socket, env=Env}, Timeout) ->
    Data = ?ENCODER:encode_token_logout(),
    try send(State, ?TDS_PKT_QUERY, Data) of
        {ok, State2} -> 
            {ok, _State3} = handle_empty_resp(State2, Timeout)
    after
        ok = gen_tcp:close(Socket)
    end,
    {ok, Env};
disconnect(#sybclient{env = Env}, _Timeout) ->
    {ok, Env}.

-spec sql_query(state(), string()) -> query_reult().
sql_query(State, Query) ->
    sql_query(State, Query, ?DEF_TIMEOUT).

-spec sql_query(state(), string(), timeout()) -> query_reult().
sql_query(State = #sybclient{conn_state = connected}, Query, Timeout) ->
    {ok, State2} = send_query_req(State, Query),
    handle_resp(State2, Timeout).

%prepare(State, Stmt, Query) ->
%    {ok, State2} = send_prepare_req(State, Stmt, Query),
%    handle_resp(State2).
%    {ok, State2}.

%unprepare(State, _Stmt) ->
%    {ok, State}.

%execute(State, Stmt, Args) ->
%    {ok, State2} = send_execute_req(State, Stmt, Args),
%    handle_resp(State2).
%    {ok, State2}.

%% internal
send_auth_req(#sybclient{env=Env} = State) ->
    Data = ?ENCODER:encode_login_record(Env),
    send(State, ?TDS_PKT_LOGIN, Data).

send_query_req(State, Query) ->
    BinaryQuery = unicode:characters_to_binary(Query),
    Data = ?ENCODER:encode_token_language(BinaryQuery),
    send(State, ?TDS_PKT_QUERY, Data).

%send_prepare_req(State, StmtId, Query) ->
%    BQuery = unicode:characters_to_binary(Query),
%    BStmtId = unicode:characters_to_binary(StmtId),
%    Data = ?ENCODER:encode_token_dynamic(?TDS_DYN_PREPARE, [], BStmtId, BQuery),
%    send(State, ?TDS_PKT_QUERY, Data).

%send_execute_req(State, StmtId, _Args) ->
%    Data = ?ENCODER:encode_token_dynamic(?TDS_DYN_EXEC, [], StmtId, <<"">>),
%    send(State, ?TDS_PKT_QUERY, Data).

handle_empty_resp(State, Timeout) ->
    case handle_resp(State, Timeout) of
        {ok, _, State2} ->
            {ok, State2};
        Other ->
            Other
    end.

handle_resp(State = #sybclient{socket=Socket}, Timeout) ->
    case recv(Socket, Timeout) of
        {ok, BinaryData} ->
            handle_resp(BinaryData, [], [], State);
        {error, ErrorCode} ->
            State2 = State#sybclient{conn_state = disconnected},
            {error, socket, ErrorCode, State2}
    end.

handle_resp(Data, TokensBufer, ResultSets, State) ->
    case ?DECODER:decode_token(Data, TokensBufer) of
        {{done, StatusFlags, _TransactState, Rows}, RestData} ->
            TokensBuferR = lists:reverse(TokensBufer),
            case get_result(StatusFlags, Rows, TokensBuferR, ResultSets) of
                {more, TokensBuferR2, ResultSets2} ->
                    TokensBufer2 = lists:reverse(TokensBuferR2),
                    handle_resp(RestData, TokensBufer2, ResultSets2, State);
                Result ->
                    erlang:append_element(Result, State)
            end;
        {{loginack, ConnState, TdsVersion, Server}, RestData} ->
             State2 = State#sybclient{
                conn_state  = ConnState,
                tds_ver     = TdsVersion,
                server      = Server
            },
            handle_resp(RestData, TokensBufer, ResultSets, State2);
        {{capability, ReqCap, RespCap}, RestData} ->
            State2 = State#sybclient{
                req_capabilities    = ReqCap,
                resp_capabilities   = RespCap
            },
            handle_resp(RestData, TokensBufer, ResultSets, State2);
        {{envchange, EnvChange}, RestData} ->
            State2 = set_env(EnvChange, State),
            handle_resp(RestData, TokensBufer, ResultSets, State2);
        {TokenTuple, RestData} ->
            handle_resp(RestData, [TokenTuple|TokensBufer], ResultSets, State)
    end.

get_result([more|_], _AffectedRows, TokensBufer, ResultSets) ->
    {more, TokensBufer, ResultSets};
get_result([count|RestFlags], AffectedRows, TokensBufer, ResultSets) ->
    case take_result_set(TokensBufer, AffectedRows) of
        {undefined, TokensBufer} ->
            ResultSets2 = [{affected_rows, AffectedRows}|ResultSets],
            get_result(RestFlags, AffectedRows, TokensBufer, ResultSets2);
        {NewResultSet, TokensBufer2} ->
            ResultSets2 = [NewResultSet|ResultSets],
            get_result(RestFlags, AffectedRows, TokensBufer2, ResultSets2)
    end;
get_result([], AffectedRows, _TokensBufer, []) ->
    {ok, [{affected_rows, AffectedRows}]};
get_result([], _AffectedRows, _TokensBufer, ResultSets) ->
    {ok, lists:reverse(ResultSets)};
get_result([proc|RestFlags], AffectedRows, TokensBufer, ResultSets) ->
    ResultSets2 = drop_inproc_updates(ResultSets),
    {Status, TokensBufer2} = take_procedure_status(TokensBufer),
    {OutParams, TokensBufer3} = take_outparams(TokensBufer2),
    ResultSets3 = [{procedure_result, Status, OutParams}|ResultSets2],
    get_result(RestFlags, AffectedRows, TokensBufer3, ResultSets3);
get_result([Flag|RestFlags], AffectedRows, TokensBufer, ResultSets) 
        when Flag =:= event; Flag =:= attn; Flag =:= trans ->
    get_result(RestFlags, AffectedRows, TokensBufer, ResultSets);
get_result([error|_RestFlags], _AffectedRows, TokensBufer, _ResultSets) ->
    {Message, _} = take_token(message, TokensBufer), 
        %% TODO check that the class > 10
    {error, remote, Message}.

%format_message(#message{}) ->
%#message{
%        msg_number          = MsgNumber,
%        msg_state           = MsgState,
%        class               = Class,
%        sql_state           = SQLState,
%        status              = Status,
%        transaction_state   = TransactionState,
%        msg_body            = MsgBody,
%        server_name         = ServerName,
%        procedure_name      = ProcedureName,
%        line_number         = LineNumber
%    },

take_result_set(TokensBufer, AffectedRows) ->
    case take_token(rowformat, TokensBufer) of
        {{rowformat, RowFormat}, TokensBufer2} ->
            FieldNames = [get_field_name(Fmt) || Fmt <- RowFormat],
            {MetaInfo, TokensBufer3} = take_metainfo(TokensBufer2),
            {TokensList, TokensBufer4} = take_tokens(row, TokensBufer3, AffectedRows),
            Rows = [Row || {row, Row} <- TokensList],
            {{result_set, FieldNames, MetaInfo, Rows}, TokensBufer4};
        false ->
            {undefined, TokensBufer}
    end.

get_field_name(#format{label_name = <<>>, column_name = ColumnName}) ->
    ColumnName;
get_field_name(#format{label_name = LabelName}) ->
    LabelName.

take_metainfo(TokensBufer) ->
    case take_token(orderby, TokensBufer) of
        {Token, TokensBufer2} ->
            {[Token], TokensBufer2};
        false ->
            {[], TokensBufer}
    end.

take_procedure_status(TokensBufer) ->
    take_token_value(returnstatus, TokensBufer, undefined).

take_outparams(TokensBufer) ->
    %% TODO returnvalue if simple table and params if widetable
    %{Result, TokensBufer2} = take_tokens(returnvalue, TokensBufer, all),
    %{[Value || {returnvalue, Value} <- Result], TokensBufer2}.
    take_token_value(params, TokensBufer, []).

drop_inproc_updates(ResultSets) ->
    drop_inproc_updates(ResultSets, []).

drop_inproc_updates([{affected_rows, _}|DraftResultSets], ResultSets) ->
    drop_inproc_updates(DraftResultSets, ResultSets);
drop_inproc_updates([Result|DraftResultSets], ResultSets) ->
    drop_inproc_updates(DraftResultSets, [Result|ResultSets]);
drop_inproc_updates([], ResultSets) ->
    lists:reverse(ResultSets).

set_env([{Key, NewValue, _OldValue}|T], State) ->
    State2 = set_env(Key, NewValue, State),
    set_env(T, State2);
set_env([], State) ->
    State.

set_env(packet_size = Key, NewValue, #sybclient{env=Env1} = State) ->
    Value = list_to_integer(binary_to_list(NewValue)), %%TODO
    Env2 = lists:keystore(Key, 1, Env1, {Key, Value}), 
    State#sybclient{env = Env2};
set_env(Key, NewValue, #sybclient{env=Env1} = State) ->
    Env2 = lists:keystore(Key, 1, Env1, {Key, NewValue}),
    State#sybclient{env = Env2}.

take_tokens(TokenName, TokensBufer, Count) ->
    take_tokens(TokenName, TokensBufer, Count, []).

take_tokens(TokenName, TokensBufer, Count, Result) when Count > 0 ->
    case take_token(TokenName, TokensBufer) of
        {TokenTuple, TokensBufer2} ->
            take_tokens(TokenName, TokensBufer2, Count-1, [TokenTuple|Result]);
        false ->
            {lists:reverse(Result), TokensBufer}
    end;
take_tokens(_TokenName, TokensBufer, _Count, Result) ->
    {lists:reverse(Result), TokensBufer}.

take_token(TokenName, TokensBufer) ->
    case lists:keytake(TokenName, 1, TokensBufer) of
        {value, TokenTuple, TokensBufer2} ->
            {TokenTuple, TokensBufer2};
        false -> false
    end.

take_token_value(Name, TokensBufer, Default) ->
    case take_token(Name, TokensBufer) of
        {{Name, Value}, TokensBufer2} ->
            {Value, TokensBufer2};
        false ->
            {Default, TokensBufer}
    end.

send(State, _PacketType, <<>>) ->
    {ok, State};
send(State, PacketType, Data) ->
    #sybclient{socket=Socket, packet_size=PacketSize} = State,
    {Packet, RestData} = ?ENCODER:encode_packet(PacketType, PacketSize, Data),
    case gen_tcp:send(Socket, Packet) of
        ok ->
            send(State, PacketType, RestData);
        {error, ErrorCode} ->
            State2 = State#sybclient{conn_state = disconnected},
            {error, socket, ErrorCode, State2}
    end.

recv(Socket, Timeout) ->
    recv(Socket, Timeout, <<>>, <<>>).

recv(Socket, Timeout, Buffer, ResultData) ->
    case ?DECODER:decode_packet(Buffer) of
        {ok, 0, PacketBody, Buffer2} ->
            ResultData2 = <<ResultData/binary, PacketBody/binary>>,
            recv(Socket, Timeout, Buffer2, ResultData2);
        {ok, 1, PacketBody, <<>>} ->
            {ok, <<ResultData/binary, PacketBody/binary>>};
        {error, incomplete_packet} ->
            case gen_tcp:recv(Socket, 0, Timeout) of
                {ok, NetworkData} ->
                    NewBuffer = <<Buffer/bits, NetworkData/bits>>,
                    recv(Socket, Timeout, NewBuffer, ResultData);
                {error, ErrorCode} ->
                    {error, socket, ErrorCode}
            end
    end.
