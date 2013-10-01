-module(jamdb_sybase).

%% API
-export([connect/5]).
-export([close/1]).
%-export([prepare/3]).
%-export([unprepare/2]).
%-export([execute/3]).
-export([sql_query/2]).

-include("TDS_5_0.hrl").
-include("login.hrl").

-define(DEF_PKT_SIZE, 512).
-define(DEF_CHARSET, <<"">>).
-define(DEF_LANG, <<"us_english">>).
-define(DEF_DATABASE, <<"">>).

-record(sybase_client, {
	socket = undefined,
    state = disconnected :: disconnected | connected | auth_negotiate,
	binary_buffer = <<>> :: binary(),
	tokens_buffer = [],
	recv_timeout = 5000 :: timeout(),
    tds_ver,
    srv_name,
    srv_version,
    env = [
        {packet_size, ?DEF_PKT_SIZE},
        {language, ?DEF_LANG},
        {charset, ?DEF_CHARSET},
        {database, ?DEF_DATABASE}
    ]
}).

-record(rowformat, {
    column_name,
    obj_name,
    format :: fixed | variable | blob | decimal,
    status,
    erlangtype,
    tdstype,
    scale,
    locale
}).

-opaque state() :: #sybase_client{}.
-export_type([state/0]).

-spec connect(string(), inet:port_number(), string(), string(), string())
    -> {ok, state()} | {error, binary()}.
connect(Host, Port, User, Password, Database) ->
    GenTcpOpts = [binary, {active, false}, {packet, raw}], 
    {ok, Socket} = gen_tcp:connect(Host, Port, GenTcpOpts),
    State = #sybase_client{socket=Socket},
    {ok, State2} = send_auth_req(User, Password, "", State),
    case handle_resp(State2) of
        {ok, _, _State3 = #sybase_client{state = auth_negotiate}} ->
            %%TODO Negotiate
            {error, <<"Auth Negotiate">>};
        {ok, _, State3} ->
            {ok, _, State4} = sql_query(["use ", Database], State3),
            {ok, State4}
    end.

-spec sql_query(string(), state()) -> {ok, integer(), state()} | 
    {result, [list()], state()} | {error, binary()}.
sql_query(Query, State = #sybase_client{state = connected}) ->
    {ok, State2} = send_query_req(Query, State),
    handle_resp(State2).

-spec close(state()) -> {ok, state()}.
close(State = #sybase_client{state = connected, socket=Socket}) ->
    {ok, State2} = send(?QUERRY_PKT, <<?TOKEN_LOGOUT, 0>>, State),
    {ok, _, State3} = handle_resp(State2),
    ok = gen_tcp:close(Socket),
    {ok, State3#sybase_client{socket=undefined, state = disconnected}}.

%% internal
send_auth_req(User, Password, Charset, State) ->
    {ok, ClientHostName} = inet:gethostname(),
    ClientPID = lists:flatten(io_lib:format("~p",[self()])),
    Data = ?LOGIN_RECORD(ClientHostName, ClientPID, User, Pass, Charset),
    send(?LOGIN_PKT, Data, State).

encode_remote_password_array(Password) ->
    SrvNameLen = 0, %% mean universal password
    PasswordLen = byte_size(Password),
    Data = <<SrvNameLen:8, PasswordLen:8, Password:PasswordLen/binary>>,
    encode_char_field(Data, 255).

encode_char_field(Data, FieldLength) ->
    ActualLength = byte_size(Data),
    EmptyLength = (FieldLength - ActualLength) * 8, 
    <<Data:ActualLength/binary, 0:EmptyLength, ActualLength>>.

send_query_req(Query, State) ->
    BinaryQuery = unicode:characters_to_binary(Query),   %% TODO charset
    QueryLength = byte_size(BinaryQuery),
    Data = <<?TOKEN_LANGUAGE, (QueryLength+1):32, 0, BinaryQuery/binary>>,
    send(?QUERRY_PKT, Data, State).

handle_resp(State) ->
    {ok, BinaryData, State2} = recv(State),
    handle_resp(State2, BinaryData, []).

handle_resp(State = #sybase_client{tokens_buffer=TokensBufer}, Data, ResultSets) ->
    case parse_token(Data, State) of
        {ok, {done_token, StatusFlags, _, AffectedRows}, State2, RestData} ->
            TokensBufer2 = lists:reverse(TokensBufer),
            case result(StatusFlags, AffectedRows, TokensBufer2, ResultSets) of
                {more, TokensBufer3, ResultSets2}  ->
                    TokensBufer4 = lists:reverse(TokensBufer3),
                    State3 = State2#sybase_client{tokens_buffer=TokensBufer4},
                    handle_resp(State3, RestData, ResultSets2);
                Result ->
                    State3 = State2#sybase_client{tokens_buffer=[]},
                    erlang:append_element(Result, State3)
            end;
        {ok, TokenTuple, State2, RestData} ->
            State3 = State2#sybase_client{tokens_buffer=[TokenTuple|TokensBufer]},
            handle_resp(State3, RestData, ResultSets);
        {ok, State2, RestData} ->
            handle_resp(State2, RestData, ResultSets)
    end.

result([more|_], _AffectedRows, TokensBufer, ResultSets) ->
    {more, TokensBufer, ResultSets};
result([count|RestFlags], AffectedRows, TokensBufer, ResultSets) ->
    case take_token(rowfmt_token, TokensBufer) of
        {{rowfmt_token, _, RowsFmt}, TokensBufer2} ->
            FieldNames = [Fmt#rowformat.column_name || Fmt <- RowsFmt],
            {TokensList, TokensBufer3} = 
                take_tokens(row_token, TokensBufer2, AffectedRows),
            Rows = [Row || {row_token, Row} <- TokensList],
            ResultSets2 = [{result_set, FieldNames, Rows}|ResultSets],
            result(RestFlags, AffectedRows, TokensBufer3, ResultSets2);
        false ->
            result(RestFlags, AffectedRows, TokensBufer, [])
    end;
result([], AffectedRows, _TokensBufer, []) ->
    {ok, AffectedRows};
result([], _AffectedRows, _TokensBufer, ResultSets) ->
    {result, lists:reverse(ResultSets)};
result([error|_RestFlags], _AffectedRows, TokensBufer, _ResultSets) ->
    {{eed_token, ErrorData}, _} = take_token(eed_token, TokensBufer),
    {error, ErrorData}.

parse_packet_header(<<?RESPONSE_PKT, Status, PacketSize:16, 0:32, Rest/bits>>) ->
    {ok, Status, <<(PacketSize-8):16, Rest/binary>>};
parse_packet_header(_) ->
    {error, incorect_packet_header}.

parse_token(<<?TOKEN_EED, Len:16, TokenData:Len/binary, Rest/binary>>, State) ->
    parse_eed_token(TokenData, Rest, State);
parse_token(<<?TOKEN_ENVCHANGE, Len:16, TokenData:Len/binary, Rest/binary>>, State) ->
    parse_envchange_token(TokenData, Rest, State);
parse_token(<<?TOKEN_CAPABILITY, Len:16, TokenData:Len/binary, Rest/binary>>, State) ->
    parse_capability_token(TokenData, Rest, State);
parse_token(<<?TOKEN_LOGINACK, Len:16, TokenData:Len/binary, Rest/binary>>, State) ->
    parse_loginack_token(TokenData, Rest, State);
parse_token(<<?TOKEN_ROWFMT, Len:16, TokenData:Len/binary, Rest/binary>>, State) ->
    parse_rowformat_token(TokenData, Rest, State);
parse_token(<<?TOKEN_PARAMFMT, Len:16, TokenData:Len/binary, Rest/binary>>, State) ->
    parse_rowformat_token(TokenData, Rest, State); %% TODO ?
parse_token(<<?TOKEN_CONTROL, Len:16, TokenData:Len/binary, Rest/binary>>, State) ->
    parse_control_token(TokenData, Rest, State);
parse_token(<<?TOKEN_ROW, Rest/binary>>, State) ->
    parse_row_token(Rest, State);
parse_token(<<?TOKEN_PARAMS, Rest/binary>>, State) ->
    parse_row_token(Rest, State);   %% TODO ?
parse_token(<<?TOKEN_RETURNSTATUS, TokenData:4/binary, Rest/binary>>, State) ->
    parse_returnstatus_token(TokenData, Rest, State);
parse_token(<<TokenId, TokenData:8/binary, Rest/binary>>, State) when 
        TokenId =:= ?TOKEN_DONE; 
        TokenId =:= ?TOKEN_DONEINPROC; 
        TokenId =:= ?TOKEN_DONEPROC ->
    parse_done_token(TokenData, Rest, State);
parse_token(<<Token, _Rest/binary>>, _State) ->
    %io:format("Unknown Token: ~p RestData: ~p~n", [Token, _Rest]),
    {error, unknown_token, Token}.

parse_capability_token(<<1, ReqLen, _Req:ReqLen/binary, 
        2, RespLen, _Resp:RespLen/binary, Rest1/binary>>, Rest2, State) ->
    %io:format("ReqCapability: ~p~n", [_Req]),
    %io:format("RespCapability: ~p~n", [_Resp]),
    Rest = <<Rest1/binary, Rest2/binary>>,  %% Sybase 11.0 and higer return 
                                            %% the wrong token length 
                                            %% in the capability packet
    {ok, {capability_token, ok}, State, Rest}.

parse_loginack_token(<<Status, TdsVersion:4/binary, SrvNameLen, 
        SrvName:SrvNameLen/binary, SrvVersion:4/binary>>, Rest, State) ->
    State2 = case Status of
        ?LOG_SUCCEED -> State#sybase_client{state = connected}; 
        ?LOG_NEGOTIATE -> State#sybase_client{state = auth_negotiate};
        ?LOG_FAIL -> State
    end,
    State3 = State2#sybase_client{tds_ver = TdsVersion,
                srv_name = SrvName, srv_version = SrvVersion},
    {ok, State3, Rest}.

parse_done_token(<<Status:16, TransState:16, AffectedRows:32>>, Rest, State) ->
    StatusFlags = [
        %{event, (Status band ?DONE_EVENT)  =/= 0},
        %{attn,  (Status band ?DONE_ATTN)   =/= 0},
        {count, (Status band ?DONE_COUNT)  =/= 0},
        %{proc,  (Status band ?DONE_PROC)   =/= 0},
        %{trans, (Status band ?DONE_INXACT) =/= 0},
        {error, (Status band ?DONE_ERROR)  =/= 0},
        {more,  (Status band ?DONE_MORE)   =/= 0}
    ],
    Flags = [K || {K, V} <- StatusFlags, V],
    {ok, {done_token, Flags, TransState, AffectedRows}, State, Rest}.

parse_returnstatus_token(<<Value:32/signed>>, Rest, State) ->
    {ok, {returnstatus_token, Value}, State, Rest}.

parse_control_token(TokenData, Rest, State) ->
    Fmts = parse_control_fmt(TokenData, []),
    {ok, {control_token, Fmts}, State, Rest}.

parse_control_fmt(<<Length, Fmt:Length/binary, Rest/binary>>, Fmts) ->
    parse_control_fmt(Rest, [Fmt|Fmts]);
parse_control_fmt(<<>>, Fmts) ->
    lists:reverse(Fmts).

parse_row_token(TokenData, #sybase_client{tokens_buffer=TokensBufer} = State) ->
    {rowfmt_token, _ColsNum, RowsFmt} = get_token(rowfmt_token, TokensBufer),
    {Rows, Rest} = parse_row(RowsFmt, TokenData, []),
    {ok, {row_token, Rows}, State, Rest}.
    
parse_row([#rowformat{format=fixed, tdstype=TdsType, erlangtype=ErlType}
        | RowsFmt], Data, Rows) ->
    Length = get_data_length(TdsType),
    <<BinValue:Length/binary, Rest/binary>> = Data,
    Value = convert_type(BinValue, ErlType),
    parse_row(RowsFmt, Rest, [Value|Rows]);
parse_row([#rowformat{format=variable, erlangtype=ErlType}
        | RowsFmt], <<Length, BinValue:Length/binary, Rest/binary>>, Rows) ->
    Value = convert_type(BinValue, ErlType),
    parse_row(RowsFmt, Rest, [Value|Rows]);
parse_row([#rowformat{format=decimal, erlangtype=ErlType, scale=Scale}
        | RowsFmt], <<Length, BinValue:Length/binary, Rest/binary>>, Rows) ->
    Value = convert_decimal(BinValue, ErlType, Scale),
    parse_row(RowsFmt, Rest, [Value|Rows]);
parse_row([#rowformat{format=blob, erlangtype=ErlType}
        | RowsFmt], Data, Rows) ->
    case Data of
        <<0, Rest/binary>> ->
            Value = convert_type(<<>>, ErlType),
            parse_row(RowsFmt, Rest, [Value|Rows]);
        <<TxtPtrLen, _TxtPtr:TxtPtrLen/binary, _TimeStamp:64,
                Length:32, BinValue:Length/binary, Rest/binary>> ->
            Value = convert_type(BinValue, ErlType),
            parse_row(RowsFmt, Rest, [Value|Rows])
    end;
parse_row([], Rest, Rows) ->
    {lists:reverse(Rows), Rest}.

parse_rowformat_token(<<ColsNumber:16, TokenRest/binary>>, Rest, State) ->
    RowsFmt = parse_rowformat(TokenRest, []),
    {ok, {rowfmt_token, ColsNumber, RowsFmt}, State, Rest}.

parse_rowformat(<<ColumnNameLength, ColumnName:ColumnNameLength/binary, Status,
        UserType:32/signed, Type, LocaleLength, Locale:LocaleLength/binary, 
        Rest/binary>>, RowsFmt) when ?IS_FIXED_LENGTH_TYPE(Type) ->
    RowFormat = #rowformat{
            column_name=ColumnName, 
            format=fixed,
            status=Status,
            erlangtype=usertype_to_erlang_type(UserType), 
            tdstype=Type, 
            locale=Locale},
    parse_rowformat(Rest, [RowFormat|RowsFmt]);
parse_rowformat(<<ColumnNameLength, ColumnName:ColumnNameLength/binary, Status,
        UserType:32/signed, Type, _DataLength, LocaleLength, 
        Locale:LocaleLength/binary, 
        Rest/binary>>, RowsFmt) when ?IS_VARIABLE_LENGTH_TYPE(Type) ->
    RowFormat = #rowformat{
            column_name=ColumnName, 
            format=variable,
            status=Status,
            erlangtype=usertype_to_erlang_type(UserType), 
            tdstype=Type, 
            locale=Locale},
    parse_rowformat(Rest, [RowFormat|RowsFmt]);
parse_rowformat(<<ColumnNameLength, ColumnName:ColumnNameLength/binary, Status,
        UserType:32/signed, Type, _DataLength, _Precision, Scale,
        LocaleLength, Locale:LocaleLength/binary,
        Rest/binary>>, RowsFmt) when ?IS_DECIMAL_TYPE(Type) ->
    RowFormat = #rowformat{
            column_name=ColumnName, 
            format=decimal,
            status=Status,
            erlangtype=usertype_to_erlang_type(UserType),
            tdstype=Type, 
            scale=Scale,
            locale=Locale},
    parse_rowformat(Rest, [RowFormat|RowsFmt]);
parse_rowformat(<<ColumnNameLength, ColumnName:ColumnNameLength/binary, Status,
        UserType:32/signed, Type, _DataLength:32, ObjNameLen:16,
        ObjName:ObjNameLen/binary, LocaleLength, Locale:LocaleLength/binary,
        Rest/binary>>, RowsFmt) when ?IS_BLOB_TYPE(Type) ->
    RowFormat = #rowformat{
            column_name=ColumnName,
            obj_name = ObjName,
            format=blob,
            status=Status,
            erlangtype=usertype_to_erlang_type(UserType),
            tdstype=Type, 
            locale=Locale},
    parse_rowformat(Rest, [RowFormat|RowsFmt]);
parse_rowformat(<<>>, RowsFmt) ->
    lists:reverse(RowsFmt).

parse_eed_token(<<MsgNumber:32, MsgState, Class, SQLStateLen, 
        SQLState:SQLStateLen/binary, Status, TransactionState:16,
        MsgLen:16, Msg:MsgLen/binary,
        ServerNameLength, ServerName:ServerNameLength/binary,
        ProcedureNameLength, ProcedureName:ProcedureNameLength/binary,
        LineNumber:16>>, Rest, State) ->
    ErrorData = {error_data, MsgNumber, MsgState, Class, SQLState, Status, 
                TransactionState, Msg, ServerName, ProcedureName, LineNumber},
    {ok, {eed_token, ErrorData}, State, Rest}.

parse_envchange_token(TokenData, Rest, State) ->
    State2 = envchange(TokenData, State),
    {ok, State2, Rest}.

envchange(<<Type, NewValLen, NewValue:NewValLen/binary, 
        OldValLen, OldValue:OldValLen/binary, RestToken/binary>>, State) ->
    State2 = case Type of
        ?ENV_DB -> set_env(database, OldValue, NewValue, State);
        ?ENV_LANG -> set_env(language, OldValue, NewValue, State);
        ?ENV_CHARSET -> set_env(charset, OldValue, NewValue, State);
        ?ENV_PACKETSIZE -> set_env(packet_size, OldValue, NewValue, State)
    end,
    envchange(RestToken, State2);
envchange(<<>>, State) ->
    State.

set_env(packet_size = Key, _OldValue, NewValue, 
        #sybase_client{env=Env1} = State) ->
    Value = list_to_integer(binary_to_list(NewValue)),
    Env2 = lists:keystore(Key, 1, Env1, {Key, Value}), 
    State#sybase_client{env = Env2};
set_env(Key, _OldValue, NewValue, 
        #sybase_client{env=Env1} = State) ->
    Env2 = lists:keystore(Key, 1, Env1, {Key, NewValue}),
    State#sybase_client{env = Env2}.

get_env(Key, #sybase_client{env = Env}) ->
    proplists:get_value(Key, Env).

take_tokens(TokenName, TokensBufer, Count) ->
    take_tokens(TokenName, TokensBufer, Count, []).

take_tokens(TokenName, TokensBufer, Count, Result) when Count > 0 ->
    case take_token(TokenName, TokensBufer) of
        {TokenTuple, TokensBufer2} ->
            take_tokens(TokenName, TokensBufer2, Count-1, [TokenTuple|Result]);
        false ->
            {Result, TokensBufer}
    end;
take_tokens(_TokenName, TokensBufer, _Count, Result) ->
    {lists:reverse(Result), TokensBufer}.

take_token(TokenName, TokensBufer) ->
    case lists:keytake(TokenName, 1, TokensBufer) of
        {value, TokenTuple, TokensBufer2} ->
            {TokenTuple, TokensBufer2};
        false -> false
    end.

get_token(TokenName, TokensBufer) ->
    lists:keyfind(TokenName, 1, TokensBufer).

get_data_length(?TDS_TYPE_UINT2) -> 2;
get_data_length(?TDS_TYPE_UINT4) -> 4;
get_data_length(?TDS_TYPE_UINT8) -> 8;
get_data_length(?TDS_TYPE_INT1) -> 1;
get_data_length(?TDS_TYPE_INT2) -> 2;
get_data_length(?TDS_TYPE_INT4) -> 4;
get_data_length(?TDS_TYPE_INT8) -> 8;
get_data_length(?TDS_TYPE_FLT4) -> 4;
get_data_length(?TDS_TYPE_FLT8) -> 8;
get_data_length(?TDS_TYPE_DATE) -> 4;
get_data_length(?TDS_TYPE_TIME) -> 4;
get_data_length(?TDS_TYPE_SHORTDATE) -> 4;
get_data_length(?TDS_TYPE_DATETIME) -> 8.

usertype_to_erlang_type(?USER_TYPE_CHAR) -> binary;
usertype_to_erlang_type(?USER_TYPE_NCHAR) -> binary;
usertype_to_erlang_type(?USER_TYPE_VARCHAR) -> binary;
usertype_to_erlang_type(?USER_TYPE_NVARCHAR) -> binary;
usertype_to_erlang_type(?USER_TYPE_BINARY) -> binary;
usertype_to_erlang_type(?USER_TYPE_VARBINARY) -> binary;
usertype_to_erlang_type(?USER_TYPE_TEXT) -> binary;
usertype_to_erlang_type(?USER_TYPE_TINYINT) -> 'unsigned-integer'; 
usertype_to_erlang_type(?USER_TYPE_SMALLINT) -> integer; 
usertype_to_erlang_type(?USER_TYPE_INT) -> integer; 
usertype_to_erlang_type(?USER_TYPE_BIGINT) -> integer; 
usertype_to_erlang_type(?USER_TYPE_INTN) -> integer;
usertype_to_erlang_type(?USER_TYPE_USMALLINT) -> 'unsigned-integer';
usertype_to_erlang_type(?USER_TYPE_UINT) -> 'unsigned-integer';
usertype_to_erlang_type(?USER_TYPE_UBIGINT) -> 'unsigned-integer';
usertype_to_erlang_type(?USER_TYPE_UNSIGNED_SHORT) -> 'unsigned-integer';
usertype_to_erlang_type(?USER_TYPE_UNSIGNED_INT) -> 'unsigned-integer';
usertype_to_erlang_type(?USER_TYPE_UNSIGNED_LONG) -> 'unsigned-integer';
usertype_to_erlang_type(?USER_TYPE_FLOAT) -> float;
usertype_to_erlang_type(?USER_TYPE_REAL) -> float;
usertype_to_erlang_type(?USER_TYPE_NUMERIC) -> decimal;
usertype_to_erlang_type(?USER_TYPE_NUMERICN) -> decimal;
usertype_to_erlang_type(?USER_TYPE_DECIMAL) -> decimal;
usertype_to_erlang_type(?USER_TYPE_DECIMALN) -> decimal;
usertype_to_erlang_type(?USER_TYPE_DATETIME) -> datetime;
usertype_to_erlang_type(?USER_TYPE_SMALLDATETIME) -> datetime;
usertype_to_erlang_type(?USER_TYPE_LONGDATE) -> date;
usertype_to_erlang_type(?USER_TYPE_DATE) -> date;
usertype_to_erlang_type(?USER_TYPE_LONGTIME) -> time;
usertype_to_erlang_type(?USER_TYPE_TIME) -> time.
%usertype_to_erlang_type(?USER_TYPE_SMALLMONEY) -> money;
%usertype_to_erlang_type(?USER_TYPE_MONEY) -> money.

convert_decimal(<<>>, _DstType, _Scale) ->
    null;
convert_decimal(BinValue, integer, _Scale) ->
    BitLength = bit_size(BinValue) - 8,
    case BinValue of
        <<0:8,Value:BitLength>> ->
            Value;
        <<1:8,Value:BitLength>> ->
            -Value
    end;
convert_decimal(BinValue, 'unsigned-integer', _Scale) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength>> = BinValue,
    Value;
convert_decimal(BinValue, decimal, Scale) ->
    BitLength = bit_size(BinValue) - 8,
    case BinValue of
        <<0:8,Value:BitLength>> ->
            {decimal, Value, Scale};
        <<1:8,Value:BitLength>> ->
            {decimal, -Value, Scale}
    end.

convert_type(<<>>, _DstType) ->
    null; 
convert_type(BinValue, binary) ->
    BinValue;
convert_type(BinValue, 'unsigned-integer') ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/unsigned>> = BinValue,
    Value;
convert_type(BinValue, integer) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/signed>> = BinValue,
    Value;
convert_type(BinValue, float) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/float>> = BinValue,
    Value;
convert_type(<<DaysSince1900:32, _/binary>>, date) ->
    calendar:gregorian_days_to_date(693961 + DaysSince1900);
convert_type(<<_:4/binary, Seconds:32>>, time) ->
    calendar:seconds_to_time(Seconds div 300);
convert_type(<<DaysSince1900:32>>, date) ->
    calendar:gregorian_days_to_date(693961 + DaysSince1900);
convert_type(<<Seconds:32>>, time) ->
    calendar:seconds_to_time(Seconds div 300);
convert_type(<<DaysSince1900:32, Seconds:32>>, datetime) ->
    Date = calendar:gregorian_days_to_date(693961 + DaysSince1900),
    Time = calendar:seconds_to_time(Seconds div 300),
    {Date, Time};
convert_type(<<DaysSince1900:16, Seconds:16>>, datetime) ->
    Date = calendar:gregorian_days_to_date(693961 + DaysSince1900),
    Time = calendar:seconds_to_time(Seconds * 60),
    {Date, Time}.

send(PacketType, Data, State=#sybase_client{socket=Socket}) ->
    PacketSize = get_env(packet_size, State),
    DataSize = PacketSize - 8,
    case Data of
        <<PacketData:DataSize/binary, RestData/binary>> ->
            Pkt = <<PacketType:8, 0:8, PacketSize:16, 0:32, PacketData/binary>>,
            ok = gen_tcp:send(Socket, Pkt), 
            send(PacketType, RestData, State);
        _ ->
            Length = 8 + byte_size(Data),
            Pkt = <<PacketType:8, 1:8, Length:16, 0:32, Data/binary>>,
            ok = gen_tcp:send(Socket, Pkt),
            {ok, State}
    end.

recv(State) ->
    recv(State, <<>>).

recv(State=#sybase_client{socket=Socket, recv_timeout=Timeout,
        binary_buffer=Buffer1}, Data1) ->
    case parse_packet_header(Buffer1) of
        {ok, 0, <<DataSize:16, Message:DataSize/binary, Rest/bits>>} ->
            Data2 = <<Data1/binary, Message/binary>>,
            recv(State#sybase_client{binary_buffer=Rest}, Data2);
        {ok, 1, <<DataSize:16, Message:DataSize/binary, Rest/bits>>} ->
            Data2 = <<Data1/binary, Message/binary>>,
            {ok, Data2, State#sybase_client{binary_buffer=Rest}};
        _ ->
            {ok, Data} = gen_tcp:recv(Socket, 0, Timeout),
            Buffer2 = <<Buffer1/binary, Data/binary>>,
            recv(State#sybase_client{binary_buffer=Buffer2}, Data1)
    end.
