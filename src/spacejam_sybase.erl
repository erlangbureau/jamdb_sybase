-module(spacejam_sybase).

%% API
-export([connect/5]).
-export([close/1]).
%-export([prepare/3]).
%-export([unprepare/2]).
%-export([execute/3]).
-export([sql_query/2]).

-include("TDS_5_0.hrl").
-include("spacejam_sybase.hrl").

-define(PKT_SIZE, 512).
-define(MAX_PKT_SIZE, 1024).


-record(sybase_client, {
	socket = undefined,
    state = disconnected :: disconnected | connected | auth,
	binary_buffer = <<>> :: binary(),
	tokens_buffer = [],
	recv_timeout = 5000 :: timeout(),
    tds_ver,
    srv_name,
    srv_version,
    env = [
        {packet_size, ?PKT_SIZE},
        {language, <<"">>}, 
        {charset, <<"">>},
        {database, <<"">>}
    ]
}).

-record(rowformat, {
    column_name,
    obj_name,
    status,
    usertype,
    datatype,
    fixed_length = false :: boolean(),
    scale,
    precision,
    locale
}).

connect(Host, Port, User, Password, Database) ->
    GenTcpOpts = [binary, {active, false}, {packet, raw}], 
    {ok, Socket} = gen_tcp:connect(Host, Port, GenTcpOpts),
    State = #sybase_client{socket=Socket},
    {ok, State2} = send_auth_req(Host, User, Password, State),
    case handle_resp(State2) of
        {ok, _, State3 = #sybase_client{state = auth}} ->
            %%TODO Negotiate
            {error, State3};
        {ok, _, State3} ->
            {ok, _, State4} = sql_query(["use ", Database], State3),
            {ok, State4}
    end.

sql_query(Query, State = #sybase_client{state = connected}) ->
    {ok, State2} = send_query_req(Query, State),
    handle_resp(State2).

close(State = #sybase_client{state = connected, socket=Socket}) ->
    {ok, State2} = send(?QUERRY_PKT, <<?TOKEN_LOGOUT, 0>>, State),
    {ok, _, State3} = handle_resp(State2),
    ok = gen_tcp:close(Socket),
    {ok, State3#sybase_client{socket=undefined, state = disconnected}}.

%% internal
send_auth_req(ServerName, User, Password, State) ->
    {ok, ClientHostName} = inet:gethostname(),
    ClientProcessId = lists:flatten(io_lib:format("~p",[self()])),
    Data = ?LOGIN_RECORD(ClientHostName, ClientProcessId, 
                User, Pass, ServerName, "", "", ?MAX_PKT_SIZE),
    send(?LOGIN_PKT, Data, State).

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
parse_token(<<Token, Rest/binary>>, _State) ->
    io:format("Unknown Token: ~p RestData: ~p~n", [Token, Rest]),
    {error, unknown_token, Token}.

parse_capability_token(<<1, ReqLen, _Req:ReqLen/binary, 2, RespLen, _Resp:RespLen/binary, Rest1/binary>>, Rest2, State) ->
    io:format("ReqCapability: ~p~n", [_Req]),
    io:format("RespCapability: ~p~n", [_Resp]),
    Rest = <<Rest1/binary, Rest2/binary>>,  %% Sybase 11.0 and higer return 
                                            %% the wrong token length 
                                            %% in the capability packet
    {ok, {capability_token, ok}, State, Rest}.

parse_loginack_token(<<Status, TdsVersion:4/binary, SrvNameLen, 
        SrvName:SrvNameLen/binary, SrvVersion:4/binary>>, Rest, State) ->
    State2 = case Status of
        ?LOG_SUCCEED -> State#sybase_client{state = connected}; 
        ?LOG_NEGOTIATE -> State#sybase_client{state = auth};
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

parse_row_token(TokenData, State=#sybase_client{tokens_buffer=TokensBufer}) ->
    {rowfmt_token, _ColsNum, RowsFmt} = get_token(rowfmt_token, TokensBufer),
    {Rows, Rest} = parse_row(TokenData, RowsFmt, []),
    {ok, {row_token, Rows}, State, Rest}.
    
parse_row(Data, [#rowformat{datatype = TdsType, usertype = UserType, 
        fixed_length = true}|RowsFmt], Rows) ->
    Length = get_data_length(TdsType),
    ErlangType = usertype_to_erlang_type(UserType),
    case Data of
        <<BinValue:Length/binary, Rest/binary>> ->
            Value = convert_type(BinValue, ErlangType),
            parse_row(Rest, RowsFmt, [Value|Rows])
    end;
parse_row(Data, [#rowformat{datatype = TdsType, usertype = UserType, 
        fixed_length = false, 
        scale = Scale}|RowsFmt], Rows) when ?IS_NUMERIC_TYPE(TdsType) ->
    ErlangType = usertype_to_erlang_type(UserType),
    case Data of
        <<Length, BinValue:Length/binary, Rest/binary>> ->
            Value = convert_decimal(BinValue, ErlangType, Scale),
            parse_row(Rest, RowsFmt, [Value|Rows])
    end;
parse_row(Data, [#rowformat{datatype = TdsType, usertype = UserType, 
        fixed_length = false}|RowsFmt], Rows) when ?IS_TEXT_OR_IMAGE_TYPE(TdsType) ->
    ErlangType = usertype_to_erlang_type(UserType),
    case Data of
        <<0, Rest/binary>> ->
            Value = convert_type(<<>>, ErlangType),
            parse_row(Rest, RowsFmt, [Value|Rows]);
        <<TxtPtrLen, _TxtPtr:TxtPtrLen/binary, 
            _TimeStamp:64, Length:32, BinValue:Length/binary, Rest/binary>> ->
            Value = convert_type(BinValue , ErlangType),
            parse_row(Rest, RowsFmt, [Value|Rows])
    end;
parse_row(Data, [#rowformat{usertype = UserType, 
        fixed_length = false}|RowsFmt], Rows) ->
    ErlangType = usertype_to_erlang_type(UserType),
    case Data of
        <<Length, BinValue:Length/binary, Rest/binary>> ->
            Value = convert_type(BinValue, ErlangType),
            parse_row(Rest, RowsFmt, [Value|Rows])
    end;
parse_row(Rest, [], Rows) ->
    {lists:reverse(Rows), Rest}.

parse_rowformat_token(<<ColsNumber:16, TokenRest/binary>>, Rest, State) ->
    RowsFmt = parse_rowformat(TokenRest, []),
    {ok, {rowfmt_token, ColsNumber, RowsFmt}, State, Rest}.

parse_rowformat(<<ColumnNameLength, ColumnName:ColumnNameLength/binary, Status,
        UserType:32/signed, Type, LocaleLength, Locale:LocaleLength/binary, 
        Rest/binary>>, RowsFmt) when ?IS_FIXED_LENGTH_TYPE(Type) ->
    RowFormat = #rowformat{column_name=ColumnName, status=Status,
        usertype=UserType, datatype=Type, fixed_length=true, locale=Locale},
    parse_rowformat(Rest, [RowFormat|RowsFmt]);
parse_rowformat(<<ColumnNameLength, ColumnName:ColumnNameLength/binary, Status,
        UserType:32/signed, Type, _DataLength, LocaleLength, 
        Locale:LocaleLength/binary, 
        Rest/binary>>, RowsFmt) when ?IS_VAR_LENGTH_TYPE(Type) ->
    RowFormat = #rowformat{
            column_name=ColumnName, 
            status=Status,
            usertype=UserType, 
            datatype=Type, 
            locale=Locale},
    parse_rowformat(Rest, [RowFormat|RowsFmt]);
parse_rowformat(<<ColumnNameLength, ColumnName:ColumnNameLength/binary, Status,
        UserType:32/signed, Type, _DataLength, Precision, Scale,
        LocaleLength, Locale:LocaleLength/binary, 
        Rest/binary>>, RowsFmt) when ?IS_NUMERIC_TYPE(Type) ->
    RowFormat = #rowformat{
            column_name=ColumnName, 
            status=Status,
            usertype=UserType, 
            datatype=Type, 
            precision=Precision, 
            scale=Scale,
            locale=Locale},
    parse_rowformat(Rest, [RowFormat|RowsFmt]);
parse_rowformat(<<ColumnNameLength, ColumnName:ColumnNameLength/binary, Status,
        UserType:32/signed, Type, _DataLength:32, ObjNameLen:16,
        ObjName:ObjNameLen/binary, LocaleLength, Locale:LocaleLength/binary,
        Rest/binary>>, RowsFmt) when ?IS_TEXT_OR_IMAGE_TYPE(Type) ->
    RowFormat = #rowformat{
            column_name=ColumnName,
            obj_name = ObjName,
            status=Status,
            usertype=UserType, 
            datatype=Type, 
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

encode_char_field(Data, FieldLength) ->
    ActualLength = byte_size(Data),
    EmptyLength = (FieldLength - ActualLength) * 8, 
    <<Data:ActualLength/binary, 0:EmptyLength, ActualLength>>.

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

set_env(packet_size = Key, _OldValue, NewValue, State) ->
    %io:format("change ~p from:~p to:~p~n", [Key, _OldValue, NewValue]),  %%TODO
    Value = list_to_integer(binary_to_list(NewValue)),
    Env = State#sybase_client.env,
    State#sybase_client{env = lists:keystore(Key, 1, Env, {Key, Value})};
set_env(Key, _OldValue, NewValue, State) ->
    %io:format("change ~p from:~p to:~p~n", [Key, _OldValue, NewValue]),  %%TODO
    Env = State#sybase_client.env,
    State#sybase_client{env = lists:keystore(Key, 1, Env, {Key, NewValue})}.

get_env(Key, State) ->
    Env = State#sybase_client.env,
    proplists:get_value(Key, Env).

get_data_length(?TDS_TYPE_INT1) -> 1;
get_data_length(?TDS_TYPE_INT2) -> 2;
get_data_length(?TDS_TYPE_INT4) -> 4;
get_data_length(?TDS_TYPE_INT8) -> 8;
get_data_length(?TDS_TYPE_DATE) -> 4;
get_data_length(?TDS_TYPE_TIME) -> 4;
get_data_length(?TDS_TYPE_DATETIME) -> 8.

usertype_to_erlang_type(?USER_TYPE_CHAR) -> binary;
usertype_to_erlang_type(?USER_TYPE_VARCHAR) -> binary;
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
usertype_to_erlang_type(?USER_TYPE_NUMERIC) -> decimal;
usertype_to_erlang_type(?USER_TYPE_NUMERICN) -> decimal;
usertype_to_erlang_type(?USER_TYPE_DECIMAL) -> decimal;
usertype_to_erlang_type(?USER_TYPE_DECIMALN) -> decimal;
usertype_to_erlang_type(?USER_TYPE_DATETIME) -> datetime;
usertype_to_erlang_type(?USER_TYPE_DATE1) -> date;
usertype_to_erlang_type(?USER_TYPE_DATE2) -> date;
usertype_to_erlang_type(?USER_TYPE_TIME1) -> time;
usertype_to_erlang_type(?USER_TYPE_TIME2) -> time.

convert_decimal(<<>>, _DstType, _Scale) ->
    null;
convert_decimal(BinValue, decimal, Scale) ->
    BitLength = bit_size(BinValue) - 8,
    case BinValue of
        <<0:8,Value:BitLength>> ->
            {decimal, Value, Scale};
        <<1:8,Value:BitLength>> ->
            {decimal, -Value, Scale}
    end;
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
    Value.

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
convert_type(<<DaysSince1900:32>>, date) ->
    calendar:gregorian_days_to_date(693961 + DaysSince1900);
convert_type(<<Seconds:32>>, time) ->
    calendar:seconds_to_time(Seconds div 300);
convert_type(<<Date:32, Time:32>>, datetime) ->
    {convert_type(Date, date), convert_type(Time, time)}.

send(PacketType, Data, State=#sybase_client{socket=Socket}) ->
    PacketSize = get_env(packet_size, State),
    DataSize = PacketSize - 8,
    case Data of
        <<PacketData:DataSize/binary, RestData/binary>> ->
            ok = gen_tcp:send(Socket, 
                <<PacketType:8, 0:8, PacketSize:16, 0:32, PacketData/binary>>),
            send(PacketType, RestData, State);
        _ ->
            Length = 8 + byte_size(Data),
            ok = gen_tcp:send(Socket, 
                <<PacketType:8, 1:8, Length:16, 0:32, Data/binary>>),
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
