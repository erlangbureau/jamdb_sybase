-module(jamdb_sybase_tds_decoder).

%% API
-export([decode_packet/1]).
-export([decode_token/2]).

-include("TDS_5_0.hrl").
-include("jamdb_sybase.hrl").

%% decode_value_format
-define(IS_FIXED_LENGTH_TYPE(TDSType),
    TDSType =:= ?TDS_TYPE_INT1; 
    TDSType =:= ?TDS_TYPE_INT2; 
    TDSType =:= ?TDS_TYPE_INT4; 
    TDSType =:= ?TDS_TYPE_INT8;
    TDSType =:= ?TDS_TYPE_UINT2; 
    TDSType =:= ?TDS_TYPE_UINT4; 
    TDSType =:= ?TDS_TYPE_UINT8;
    TDSType =:= ?TDS_TYPE_FLT4;
    TDSType =:= ?TDS_TYPE_FLT8;
    TDSType =:= ?TDS_TYPE_SHORTDATE;
    TDSType =:= ?TDS_TYPE_DATETIME;
    TDSType =:= ?TDS_TYPE_DATE;
    TDSType =:= ?TDS_TYPE_TIME;
    TDSType =:= ?TDS_TYPE_BIT;
    TDSType =:= ?TDS_TYPE_SHORTMONEY;
    TDSType =:= ?TDS_TYPE_MONEY;
    TDSType =:= ?TDS_TYPE_VOID
).

-define(IS_VARIABLE_LENGTH_TYPE(TDSType),
    TDSType =:= ?TDS_TYPE_INTN;
    TDSType =:= ?TDS_TYPE_UINTN;
    TDSType =:= ?TDS_TYPE_FLTN;
    TDSType =:= ?TDS_TYPE_CHAR;
    TDSType =:= ?TDS_TYPE_VARCHAR;
    TDSType =:= ?TDS_TYPE_BINARY;
    TDSType =:= ?TDS_TYPE_VARBINARY;
    TDSType =:= ?TDS_TYPE_DATETIMEN;
    TDSType =:= ?TDS_TYPE_DATEN;
    TDSType =:= ?TDS_TYPE_TIMEN;
    TDSType =:= ?TDS_TYPE_MONEYN
).

-define(IS_DECIMAL_TYPE(TDSType),
    TDSType =:= ?TDS_TYPE_NUMN;
    TDSType =:= ?TDS_TYPE_DECN
).

-define(IS_BLOB_TYPE(TDSType), 
    TDSType =:= ?TDS_TYPE_TEXT;
    TDSType =:= ?TDS_TYPE_IMAGE
).

%% decode_value
-define(IS_ERL_BINARY(UserType),
    UserType =:= ?USER_TYPE_CHAR;
    UserType =:= ?USER_TYPE_NCHAR;
    UserType =:= ?USER_TYPE_VARCHAR;
    UserType =:= ?USER_TYPE_NVARCHAR;
    UserType =:= ?USER_TYPE_BINARY;
    UserType =:= ?USER_TYPE_VARBINARY;
    UserType =:= ?USER_TYPE_TEXT
).

-define(IS_ERL_INTEGER(UserType),
    UserType =:= ?USER_TYPE_SMALLINT;
    UserType =:= ?USER_TYPE_INT;
    UserType =:= ?USER_TYPE_BIGINT;
    UserType =:= ?USER_TYPE_INTN
).

-define(IS_ERL_UNSIGNED_INTEGER(UserType),
    UserType =:= ?USER_TYPE_TINYINT;
    UserType =:= ?USER_TYPE_USMALLINT;
    UserType =:= ?USER_TYPE_UINT;
    UserType =:= ?USER_TYPE_UBIGINT;
    UserType =:= ?USER_TYPE_UNSIGNED_SHORT;
    UserType =:= ?USER_TYPE_UNSIGNED_INT;
    UserType =:= ?USER_TYPE_UNSIGNED_LONG
).

-define(IS_ERL_FLOAT(UserType),
    UserType =:= ?USER_TYPE_FLOAT;
    UserType =:= ?USER_TYPE_REAL
).

-define(IS_ERL_NUMERIC(UserType),
    UserType =:= ?USER_TYPE_NUMERIC;
    UserType =:= ?USER_TYPE_NUMERICN
).

-define(IS_ERL_DECIMAL(UserType),
    UserType =:= ?USER_TYPE_DECIMAL;
    UserType =:= ?USER_TYPE_DECIMALN
).

-define(IS_ERL_MONEY(UserType),
    UserType =:= ?USER_TYPE_SMALLMONEY;
    UserType =:= ?USER_TYPE_MONEY
).

%% API
decode_packet(<<?TDS_PKT_RESPONSE, Status, PacketSize:16, 0:32, Rest/bits>>) ->
    BodySize = PacketSize-8,
    case Rest of
        <<PacketBody:BodySize/binary, Rest2/bits>> ->
            {ok, Status, PacketBody, Rest2};
        _ ->
            {error, incomplete_packet}
    end;
decode_packet(_) ->
    {error, incomplete_packet}.

decode_token(<<Token, Data/binary>>, TokensBufer) ->
    case Token of
        ?TDS_TOKEN_ROW ->
            {rowformat, Format} = lists:keyfind(rowformat, 1, TokensBufer),
            decode_row_token(Data, Format);
        ?TDS_TOKEN_PARAMS ->
            {paramsformat, Format} = lists:keyfind(paramsformat,1, TokensBufer),
            decode_params_token(Data, Format);
        ?TDS_TOKEN_ROWFMT ->        decode_format_token(rowformat, Data);
        ?TDS_TOKEN_PARAMFMT ->      decode_format_token(paramsformat, Data);
        ?TDS_TOKEN_ORDERBY ->       decode_orderby_token(1, Data);
        ?TDS_TOKEN_ORDERBY2 ->      decode_orderby_token(2, Data);
        ?TDS_TOKEN_DONE ->          decode_done_token(Data);
        ?TDS_TOKEN_DONEINPROC ->    decode_done_token(Data);
        ?TDS_TOKEN_DONEPROC ->      decode_done_token(Data);
        ?TDS_TOKEN_LOGINACK ->      decode_loginack_token(Data);
        ?TDS_TOKEN_CAPABILITY ->    decode_capability_token(Data);
        ?TDS_TOKEN_EED ->           decode_message_token(Data);
        ?TDS_TOKEN_ENVCHANGE ->     decode_envchange_token(Data);
        ?TDS_TOKEN_CONTROL ->       decode_control_token(Data);
        ?TDS_TOKEN_RETURNVALUE ->   decode_returnvalue_token(Data);
        ?TDS_TOKEN_RETURNSTATUS ->  decode_returnstatus_token(Data);
        _ ->
            io:format("Unknown Token: ~p Data: ~p~n", [Token, Data]),
            {error, unknown_token, Token}
    end.

%% internal
decode_loginack_token(<<_Len:16, Status, TdsVersion:4/binary, SrvNameLen, 
        SrvName:SrvNameLen/binary, SrvVersion:4/binary, Rest/binary>>) ->
    ConnState = case Status of
        ?TDS_LOG_SUCCEED    -> connected;
        ?TDS_LOG_NEGOTIATE  -> auth_negotiate;
        ?TDS_LOG_FAIL       -> disconnected
    end,
    {{loginack, ConnState, TdsVersion, {SrvName, SrvVersion}}, Rest}.

decode_capability_token(<<_Len:16, 1, ReqLen, Req:ReqLen/binary, 
        2, RespLen, Resp:RespLen/binary, Rest/binary>>) ->
    {{capability, [Req], [Resp]}, Rest}.

decode_message_token(<<_Len:16, MsgNumber:32, MsgState, Class, SQLStateLen, 
        SQLState:SQLStateLen/binary, Status, TransactionState:16,
        MsgLen:16, MsgBody:MsgLen/binary,
        ServerNameLength, ServerName:ServerNameLength/binary,
        ProcedureNameLength, ProcedureName:ProcedureNameLength/binary,
        LineNumber:16, Rest/binary>>) ->
    Msg = #message{
        msg_number          = MsgNumber,
        msg_state           = MsgState,
        class               = Class,
        sql_state           = SQLState,
        status              = Status,
        transaction_state   = TransactionState,
        msg_body            = MsgBody,
        server_name         = ServerName,
        procedure_name      = ProcedureName,
        line_number         = LineNumber
    },
    {Msg, Rest}.

envtype_to_atom(?ENV_DB)         -> database;
envtype_to_atom(?ENV_LANG)       -> language;
envtype_to_atom(?ENV_CHARSET)    -> charset;
envtype_to_atom(?ENV_PACKETSIZE) -> packet_size.

decode_envchange_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    EnvChange = [
        {envtype_to_atom(Type), NewValue, OldValue}
        || <<Type, NVLen, NewValue:NVLen/binary, 
                OVLen, OldValue:OVLen/binary>> <= TokenData
    ],
    {{envchange, EnvChange}, Rest}.

decode_control_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    Fmts = [ Fmt || <<Length, Fmt:Length/binary>> <= TokenData],
    {{control, lists:reverse(Fmts)}, Rest}.

decode_done_token(<<Status:2/binary, TransState:16, AffectedRows:32, Rest/binary>>) ->
    <<_:9, Evnt:1, Attn:1, Cnt:1, Proc:1, InXAct:1, Err:1, More:1>> = Status,
    StatusFlags = [ 
        {event, Evnt}, 
        {attn, Attn}, 
        {count, Cnt}, 
        {proc, Proc},
        {trans, InXAct}, 
        {error, Err}, 
        {more, More}
    ],
    Flags = [K || {K, V} <- StatusFlags, V =/= 0],
    {{done, Flags, TransState, AffectedRows}, Rest}.

%decode_status(StatusFlags, StatusBits) ->
%    FlagsCount = length(StatusFlags),
%    decode_status(StatusFlags, StatusBits, FlagsCount, Acc).

%decode_status([CurrentFlag|RestFlags], StatusBits, FlagsCount, Acc) ->
%    <<_:FlagsCount, CurrentBit:1, RestBits/bits>> = StatusBits,
%    decode_status(RestFlags, RestBits, FlagsCount+1, [{CurrentFlag, CurrentBit}|Acc]).

decode_orderby_token(1, <<Columns:16, Rest/binary>>) ->
    {Order, Rest2} = decode_orderby_sequence(Rest, 8, Columns, []),
    {{orderby, Order}, Rest2};
decode_orderby_token(2, <<_Len:32, Columns:16, Rest/binary>>) ->
    {Order, Rest2} = decode_orderby_sequence(Rest, 16, Columns, []),
    {{orderby, Order}, Rest2}.

decode_orderby_sequence(Rest, _Size, 0, Sequence) ->
    {lists:reverse(Sequence), Rest};
decode_orderby_sequence(Data, Size, Count, Sequence) ->
    <<Column:Size, Rest/binary>> = Data,
    decode_orderby_sequence(Rest, Size, Count-1, [Column|Sequence]).

decode_returnstatus_token(<<Status:32/signed, Rest/binary>>) ->
    {{returnstatus, Status}, Rest}.

decode_returnvalue_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    {Format, RestData} = decode_data_format(TokenData),
    {Value, <<>>}  = decode_data(Format, RestData),
    {{returnvalue, Value}, Rest}.

decode_format_token(TokenType, <<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    <<_ColsNumber:16, TokenRest/binary>> = TokenData,
    RowFormat = decode_data_format(TokenRest, []),
    {{TokenType, RowFormat}, Rest}.

decode_row_token(TokenData, RowFormat) ->
    {Rows, Rest} = decode_row(TokenData, RowFormat),
    {{row, Rows}, Rest}.

decode_params_token(TokenData, ParamsFormat) ->
    {Rows, Rest} = decode_row(TokenData, ParamsFormat),
    {{params, Rows}, Rest}.

%% TODO status byte
decode_data_format(<<>>, RowFormat) ->
    lists:reverse(RowFormat);
decode_data_format(Data, RowFormat) ->
    {ValueFormat, Rest} = decode_data_format(Data),
    <<LocaleLength, Locale:LocaleLength/binary, Rest2/binary>> = Rest,
    ValueFormat2 = ValueFormat#format{locale = Locale},
    decode_data_format(Rest2, [ValueFormat2|RowFormat]).

decode_data_format(<<LNLen, LabelName:LNLen/binary,
            Status, UserType:32/signed, TdsType, Rest/binary>>) ->
    if
        ?IS_FIXED_LENGTH_TYPE(TdsType) ->
            Format = #format{
                format      = fixed,
                label_name  = LabelName,
                status      = Status,
                usertype    = UserType, 
                tdstype     = TdsType},
            {Format, Rest};
        ?IS_VARIABLE_LENGTH_TYPE(TdsType) ->
            <<_DataLen, Rest2/binary>> = Rest,
            Format = #format{
                format      = variable,
                label_name  = LabelName,
                status      = Status,
                usertype    = UserType, 
                tdstype     = TdsType},
            {Format, Rest2};
        ?IS_DECIMAL_TYPE(TdsType) ->
            <<_DataLen, _Precision, Scale, Rest2/binary>> = Rest,
            Format = #format{
                format      = decimal,
                label_name  = LabelName,
                status      = Status,
                usertype    = UserType,
                tdstype     = TdsType,
                scale       = Scale},
            {Format, Rest2};
        ?IS_BLOB_TYPE(TdsType) ->
            <<_DataLen, ONLen:16, ObjName:ONLen/binary, Rest2/binary>> = Rest,
            Format = #format{
                format      = blob,
                label_name  = LabelName,
                obj_name    = ObjName,
                status      = Status,
                usertype    = UserType,
                tdstype     = TdsType},
            {Format, Rest2}
    end.

decode_row(Data, RowFormat) ->
    decode_row(Data, RowFormat, []).

decode_row(Data, [ValueFormat|RestRowFormat], Values) ->
    {Value, RestData} = decode_data(Data, ValueFormat),
    decode_row(RestData, RestRowFormat, [Value|Values]);
decode_row(Data, [], Values) ->
    {lists:reverse(Values), Data}.

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

decode_data(Data, #format{format=fixed, tdstype=TdsType, usertype=UserType}) ->
    Length = get_data_length(TdsType),
    <<BinValue:Length/binary, Rest/binary>> = Data,
    Value = decode_value(BinValue, UserType),
    {Value, Rest};
decode_data(Data, #format{format=variable, usertype=UserType}) ->
    <<Length, BinValue:Length/binary, Rest/binary>> = Data,
    Value = decode_value(BinValue, UserType),
    {Value, Rest};
decode_data(Data, #format{format=decimal, usertype=UserType, scale=Scale}) ->
    <<Length, BinValue:Length/binary, Rest/binary>> = Data,
    Value = decode_value(BinValue, UserType, Scale),
    {Value, Rest};
decode_data(Data, #format{format=blob, usertype=UserType}) ->
    case Data of
        <<0, Rest/binary>> ->
            Value = decode_value(<<>>, UserType),
            {Value, Rest};
        <<TPLen, _TxtPtr:TPLen/binary, _TimeStamp:64, Rest1/binary>> ->
            <<Length:32, BinValue:Length/binary, Rest2/binary>> = Rest1,
            Value = decode_value(BinValue, UserType),
            {Value, Rest2}
    end.

decode_value(<<>>, _UserType, _Scale) ->
    undefined; %% null
decode_value(BinValue, UserType, _Scale) 
        when ?IS_ERL_INTEGER(UserType) ->
    BitLength = bit_size(BinValue) - 8,
    case BinValue of
        <<0:8,Value:BitLength>> ->
            Value;
        <<1:8,Value:BitLength>> ->
            -Value
    end;
decode_value(BinValue, UserType, _Scale) 
        when ?IS_ERL_UNSIGNED_INTEGER(UserType) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength>> = BinValue,
    Value;
decode_value(BinValue, UserType, Scale) 
        when ?IS_ERL_DECIMAL(UserType) ->
    BitLength = bit_size(BinValue) - 8,
    case BinValue of
        <<0:8,Value:BitLength>> ->
            {decimal, Value, Scale};
        <<1:8,Value:BitLength>> ->
            {decimal, -Value, Scale}
    end;
decode_value(BinValue, UserType, Scale) 
        when ?IS_ERL_NUMERIC(UserType) ->
    BitLength = bit_size(BinValue) - 8,
    case BinValue of
        <<0:8,Value:BitLength>> ->
            {numeric, Value, Scale};
        <<1:8,Value:BitLength>> ->
            {numeric, -Value, Scale}
    end.

decode_value(<<>>, _UserType) ->
    undefined; %% null
decode_value(BinValue, UserType) when ?IS_ERL_BINARY(UserType) ->
    BinValue;
decode_value(BinValue, UserType) when ?IS_ERL_UNSIGNED_INTEGER(UserType) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/unsigned>> = BinValue,
    Value;
decode_value(BinValue, UserType) when ?IS_ERL_INTEGER(UserType) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/signed>> = BinValue,
    Value;
decode_value(BinValue, UserType) when ?IS_ERL_FLOAT(UserType) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/float>> = BinValue,
    Value;
decode_value(<<DaysSince1900:32>>, ?USER_TYPE_DATE) ->
    {date, calendar:gregorian_days_to_date(693961 + DaysSince1900)};
decode_value(<<Seconds:32>>, ?USER_TYPE_TIME) ->
    {time, calendar:seconds_to_time(Seconds div 300)};
decode_value(<<DaysSince1900:32, Seconds:32>>, ?USER_TYPE_DATETIME) ->
    Date = calendar:gregorian_days_to_date(693961 + DaysSince1900),
    Time = calendar:seconds_to_time(Seconds div 300),
    {Date, Time};
decode_value(<<DaysSince1900:32, _/binary>>, ?USER_TYPE_LONGDATE) ->
    {date, calendar:gregorian_days_to_date(693961 + DaysSince1900)};
decode_value(<<_:4/binary, Seconds:32>>, ?USER_TYPE_LONGTIME) ->
    {time, calendar:seconds_to_time(Seconds div 300)};
decode_value(<<DaysSince1900:16, Seconds:16>>, ?USER_TYPE_SMALLDATETIME) ->
    Date = calendar:gregorian_days_to_date(693961 + DaysSince1900),
    Time = calendar:seconds_to_time(Seconds * 60),
    {Date, Time};
decode_value(BinValue, _UnknownUserType) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/signed>> = BinValue,
    Value.
