-module(jamdb_sybase_tds_decoder).

%% API
-export([decode_packet/1]).
-export([decode_token/2]).

-include("TDS_5_0.hrl").
-include("jamdb_sybase.hrl").

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
        ?TDS_TOKEN_ROW ->           decode_row_token(Data, TokensBufer);
        ?TDS_TOKEN_PARAMS ->        decode_params_token(Data, TokensBufer);
        ?TDS_TOKEN_ROWFMT ->        decode_rowformat_token(Data);
        ?TDS_TOKEN_ROWFMT2 ->       decode_rowformat2_token(Data);
        ?TDS_TOKEN_PARAMFMT ->      decode_paramsformat_token(Data);
        ?TDS_TOKEN_PARAMFMT2 ->     decode_paramsformat2_token(Data);
        ?TDS_TOKEN_ORDERBY ->       decode_orderby_token(Data);
        ?TDS_TOKEN_ORDERBY2 ->      decode_orderby2_token(Data);
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
        ?TDS_TOKEN_DYNAMIC ->       decode_dynamic_token(Data);
        ?TDS_TOKEN_DYNAMIC2 ->      decode_dynamic2_token(Data);
        _ ->
            %io:format("Unknown Token: ~p Data: ~p~n", [Token, Data]),
            {error, unknown_tds_token}
    end.

decode_dynamic_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    <<?TDS_DYN_ACK, Status, IdLen, Id:IdLen/binary>> = TokenData,
    {ok, {dynamic, ack, Status, Id}, Rest}.

decode_dynamic2_token(<<Len:32, TokenData:Len/binary, Rest/binary>>) ->
    <<?TDS_DYN_ACK, Status, IdLen, Id:IdLen/binary>> = TokenData,
    {ok, {dynamic, ack, Status, Id}, Rest}.

%% internal
decode_loginack_token(<<_Len:16, Status, TdsVersion:4/binary, SrvNameLen,
        SrvName:SrvNameLen/binary, SrvVersion:4/binary, Rest/binary>>) ->
    ConnState = case Status of
        ?TDS_LOG_SUCCEED    -> connected;
        ?TDS_LOG_NEGOTIATE  -> auth_negotiate;
        ?TDS_LOG_FAIL       -> disconnected
    end,
    TdsVer = decode_version(TdsVersion),
    SrvVer = decode_version(SrvVersion),
    {ok, {loginack, ConnState, TdsVer, {SrvName, SrvVer}}, Rest}.

decode_version(<<V1, V2, V3, V4>>) ->
    {V1, V2, V3, V4}.

decode_capability_token(<<_Len:16, 1, ReqLen, Req:ReqLen/binary, 
        2, RespLen, Resp:RespLen/binary, Rest/binary>>) ->
    {ok, {capability, [Req], [Resp]}, Rest}.

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
    {ok, Msg, Rest}.

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
    {ok, {envchange, EnvChange}, Rest}.

decode_control_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    Fmts = [ Fmt || <<Length, Fmt:Length/binary>> <= TokenData],
    {ok, {control, lists:reverse(Fmts)}, Rest}.

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
    {ok, {done, Flags, TransState, AffectedRows}, Rest}.

%decode_status(StatusFlags, StatusBits) ->
%    FlagsCount = length(StatusFlags),
%    decode_status(StatusFlags, StatusBits, FlagsCount, Acc).

%decode_status([CurrentFlag|RestFlags], StatusBits, FlagsCount, Acc) ->
%    <<_:FlagsCount, CurrentBit:1, RestBits/bits>> = StatusBits,
%    decode_status(RestFlags, RestBits, FlagsCount+1, [{CurrentFlag, CurrentBit}|Acc]).

decode_orderby_token(<<Columns:16, Rest/binary>>) ->
    {ok, Order, Rest2} = decode_orderby_sequence(Rest, 8, Columns, []),
    {ok, {orderby, Order}, Rest2}.

decode_orderby2_token(<<_Len:32, Columns:16, Rest/binary>>) ->
    {ok, Order, Rest2} = decode_orderby_sequence(Rest, 16, Columns, []),
    {ok, {orderby, Order}, Rest2}.

decode_orderby_sequence(Rest, _Size, 0, Sequence) ->
    {ok, lists:reverse(Sequence), Rest};
decode_orderby_sequence(Data, Size, Count, Sequence) ->
    <<Column:Size, Rest/binary>> = Data,
    decode_orderby_sequence(Rest, Size, Count-1, [Column|Sequence]).

decode_returnstatus_token(<<Status:32/signed, Rest/binary>>) ->
    {ok, {returnstatus, Status}, Rest}.

decode_returnvalue_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    {ok, Format, RestData} = decode_returnvalue_format(TokenData),
    #format{
        datatype_group=DatatypeGroup, 
        datatype_max_len=MaxLen, 
        usertype_group=UsertypeGroup, 
        datatype_scale=Scale} = Format,
    {Value1, <<>>} = decode_datatype(RestData, DatatypeGroup, MaxLen),
    Value2 = decode_usertype(Value1, UsertypeGroup, Scale),
    {ok, {returnvalue, Value2}, Rest}.

decode_returnvalue_format(Data) ->
    <<ParamNameLen, ParamName:ParamNameLen/binary,
        Status, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_dataformat(Rest),
    F2 = F1#format{
        column_name     = ParamName,
        status          = Status,
        usertype        = UserType,
        usertype_group  = get_usertype_group(UserType)
    },
    {ok, F2, Rest2}.

decode_rowformat_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    <<RowsAmount:16, TokenRest/binary>> = TokenData,
    RowFormat = decode_rowformat(TokenRest, []),
    {ok, {rowformat, RowsAmount, RowFormat}, Rest}.

decode_rowformat(<<>>, RowFormat) ->
    lists:reverse(RowFormat);
decode_rowformat(Data, RowFormat) ->
    <<ParamNameLen, ParamName:ParamNameLen/binary,
        Status, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_dataformat(Rest),
    <<LocaleLength, LocaleInfo:LocaleLength/binary, Rest3/binary>> = Rest2,
    F2 = F1#format{
        column_name     = ParamName,
        status          = Status,
        usertype        = UserType,
        usertype_group  = get_usertype_group(UserType),
        datatype_locale = LocaleInfo
    },
    decode_rowformat(Rest3, [F2|RowFormat]).

decode_rowformat2_token(<<Len:32, TokenData:Len/binary, Rest/binary>>) ->
    <<RowsAmount:16, TokenRest/binary>> = TokenData,
    RowFormat = decode_rowformat2(TokenRest, []),
    {ok, {rowformat, RowsAmount, RowFormat}, Rest}.

decode_rowformat2(<<>>, RowsFormat) ->
    lists:reverse(RowsFormat);
decode_rowformat2(Data, RowsFormat) ->
    <<LabelNameLen, LabelName:LabelNameLen/binary,
        CatalogNameLen, CatalogName:CatalogNameLen/binary,
        SchemaNameLen, SchemaName:SchemaNameLen/binary,
        TableNameLen, TableName:TableNameLen/binary,
        ColumnNameLen, ColumnName:ColumnNameLen/binary,
        Status:32, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_dataformat(Rest),
    <<LocaleLength, LocaleInfo:LocaleLength/binary, Rest3/binary>> = Rest2,
    F2 = F1#format{
        label_name      = LabelName,
        db_name         = CatalogName,
        owner_name      = SchemaName,
        table_name      = TableName,
        column_name     = ColumnName,
        status          = Status,
        usertype        = UserType,
        usertype_group  = get_usertype_group(UserType),
        datatype_locale = LocaleInfo
    },
    decode_rowformat2(Rest3, [F2|RowsFormat]).

decode_paramsformat_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    <<ParamsAmount:16, TokenRest/binary>> = TokenData,
    ParamsFormat = decode_paramsformat(TokenRest, []),
    {ok, {paramsformat, ParamsAmount, ParamsFormat}, Rest}.

decode_paramsformat(<<>>, ParamsFormat) ->
    lists:reverse(ParamsFormat);
decode_paramsformat(Data, ParamsFormat) ->
    <<ParamNameLen, ParamName:ParamNameLen/binary,
        Status, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_dataformat(Rest),
    <<LocaleLength, LocaleInfo:LocaleLength/binary, Rest3/binary>> = Rest2,
    F2 = F1#format{
        column_name     = ParamName,
        status          = Status,
        usertype        = UserType,
        usertype_group  = get_usertype_group(UserType),
        datatype_locale = LocaleInfo
    },
    decode_paramsformat(Rest3, [F2|ParamsFormat]).

decode_paramsformat2_token(<<Len:32, TokenData:Len/binary, Rest/binary>>) ->
    <<ParamsAmount:16, TokenRest/binary>> = TokenData,
    ParamsFormat = decode_paramsformat2(TokenRest, []),
    {ok, {paramsformat, ParamsAmount, ParamsFormat}, Rest}.

decode_paramsformat2(<<>>, ParamsFormat) ->
    lists:reverse(ParamsFormat);
decode_paramsformat2(Data, ParamsFormat) ->
    <<ParamNameLen, ParamName:ParamNameLen/binary,
        Status:32, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_dataformat(Rest),
    <<LocaleLength, LocaleInfo:LocaleLength/binary, Rest3/binary>> = Rest2,
    F2 = F1#format{
        column_name     = ParamName,
        status          = Status,
        usertype        = UserType,
        usertype_group  = get_usertype_group(UserType),
        datatype_locale = LocaleInfo
    },
    decode_paramsformat2(Rest3, [F2|ParamsFormat]).

decode_row_token(TokenData, TokensBufer) ->
    {rowformat, _Amount, RowFormat} = lists:keyfind(rowformat, 1, TokensBufer),
    {ok, Rows, Rest} = decode_row(TokenData, RowFormat),
    {ok, {row, Rows}, Rest}.

decode_params_token(TokenData, TokensBufer) ->
    {paramsformat, _Amount, ParamsFormat} = lists:keyfind(paramsformat,1, TokensBufer),
    {ok, Params, Rest} = decode_row(TokenData, ParamsFormat),
    {ok, {params, Params}, Rest}.


decode_dataformat(<<DataType, Rest/binary>>) ->
    case get_datatype_group(DataType) of
        fixed ->
            Format = #format{
                datatype_group      = fixed,
                datatype            = DataType,
                datatype_max_len    = get_datatype_max_length(DataType)
            },
            {Format, Rest};
        variable ->
            <<MaxLen, Rest2/binary>> = Rest,
            Format = #format{
                datatype_group      = variable,
                datatype            = DataType,
                datatype_max_len    = MaxLen
            },
            {Format, Rest2};
        long ->
            <<MaxLen:32, Rest2/binary>> = Rest,
            Format = #format{
                datatype_group      = long,
                datatype            = DataType,
                datatype_max_len    = MaxLen
            },
            {Format, Rest2};
        decimal ->
            <<MaxLen, Precision, Scale, Rest2/binary>> = Rest,
            Format = #format{
                datatype_group      = decimal,
                datatype            = DataType,
                datatype_max_len    = MaxLen,
                datatype_precision  = Precision,
                datatype_scale      = Scale
            },
            {Format, Rest2};
        clob ->
            <<MaxLen:32, NLen:16, Name:NLen/binary, Rest2/binary>> = Rest,
            Format = #format{
                datatype_group      = clob,
                datatype            = DataType,
                datatype_max_len    = MaxLen,
                datatype_name       = Name
            },
            {Format, Rest2}
    end.

decode_row(Data, RowFormat) ->
    decode_row(Data, RowFormat, []).

decode_row(Data, [Format|RestRowFormat], Values) ->
    #format{
        datatype_group=DatatypeGroup, 
        datatype_max_len=MaxLen, 
        usertype_group=UsertypeGroup, 
        datatype_scale=Scale} = Format,
    {Value1, RestData} = decode_datatype(Data, DatatypeGroup, MaxLen),
    Value2 = decode_usertype(Value1, UsertypeGroup, Scale),
    decode_row(RestData, RestRowFormat, [Value2|Values]);
decode_row(Data, [], Values) ->
    {ok, lists:reverse(Values), Data}.

decode_datatype(Data, fixed, MaxLen) ->
    <<Value:MaxLen/binary, Rest/binary>> = Data,
    {Value, Rest};
decode_datatype(Data, variable, _MaxLen) ->
    <<Length, Value:Length/binary, Rest/binary>> = Data,
    {Value, Rest};
decode_datatype(Data, long, _MaxLen) ->
    <<Length:32, Value:Length/binary, Rest/binary>> = Data,
    {Value, Rest};
decode_datatype(Data, decimal, _MaxLen) ->
    <<Length, Value:Length/binary, Rest/binary>> = Data,
    {Value, Rest};
decode_datatype(Data, clob, _MaxLen) ->
    case Data of
        <<0, Rest/binary>> ->
            {<<>>, Rest};
        <<TPLen, _TxtPtr:TPLen/binary, _TimeStamp:64, Rest1/binary>> ->
            <<Length:32, Value:Length/binary, Rest2/binary>> = Rest1,
            {Value, Rest2}
    end.

decode_usertype(<<>>, _UsertypeGroup, _Scale) ->
    null;
decode_usertype(BinValue, binary, _Scale) ->
    BinValue;
decode_usertype(BinValue, unsigned_integer, _Scale) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/unsigned>> = BinValue,
    Value;
decode_usertype(BinValue, signed_integer, _Scale) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/signed>> = BinValue,
    Value;
decode_usertype(BinValue, float, _Scale) ->
    BitLength = bit_size(BinValue),
    <<Value:BitLength/float>> = BinValue,
    Value;
decode_usertype(<<DaysSince1900:32, _/binary>>, date, _Scale) ->
    {date, calendar:gregorian_days_to_date(693961 + DaysSince1900)};
decode_usertype(<<Seconds:32>>, time, _Scale) ->
    {time, calendar:seconds_to_time(Seconds div 300)};
decode_usertype(<<_:4/binary, Seconds:32>>, time, _Scale) ->
    {time, calendar:seconds_to_time(Seconds div 300)};
decode_usertype(<<DaysSince1900:32, Seconds:32>>, datetime, _Scale) ->
    Date = calendar:gregorian_days_to_date(693961 + DaysSince1900),
    Time = calendar:seconds_to_time(Seconds div 300),
    {Date, Time};
decode_usertype(<<DaysSince1900:16, Seconds:16>>, datetime, _Scale) ->
    Date = calendar:gregorian_days_to_date(693961 + DaysSince1900),
    Time = calendar:seconds_to_time(Seconds * 60),
    {Date, Time};
decode_usertype(BinValue, decimal, 0) ->
    BitLength = bit_size(BinValue) - 8,
    case BinValue of
        <<0:8,Value:BitLength>> ->
            Value;
        <<1:8,Value:BitLength>> ->
            -Value
    end;
decode_usertype(BinValue, decimal, Scale) ->
    BitLength = bit_size(BinValue) - 8,
    case BinValue of
        <<0:8,Value:BitLength>> ->
            {decimal, Value, Scale};
        <<1:8,Value:BitLength>> ->
            {decimal, -Value, Scale}
    end.

get_datatype_max_length(?TDS_TYPE_UINT2)        -> 2;
get_datatype_max_length(?TDS_TYPE_UINT4)        -> 4;
get_datatype_max_length(?TDS_TYPE_UINT8)        -> 8;
get_datatype_max_length(?TDS_TYPE_INT1)         -> 1;
get_datatype_max_length(?TDS_TYPE_INT2)         -> 2;
get_datatype_max_length(?TDS_TYPE_INT4)         -> 4;
get_datatype_max_length(?TDS_TYPE_INT8)         -> 8;
get_datatype_max_length(?TDS_TYPE_FLT4)         -> 4;
get_datatype_max_length(?TDS_TYPE_FLT8)         -> 8;
get_datatype_max_length(?TDS_TYPE_DATE)         -> 4;
get_datatype_max_length(?TDS_TYPE_TIME)         -> 4;
get_datatype_max_length(?TDS_TYPE_SHORTDATE)    -> 4;
get_datatype_max_length(?TDS_TYPE_DATETIME)     -> 8.

get_datatype_group(?TDS_TYPE_INT1)              -> fixed;
get_datatype_group(?TDS_TYPE_INT2)              -> fixed;
get_datatype_group(?TDS_TYPE_INT4)              -> fixed;
get_datatype_group(?TDS_TYPE_INT8)              -> fixed;
get_datatype_group(?TDS_TYPE_UINT2)             -> fixed;
get_datatype_group(?TDS_TYPE_UINT4)             -> fixed;
get_datatype_group(?TDS_TYPE_UINT8)             -> fixed;
get_datatype_group(?TDS_TYPE_FLT4)              -> fixed;
get_datatype_group(?TDS_TYPE_FLT8)              -> fixed;
get_datatype_group(?TDS_TYPE_SHORTDATE)         -> fixed;
get_datatype_group(?TDS_TYPE_DATETIME)          -> fixed;
get_datatype_group(?TDS_TYPE_DATE)              -> fixed;
get_datatype_group(?TDS_TYPE_TIME)              -> fixed;
get_datatype_group(?TDS_TYPE_BIT)               -> fixed;
get_datatype_group(?TDS_TYPE_SHORTMONEY)        -> fixed;
get_datatype_group(?TDS_TYPE_MONEY)             -> fixed;
get_datatype_group(?TDS_TYPE_VOID)              -> fixed;
get_datatype_group(?TDS_TYPE_INTN)              -> variable;
get_datatype_group(?TDS_TYPE_UINTN)             -> variable;
get_datatype_group(?TDS_TYPE_FLTN)              -> variable;
get_datatype_group(?TDS_TYPE_CHAR)              -> variable;
get_datatype_group(?TDS_TYPE_VARCHAR)           -> variable;
get_datatype_group(?TDS_TYPE_BINARY)            -> variable;
get_datatype_group(?TDS_TYPE_VARBINARY)         -> variable;
get_datatype_group(?TDS_TYPE_DATETIMEN)         -> variable;
get_datatype_group(?TDS_TYPE_DATEN)             -> variable;
get_datatype_group(?TDS_TYPE_TIMEN)             -> variable;
get_datatype_group(?TDS_TYPE_MONEYN)            -> variable;
get_datatype_group(?TDS_TYPE_LONGCHAR)          -> long;
get_datatype_group(?TDS_TYPE_LONGBINARY)        -> long;
get_datatype_group(?TDS_TYPE_NUMN)              -> decimal;
get_datatype_group(?TDS_TYPE_DECN)              -> decimal;
get_datatype_group(?TDS_TYPE_TEXT)              -> clob;
get_datatype_group(?TDS_TYPE_IMAGE)             -> clob.

get_usertype_group(?USER_TYPE_CHAR)             -> binary;
get_usertype_group(?USER_TYPE_NCHAR)            -> binary;
get_usertype_group(?USER_TYPE_VARCHAR)          -> binary;
get_usertype_group(?USER_TYPE_NVARCHAR)         -> binary;
get_usertype_group(?USER_TYPE_BINARY)           -> binary;
get_usertype_group(?USER_TYPE_VARBINARY)        -> binary;
get_usertype_group(?USER_TYPE_TEXT)             -> binary;
get_usertype_group(?USER_TYPE_SMALLINT)         -> signed_integer;
get_usertype_group(?USER_TYPE_INT)              -> signed_integer;
get_usertype_group(?USER_TYPE_BIGINT)           -> signed_integer;
get_usertype_group(?USER_TYPE_INTN)             -> signed_integer;
get_usertype_group(?USER_TYPE_TINYINT)          -> unsigned_integer;
get_usertype_group(?USER_TYPE_USMALLINT)        -> unsigned_integer;
get_usertype_group(?USER_TYPE_UINT)             -> unsigned_integer;
get_usertype_group(?USER_TYPE_UBIGINT)          -> unsigned_integer;
get_usertype_group(?USER_TYPE_UNSIGNED_SHORT)   -> unsigned_integer;
get_usertype_group(?USER_TYPE_UNSIGNED_INT)     -> unsigned_integer;
get_usertype_group(?USER_TYPE_UNSIGNED_LONG)    -> unsigned_integer;
get_usertype_group(?USER_TYPE_FLOAT)            -> float;
get_usertype_group(?USER_TYPE_REAL)             -> float;
get_usertype_group(?USER_TYPE_NUMERIC)          -> decimal;
get_usertype_group(?USER_TYPE_NUMERICN)         -> decimal;
get_usertype_group(?USER_TYPE_DECIMAL)          -> decimal;
get_usertype_group(?USER_TYPE_DECIMALN)         -> decimal;
get_usertype_group(?USER_TYPE_SMALLMONEY)       -> money;
get_usertype_group(?USER_TYPE_MONEY)            -> money;
get_usertype_group(?USER_TYPE_DATE)             -> date;
get_usertype_group(?USER_TYPE_LONGDATE)         -> date;
get_usertype_group(?USER_TYPE_TIME)             -> time;
get_usertype_group(?USER_TYPE_LONGTIME)         -> time;
get_usertype_group(?USER_TYPE_DATETIME)         -> datetime;
get_usertype_group(?USER_TYPE_SMALLDATETIME)    -> datetime;
get_usertype_group(_)                           -> signed_integer.
