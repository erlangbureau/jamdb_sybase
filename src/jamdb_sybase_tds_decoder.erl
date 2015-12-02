-module(jamdb_sybase_tds_decoder).

%% API
-export([decode_packet/1]).
-export([decode_token/2]).

-include("TDS_constants.hrl").
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
        ?TDS_TOKEN_DONE ->          decode_done_token(Data, done);
        ?TDS_TOKEN_DONEINPROC ->    decode_done_token(Data, doneinproc);
        ?TDS_TOKEN_DONEPROC ->      decode_done_token(Data, doneproc);
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
            {error, {unknown_tds_token, [{token, Token}, {token_data, Data}]}}
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

decode_capability_token(<<_Len:16, 1, ReqLen, Req:ReqLen/unit:8, 2, RespLen, 
        Resp:RespLen/unit:8, Rest/binary>>) ->
    ReqCap = decode_valuemask(Req),
    RespCap = decode_valuemask(Resp),
    {ok, {capability, ReqCap, RespCap}, Rest}.

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

decode_done_token(<<Status:16, TransState:16, Count:32, Rest/binary>>, Type) ->
    StatusFlags = decode_bitmask(Status),
    {ok, {Type, StatusFlags, TransState, Count}, Rest}.

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
    {Value, <<>>} = decode_value(RestData, Format, true),
    {ok, {returnvalue, Value}, Rest}.

decode_returnvalue_format(Data) ->
    <<ParamNameLen, ParamName:ParamNameLen/binary,
        Status, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_dataformat(Rest),
    F2 = F1#format{
        column_name     = ParamName,
        status          = decode_bitmask(Status),
        usertype        = UserType
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
        status          = decode_bitmask(Status),
        usertype        = UserType,
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
        status          = decode_bitmask(Status),
        usertype        = UserType,
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
        status          = decode_bitmask(Status),
        usertype        = UserType,
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
        status          = decode_bitmask(Status),
        usertype        = UserType,
        datatype_locale = LocaleInfo
    },
    decode_paramsformat2(Rest3, [F2|ParamsFormat]).

decode_row_token(TokenData, TokensBufer) ->
    {rowformat, _Amount, RowFormat} = lists:keyfind(rowformat, 1, TokensBufer),
    {ok, Rows, Rest} = decode_values(TokenData, RowFormat),
    {ok, {row, Rows}, Rest}.

decode_params_token(TokenData, TokensBufer) ->
    {paramsformat, _Amount, ParamsFormat} = lists:keyfind(paramsformat,1, TokensBufer),
    {ok, Params, Rest} = decode_values(TokenData, ParamsFormat),
    {ok, {params, Params}, Rest}.

decode_dataformat(<<DataType, Rest/binary>>) ->
    case get_datatype_group(DataType) of
        fixed ->
            Format = #format{
                datatype            = DataType,
                datatype_group      = fixed
            },
            {Format, Rest};
        variable ->
            <<MaxLen, Rest2/binary>> = Rest,
            Format = #format{
                datatype            = DataType,
                datatype_group      = variable,
                datatype_max_len    = MaxLen
            },
            {Format, Rest2};
        long ->
            <<MaxLen:32, Rest2/binary>> = Rest,
            Format = #format{
                datatype            = DataType,
                datatype_group      = long,
                datatype_max_len    = MaxLen
            },
            {Format, Rest2};
        decimal ->
            <<MaxLen, Precision, Scale, Rest2/binary>> = Rest,
            Format = #format{
                datatype            = DataType,
                datatype_group      = decimal,
                datatype_max_len    = MaxLen,
                datatype_precision  = Precision,
                datatype_scale      = Scale
            },
            {Format, Rest2};
        clob ->
            <<MaxLen:32, NLen:16, Name:NLen/binary, Rest2/binary>> = Rest,
            Format = #format{
                datatype            = DataType,
                datatype_group      = clob,
                datatype_max_len    = MaxLen,
                datatype_name       = Name
            },
            {Format, Rest2}
    end.

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
get_datatype_group(?TDS_TYPE_UNITEXT)           -> clob;
get_datatype_group(?TDS_TYPE_IMAGE)             -> clob.

decode_bitmask(BitMask) ->
    decode_bitmask(BitMask, 1, []).

decode_bitmask(BitMask, Flag, Result) when BitMask < Flag ->
    Result;
decode_bitmask(BitMask, Flag, Result) ->
    IsSet = ((BitMask band Flag) == Flag),
    Result2 = case IsSet of
        true  -> [Flag|Result];
        false -> Result
    end,
    decode_bitmask(BitMask, Flag*2, Result2).

decode_valuemask(ValueMask) ->
    decode_valuemask(ValueMask, 1, []).

decode_valuemask(ValueMask, Value, Result) when ValueMask < 1 bsl Value ->
    Result;
decode_valuemask(ValueMask, Value, Result) ->
    IsSet = ((ValueMask bsr Value) band 1) == 1,
    Result2 = case IsSet of
        true  -> [Value|Result];
        false -> Result
    end,
    decode_valuemask(ValueMask, Value+1, Result2).

decode_values(Data, RowFormat) ->
    decode_values(Data, RowFormat, []).

decode_values(Data, [Format = #format{status=Status}|RestRowFormat], Values) ->
    {Value, RestData} = case lists:member(?TDS_ROW_COLUMNSTATUS, Status) of
        true ->
            <<ColumnStatus, Rest/binary>> = Data,
            case ColumnStatus of
                1 -> {null, Rest};
                _ ->
                    decode_value(Rest, Format, false)
            end;
        false ->
            decode_value(Data, Format, true)
    end,
    decode_values(RestData, RestRowFormat, [Value|Values]);
decode_values(Data, [], Values) ->
    {ok, lists:reverse(Values), Data}.

decode_value(Data, #format{datatype=?TDS_TYPE_INTN}, _Nullable) ->
    case Data of
        <<0, Rest/binary>> ->
            {null, Rest};
        <<1, Value:1/unsigned-unit:8, Rest/binary>> ->
            {Value, Rest};
        <<Length, Value:Length/signed-unit:8, Rest/binary>> ->
            {Value, Rest}
    end;
decode_value(Data, #format{datatype=?TDS_TYPE_INT1}, _Nullable) ->
    <<Value:8/unsigned, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_INT2}, _Nullable) ->
    <<Value:16/signed, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_INT4}, _Nullable) ->
    <<Value:32/signed, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_INT8}, _Nullable) ->
    <<Value:64/signed, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_UINTN}, Nullable) ->
    <<Length, Value:Length/unsigned-unit:8, Rest/binary>> = Data,
    {decode_nullable(Length, Value, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_UINT2}, _Nullable) ->
    <<Value:16/unsigned, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_UINT4}, _Nullable) ->
    <<Value:32/unsigned, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_UINT8}, _Nullable) ->
    <<Value:64/unsigned, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_NUMN, datatype_scale=Scale}, _) ->
    <<Length, Value:Length/binary, Rest/binary>> = Data,
    {decode_decimal(Value, Scale), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_DECN, datatype_scale=Scale}, _) ->
    <<Length, Value:Length/binary, Rest/binary>> = Data,
    {decode_decimal(Value, Scale), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_CHAR}, Nullable) ->
    <<Length, Value:Length/binary, Rest/binary>> = Data,
    {decode_nullable(Length, Value, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_VARCHAR}, Nullable) ->
    <<Length, Value:Length/binary, Rest/binary>> = Data,
    {decode_nullable(Length, Value, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_BINARY}, Nullable) ->
    <<Length, Value:Length/binary, Rest/binary>> = Data,
    {decode_nullable(Length, Value, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_VARBINARY}, Nullable) ->
    <<Length, Value:Length/binary, Rest/binary>> = Data,
    {decode_nullable(Length, Value, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_FLTN}, _Nullable) ->
    <<Length, Value:Length/float-unit:8, Rest/binary>> = Data,
    {decode_nullable(Length, Value, _Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_FLT4}, _Nullable) ->
    <<Value:32/float, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_FLT8}, _Nullable) ->
    <<Value:64/float, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_SHORTDATE}, _Nullable) ->
    <<DaysSince1900:16/unsigned, Seconds:16/unsigned, Rest/binary>> = Data,
    Date = decode_date(DaysSince1900),
    Time = decode_time(Seconds, seconds),
    {{Date, Time}, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_DATETIME}, _Nullable) ->
    <<DaysSince1900:32/unsigned, MlSeconds:32/unsigned, Rest/binary>> = Data,
    Date = decode_date(DaysSince1900),
    Time = decode_time(MlSeconds, milliseconds),
    {{Date, Time}, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_DATE}, _Nullable) ->
    <<DaysSince1900:32/unsigned, Rest/binary>> = Data,
    {decode_date(DaysSince1900), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_TIME}, _Nullable) ->
    <<MlSeconds:32/unsigned, Rest/binary>> = Data,
    {decode_time(MlSeconds, milliseconds), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_DATETIMEN}, _Nullable) ->
    case Data of
        <<0, Rest/binary>> ->
            {null, Rest};
        <<4, DaysSince1900:16/unsigned, Seconds:16/unsigned, Rest/binary>> ->
                Date = decode_date(DaysSince1900),
                Time = decode_time(Seconds, seconds),
                {{Date, Time}, Rest};
        <<8, DaysSince1900:32/unsigned, MlSeconds:32/unsigned, Rest/binary>> ->
                Date = decode_date(DaysSince1900),
                Time = decode_time(MlSeconds, milliseconds),
                {{Date, Time}, Rest}
    end;
decode_value(Data, #format{datatype=?TDS_TYPE_DATEN}, Nullable) ->
    <<Length, DaysSince1900:Length/unsigned-unit:8, Rest/binary>> = Data,
    Date = decode_date(DaysSince1900),
    {decode_nullable(Length, Date, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_TIMEN}, Nullable) ->
    <<Length, MlSeconds:Length/unsigned-unit:8, Rest/binary>> = Data,
    Time = decode_time(MlSeconds, milliseconds),
    {decode_nullable(Length, Time, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_MONEYN}, Nullable) ->
    <<Length, Value:Length/float-unit:8, Rest/binary>> = Data,
    {decode_nullable(Length, Value, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_SHORTMONEY}, _Nullable) ->
    <<Value:32/float, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_MONEY}, _Nullable) ->
    <<Value:64/float, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_BIT}, _Nullable) ->
    <<Value:8/unsigned, Rest/binary>> = Data,
    {Value, Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_LONGCHAR}, Nullable) ->
    <<Length:32, Value:Length/binary, Rest/binary>> = Data,
    {decode_nullable(Length, Value, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_LONGBINARY}, Nullable) ->
    <<Length:32, Value:Length/binary, Rest/binary>> = Data,
    {decode_nullable(Length, Value, Nullable), Rest};
decode_value(Data, #format{datatype=?TDS_TYPE_TEXT}, _Nullable) ->
    decode_text(Data, utf8);
decode_value(Data, #format{datatype=?TDS_TYPE_UNITEXT}, _Nullable) ->
    decode_text(Data, utf16).

decode_date(DaysSince1900) ->
    calendar:gregorian_days_to_date(693961 + DaysSince1900).

decode_time(Seconds, seconds) ->
    calendar:seconds_to_time(Seconds * 60);
decode_time(Seconds, milliseconds) ->
    calendar:seconds_to_time(Seconds div 300).

decode_decimal(<<>>, _) ->
    null;
decode_decimal(Data, 0) ->
    Bits = bit_size(Data) - 8,
    case Data of
        <<0:8, Value:Bits>> -> Value;
        <<1:8, Value:Bits>> -> -Value
    end;
decode_decimal(Data, Scale) ->
    Bits = bit_size(Data) - 8,
    case Data of
        <<0:8, Value:Bits>> -> {0, Value, -Scale};
        <<1:8, Value:Bits>> -> {1, Value, -Scale}
    end.

decode_text(<<0, Rest/binary>>, _) ->
    {null, Rest};
decode_text(<<TPLen, _TxtPtr:TPLen/binary, _TimeStamp:8/binary, 
        Length:32, Value:Length/binary, Rest/binary>>, utf8) ->
    {Value, Rest};
decode_text(<<TPLen, _TxtPtr:TPLen/binary, _TimeStamp:8/binary, 
        Length:32, Value:Length/binary, Rest/binary>>, utf16) ->
    Value2 = unicode:characters_to_binary(Value, utf16),
    {Value2, Rest}.

decode_nullable(0, _Value, true) ->    null;
decode_nullable(0, Value, false) ->    Value;
decode_nullable(_Length, Value, _) ->  Value.
