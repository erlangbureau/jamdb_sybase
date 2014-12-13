-module(jamdb_sybase_tds_decoder).

%% API
-export([decode_packet/1]).
-export([decode_token/2]).

-include("TDS_5_0.hrl").
-include("jamdb_sybase.hrl").

%% decode_data_format
-define(IS_FIXED_LENGTH_TYPE(DataType),
    DataType =:= ?TDS_TYPE_INT1; 
    DataType =:= ?TDS_TYPE_INT2; 
    DataType =:= ?TDS_TYPE_INT4; 
    DataType =:= ?TDS_TYPE_INT8;
    DataType =:= ?TDS_TYPE_UINT2; 
    DataType =:= ?TDS_TYPE_UINT4; 
    DataType =:= ?TDS_TYPE_UINT8;
    DataType =:= ?TDS_TYPE_FLT4;
    DataType =:= ?TDS_TYPE_FLT8;
    DataType =:= ?TDS_TYPE_SHORTDATE;
    DataType =:= ?TDS_TYPE_DATETIME;
    DataType =:= ?TDS_TYPE_DATE;
    DataType =:= ?TDS_TYPE_TIME;
    DataType =:= ?TDS_TYPE_BIT;
    DataType =:= ?TDS_TYPE_SHORTMONEY;
    DataType =:= ?TDS_TYPE_MONEY;
    DataType =:= ?TDS_TYPE_VOID
).

-define(IS_VARIABLE_LENGTH_TYPE(DataType),
    DataType =:= ?TDS_TYPE_INTN;
    DataType =:= ?TDS_TYPE_UINTN;
    DataType =:= ?TDS_TYPE_FLTN;
    DataType =:= ?TDS_TYPE_CHAR;
    DataType =:= ?TDS_TYPE_VARCHAR;
    DataType =:= ?TDS_TYPE_BINARY;
    DataType =:= ?TDS_TYPE_VARBINARY;
    DataType =:= ?TDS_TYPE_DATETIMEN;
    DataType =:= ?TDS_TYPE_DATEN;
    DataType =:= ?TDS_TYPE_TIMEN;
    DataType =:= ?TDS_TYPE_MONEYN
).

-define(IS_LONG_LENGTH_TYPE(DataType),
    DataType =:= ?TDS_TYPE_LONGCHAR;
    DataType =:= ?TDS_TYPE_LONGBINARY
).

-define(IS_DECIMAL_TYPE(DataType),
    DataType =:= ?TDS_TYPE_NUMN;
    DataType =:= ?TDS_TYPE_DECN
).

-define(IS_BLOB_TYPE(DataType), 
    DataType =:= ?TDS_TYPE_TEXT;
    DataType =:= ?TDS_TYPE_IMAGE
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
        %?TDS_TOKEN_DYNAMIC ->       decode_dynamic_token(Data);
        _ ->
            %io:format("Unknown Token: ~p Data: ~p~n", [Token, Data]),
            {error, unknown_tds_token}
    end.

%decode_dynamic_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
%    io:format("DynamicToken: ~p~n", [TokenData]),
%    {{dynamic, TokenData}, Rest}.

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

decode_orderby_token(<<Columns:16, Rest/binary>>) ->
    {Order, Rest2} = decode_orderby_sequence(Rest, 8, Columns, []),
    {{orderby, Order}, Rest2}.

decode_orderby2_token(<<_Len:32, Columns:16, Rest/binary>>) ->
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
    {Format, RestData} = decode_data_format(TokenData, returnvalue),
    {Value, <<>>} = decode_data(RestData, Format),
    {{returnvalue, Value}, Rest}.

decode_rowformat_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    <<_ColsNumber:16, TokenRest/binary>> = TokenData,
    RowFormat = decode_data_format(TokenRest, simple, []),
    {{rowformat, RowFormat}, Rest}.

decode_rowformat2_token(<<Len:32, TokenData:Len/binary, Rest/binary>>) ->
    <<_ColsNumber:16, TokenRest/binary>> = TokenData,
    RowFormat = decode_data_format(TokenRest, column_extended, []),
    {{rowformat, RowFormat}, Rest}.
    
decode_paramsformat_token(<<Len:16, TokenData:Len/binary, Rest/binary>>) ->
    <<_ColsNumber:16, TokenRest/binary>> = TokenData,
    RowFormat = decode_data_format(TokenRest, simple, []),
    {{paramsformat, RowFormat}, Rest}.

decode_paramsformat2_token(<<Len:32, TokenData:Len/binary, Rest/binary>>) ->
    <<_ColsNumber:16, TokenRest/binary>> = TokenData,
    RowFormat = decode_data_format(TokenRest, param_extended, []),
    {{paramsformat, RowFormat}, Rest}.

decode_row_token(TokenData, RowFormat) ->
    {Rows, Rest} = decode_row(TokenData, RowFormat),
    {{row, Rows}, Rest}.

decode_params_token(TokenData, ParamsFormat) ->
    {Rows, Rest} = decode_row(TokenData, ParamsFormat),
    {{params, Rows}, Rest}.

decode_data_format(<<>>, _EncodingFormat, RowFormat) ->
    lists:reverse(RowFormat);
decode_data_format(Data, EncodingFormat, RowFormat) ->
    {ColFormat, Rest} = decode_data_format(Data, EncodingFormat),
    decode_data_format(Rest, EncodingFormat, [ColFormat|RowFormat]).

%% TODO status byte
decode_data_format(Data, returnvalue) ->
    <<ParamNameLen, ParamName:ParamNameLen/binary,
        Status, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_datatype_format(Rest),
    F2 = F1#format{
        column_name = ParamName,
        status      = Status,
        usertype    = UserType
    },
    {F2, Rest2};
decode_data_format(Data, simple) ->
    <<ParamNameLen, ParamName:ParamNameLen/binary,
        Status, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_datatype_format(Rest),
    <<LocaleLength, LocaleInfo:LocaleLength/binary, Rest3/binary>> = Rest2,
    F2 = F1#format{
        column_name = ParamName,
        status      = Status,
        usertype    = UserType,
        locale      = LocaleInfo
    },
    {F2, Rest3};
decode_data_format(Data, param_extended) ->
    <<ParamNameLen, ParamName:ParamNameLen/binary,
        Status:32, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_datatype_format(Rest),
    <<LocaleLength, LocaleInfo:LocaleLength/binary, Rest3/binary>> = Rest2,
    F2 = F1#format{
        column_name = ParamName,
        status      = Status,
        usertype    = UserType,
        locale      = LocaleInfo
    },
    {F2, Rest3};
decode_data_format(Data, column_extended) ->
    <<LabelNameLen, LabelName:LabelNameLen/binary,
        CatalogNameLen, CatalogName:CatalogNameLen/binary,
        SchemaNameLen, SchemaName:SchemaNameLen/binary,
        TableNameLen, TableName:TableNameLen/binary,
        ColumnNameLen, ColumnName:ColumnNameLen/binary,
        Status:32, UserType:32/signed, Rest/binary>> = Data,
    {F1, Rest2} = decode_datatype_format(Rest),
    <<LocaleLength, LocaleInfo:LocaleLength/binary, Rest3/binary>> = Rest2,
    F2 = F1#format{
        label_name  = LabelName,
        db_name     = CatalogName,
        owner_name  = SchemaName,
        table_name  = TableName,
        column_name = ColumnName,
        status      = Status,
        usertype    = UserType,
        locale      = LocaleInfo
    },
    {F2, Rest3}.

decode_datatype_format(<<DataType, Rest/binary>>) ->
    if
        ?IS_FIXED_LENGTH_TYPE(DataType) ->
            Format = #format{
                format      = fixed,
                tdstype     = DataType},
            {Format, Rest};
        ?IS_VARIABLE_LENGTH_TYPE(DataType) ->
            <<_DataLen, Rest2/binary>> = Rest,
            Format = #format{
                format      = variable,
                tdstype     = DataType},
            {Format, Rest2};
        ?IS_LONG_LENGTH_TYPE(DataType) ->
            <<_DataLen:32, Rest2/binary>> = Rest,
            Format = #format{
                format      = long,
                tdstype     = DataType},
            {Format, Rest2};
        ?IS_DECIMAL_TYPE(DataType) ->
            <<_DataLen, _Precision, Scale, Rest2/binary>> = Rest,
            Format = #format{
                format      = decimal,
                tdstype     = DataType,
                scale       = Scale},
            {Format, Rest2};
        DataType =:= ?TDS_TYPE_TEXT ->
            <<_DataLen:32, NLen:16, ObjName:NLen/binary, Rest2/binary>> = Rest,
            Format = #format{
                format      = text,
                obj_name    = ObjName,
                tdstype     = DataType},
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
decode_data(Data, #format{format=long, usertype=UserType}) ->
    <<Length:32, BinValue:Length/binary, Rest/binary>> = Data,
    Value = decode_value(BinValue, UserType),
    {Value, Rest};
decode_data(Data, #format{format=decimal, usertype=UserType, scale=Scale}) ->
    <<Length, BinValue:Length/binary, Rest/binary>> = Data,
    Value = decode_value(BinValue, UserType, Scale),
    {Value, Rest};
decode_data(Data, #format{format=text, usertype=UserType}) ->
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
