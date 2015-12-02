-module(jamdb_sybase_tds_encoder).

%% API
-export([encode_packets/3]).
-export([encode_tokens/1]).
-export([prepare_input_dataformat/1]).

-include("TDS_constants.hrl").
-include("jamdb_sybase.hrl").

%% API
encode_packets(TokenStream, PacketType, PktSize) ->
    DataSize = PktSize - 8,
    PktType = case PacketType of
        normal  -> ?TDS_PKT_NORMAL;
        login   -> ?TDS_PKT_LOGIN
    end,
    encode_packets(TokenStream, PktType, PktSize, DataSize, <<>>).

encode_packets(InStream, PktType, PktSize, DataSize, OutStream) ->
    case InStream of
        <<PktData:DataSize/binary, InStream2/binary>> ->
            OutStream2 = <<OutStream/binary, 
                PktType:8, 0:8, PktSize:16, 0:32, PktData/binary>>,
            encode_packets(InStream2, PktType, PktSize, DataSize, OutStream2);
        PktData ->
            LastPktSize = byte_size(PktData) + 8,
            <<OutStream/binary, 
                PktType:8, 1:8, LastPktSize:16, 0:32, PktData/binary>>
    end.

encode_tokens(TokenList) ->
    encode_tokens(TokenList, {}, <<>>).

encode_tokens([Token|TokensList], Previous, TokenStream) ->
    TokenType = element(1, Token),
    Data = case TokenType of
        language ->     encode_language_token(Token);
        dynamic ->      encode_dynamic_token(Token);
        paramsformat -> encode_paramsformat_token(Token);
        dbrpc ->        encode_dbrpc_token(Token);
        login ->        encode_login_record(Token);
        logout ->       encode_logout_token(Token);
        params ->
            {paramsformat, _, Formats} = Previous,
            encode_params_token(Token, Formats)
    end,
    encode_tokens(TokensList, Token, <<TokenStream/binary, Data/binary>>);
encode_tokens([], _Previous, TokenStream) ->
    TokenStream.

%% internal
encode_login_record({login, EnvOpts}) ->
    {ok, UserHost}  = inet:gethostname(),
    UserPID         = os:getpid(),
    User            = proplists:get_value(user, EnvOpts),
    Pass            = proplists:get_value(password, EnvOpts),
    AppName         = proplists:get_value(app_name, EnvOpts, "erlang"),
    LibName         = proplists:get_value(lib_name, EnvOpts, "jamdb"),
    ServerName      = proplists:get_value(server_name, EnvOpts, ""),
    Language        = proplists:get_value(language, EnvOpts, "us_english"),
    Charset         = proplists:get_value(charset, EnvOpts, utf8),
    PacketSize      = proplists:get_value(packet_size, EnvOpts, 512),
    <<
        (encode_data(UserHost, ?USER_TYPE_CHAR, 30)):31/binary,
        (encode_data(User, ?USER_TYPE_CHAR, 30)):31/binary,
        (encode_data(Pass, ?USER_TYPE_CHAR, 30)):31/binary,
        (encode_data(UserPID, ?USER_TYPE_CHAR, 30)):31/binary,
        2,          %% type of int2: bigendian
        0,          %% type of int4: bigendian
        6,          %% type of char: ascii
        4,          %% type of float: bigendian
        8,          %% type of date: bigendian
        1,          %% notify of use db
        0,          %% disallow dump/load and bulk insert
        0,          %% sql interface type: server's default SQL
        0,          %% type of network dialog: client
        0:56,       %% deprecated fields 
        (encode_data(AppName, ?USER_TYPE_CHAR, 30)):31/binary,
        (encode_data(ServerName, ?USER_TYPE_CHAR, 30)):31/binary,
        %% Remote password array (used on server-to-server dialogs)
        (encode_data(encode_lrempw(Pass), ?USER_TYPE_CHAR, 255)):256/binary, 
        5,0,0,0,    %% Protocol version
        (encode_data(LibName, ?USER_TYPE_CHAR, 10)):11/binary,
        16,0,0,1,   %% Lib version
        0,          %% auto convert 4 byte types to 8 byte
        12,         %% type of 4 byte floating: bigendian
        16,         %% type of 4 byte datetime: bigendian
        (encode_data(Language, ?USER_TYPE_CHAR, 30)):31/binary,
        1,          %% notify on lang change
        0:104,      %% security fields
        (encode_data(Charset, ?USER_TYPE_CHAR, 30)):31/binary,
        1,          %% notify on charset change
        (encode_data(PacketSize, ?USER_TYPE_CHAR, 6)):7/binary,
        0:32,       %% something magic
        (encode_token_capability())/binary
    >>.

encode_logout_token({logout, _Opts}) ->
    <<
        ?TDS_TOKEN_LOGOUT, 
        0   %% Options: no options
    >>.

encode_language_token({language, Query}) ->
    <<
        ?TDS_TOKEN_LANGUAGE, 
        (byte_size(Query)+1):32,    %% token length
        0,                          %% status mask: not parameterized query
        Query/binary
    >>.

%% If TDS_PROTO_DYNAMIC (CS_PROTO_DYNAMIC) capability is enabled (1),
%%   the TDS_DYN_DESCOUT/DESCIN protocol is used tosend input and output formats to a client.
%% If TDS_PROTO_DYNAMIC (CS_PROTO_DYNAMIC) capability is disabled (0), 
%%   the format information is sent back automatically by the serverat TDS_DYN_PREPARE time.
%% If TDS_PROTO_DYNPROC (CS_PROTO_DYNPROC) capability is enabled (1),
%%   a client library will prepend “create proc” in the Stmt field of the TDS_DYN_PREPARE data stream. 
%% If TDS_PROTO_DYNPROC (CS_PROTO_DYNPROC) capability is disabled (0),
%%   a client library will just send the Stmt information un-modified.
encode_dynamic_token({dynamic, Type, Status, Id, Stmt}) ->
    StmtLen = byte_size(Stmt),
    IdLen = byte_size(Id),
    BinType = case Type of
        prepare  -> ?TDS_DYN_PREPARE;
        execute  -> ?TDS_DYN_EXEC;
        unprepare -> ?TDS_DYN_DEALLOC
    end,
    case (StmtLen < 32765 - IdLen) of
        true ->
            <<
                ?TDS_TOKEN_DYNAMIC,
                (5 + IdLen + StmtLen):16,   %% Token Length
                BinType:8,                  %% type of dynamic operation
                (encode_bitmask(Status)):8, %% status
                IdLen:8,                    %% Length of statement id
                Id:IdLen/binary,            %% statement id
                StmtLen:16,                 %% Length of statement
                Stmt:StmtLen/binary         %% statement
            >>;
        false ->
            <<
                ?TDS_TOKEN_DYNAMIC2,
                (7 + IdLen + StmtLen):32,   %% Token Length
                BinType:8,                  %% type of dynamic operation
                (encode_bitmask(Status)):8, %% status
                IdLen:8,                    %% Length of statement id
                Id:IdLen/binary,            %% statement id
                StmtLen:32,                 %% Length of statement
                Stmt:StmtLen/binary         %% statement
            >>
    end.

encode_paramsformat_token({paramsformat, ParamsAmount, ParamsFormat}) ->
    EncodedParamsFormat = encode_paramsformat(ParamsFormat),
    EncodedParamsFormatLen = byte_size(EncodedParamsFormat),
    <<
        ?TDS_TOKEN_PARAMFMT, 
        (EncodedParamsFormatLen + 2):16,
        ParamsAmount:16,
        EncodedParamsFormat/binary
    >>.

encode_paramsformat(List) ->
    encode_paramsformat(List, <<>>).

encode_paramsformat([#format{
        column_name=ParamName, 
        usertype=UserType,
        datatype_locale=LocaleInfo} = Format|Rest], Encoded) ->
    ParamNameLen = byte_size(ParamName),
    LocaleLen = byte_size(LocaleInfo),
    EncodedParam = <<
        ParamNameLen,
        ParamName:ParamNameLen/binary,
        (encode_bitmask([])),
        UserType:32/signed,
        (encode_dataformat(Format))/binary,
        LocaleLen,
        LocaleInfo:LocaleLen/binary
    >>,
    encode_paramsformat(Rest, <<Encoded/binary, EncodedParam/binary>>);
encode_paramsformat([], Token) ->
    Token.

encode_dataformat(#format{datatype_group=variable, datatype=DataType, 
        datatype_max_len=MaxLen}) ->
    <<DataType, MaxLen>>;
encode_dataformat(#format{datatype_group=long, datatype=DataType, 
        datatype_max_len=MaxLen}) ->
    <<DataType, MaxLen:32>>;
encode_dataformat(#format{datatype_group=decimal, datatype=DataType, 
        datatype_max_len=MaxLen, datatype_precision=Precision, 
        datatype_scale=Scale}) ->
    <<DataType, MaxLen, Precision, Scale>>;
encode_dataformat(#format{datatype_group=clob, datatype=DataType, 
        datatype_max_len=MaxLen, datatype_name=Name}) ->
    NLen = byte_size(Name),
    <<DataType, MaxLen:32, NLen:16, Name:NLen/binary>>;
encode_dataformat(#format{datatype_group=fixed, datatype=DataType}) ->
    <<DataType>>.

encode_params_token({params, Params}, Formats) ->
    <<
        ?TDS_TOKEN_PARAMS,
        (encode_params(Params, Formats, <<>>))/binary
    >>.

encode_params([Param|Params], [Format|Formats], EncodedParams) ->
    Encoded = encode_value(Param, Format),
    encode_params(Params, Formats, <<EncodedParams/binary, Encoded/binary>>);
encode_params([], _Formats, EncodedParams) ->
    EncodedParams.

encode_value(null, _Format) ->
    <<0>>;
encode_value(Value, #format{datatype=?TDS_TYPE_INTN, datatype_max_len=Len}) ->
    case Len of
        1 -> <<1, Value:1/unsigned-unit:8>>;
        _ -> <<Len, Value:Len/signed-unit:8>>
    end;
encode_value(Value, #format{datatype=?TDS_TYPE_UINTN, datatype_max_len=Len}) ->
    <<Len, Value:Len/unsigned-unit:8>>;
encode_value(Value, #format{datatype=?TDS_TYPE_UINT8}) ->
    <<Value:64/unsigned>>;
encode_value(Decimal, #format{datatype=?TDS_TYPE_NUMN, datatype_max_len=Len}) ->
    encode_decimal(Decimal, Len);
encode_value(Decimal, #format{datatype=?TDS_TYPE_DECN, datatype_max_len=Len}) ->
    encode_decimal(Decimal, Len);
encode_value(Value, #format{datatype=?TDS_TYPE_CHAR, datatype_max_len=Len}) ->
    encode_binary(Value, Len, basic);
encode_value(Value, #format{datatype=?TDS_TYPE_VARCHAR, datatype_max_len=Len}) ->
    encode_binary(Value, Len, basic);
encode_value(Value, #format{datatype=?TDS_TYPE_BINARY, datatype_max_len=Len}) ->
    encode_binary(Value, Len, basic);
encode_value(Value, #format{datatype=?TDS_TYPE_VARBINARY, datatype_max_len=Len}) ->
    encode_binary(Value, Len, basic);
encode_value(Value, #format{datatype=?TDS_TYPE_FLTN, datatype_max_len=Len}) ->
    <<Len, Value:Len/float-unit:8>>;
encode_value({Date, Time}, #format{datatype=?TDS_TYPE_DATETIMEN, 
        datatype_max_len=Len}) ->
    case Len of
        4 ->
            DaysSince1900 = encode_date(Date),
            Seconds = encode_time(Time, seconds),
            <<4, DaysSince1900:16, Seconds:16>>;
        8 ->
            DaysSince1900 = encode_date(Date),
            MlSeconds = encode_time(Time, milliseconds),
            <<8, DaysSince1900:32, MlSeconds:32>>
    end;
encode_value(Date, #format{datatype=?TDS_TYPE_DATEN, datatype_max_len=Len}) ->
    DaysSince1900 = encode_date(Date),
    <<Len, DaysSince1900:Len/unit:8>>;
encode_value(Time, #format{datatype=?TDS_TYPE_TIMEN, datatype_max_len=Len}) ->
    MlSeconds = encode_time(Time, milliseconds),
    <<Len, MlSeconds:Len/unit:8>>;
encode_value(Value, #format{datatype=?TDS_TYPE_MONEYN, datatype_max_len=Len}) ->
    <<Len, Value:Len/float-unit:8>>;
encode_value(Value, #format{datatype=?TDS_TYPE_BIT}) ->
    <<Value>>;
encode_value(Value, #format{datatype=?TDS_TYPE_LONGCHAR, 
        datatype_max_len=Len}) ->
    encode_binary(Value, Len, long);
encode_value(Value, #format{datatype=?TDS_TYPE_LONGBINARY, 
        datatype_max_len=Len}) ->
    encode_binary(Value, Len, long).

encode_decimal(Value, MaxLen) when is_integer(Value) andalso Value >= 0 ->
    Len = MaxLen - 1,
    <<MaxLen, 0, Value:Len/unit:8>>;
encode_decimal(Value, MaxLen) when is_integer(Value) ->
    Len = MaxLen - 1,
    <<MaxLen, 1, (-Value):Len/unit:8>>;
encode_decimal({Sign, Value, _}, MaxLen) when 
        is_integer(Sign) andalso ((Sign =:= 1) orelse (Sign =:= 0)) 
        andalso is_integer(Value) andalso Value >= 0 ->
    Len = MaxLen - 1,
    <<MaxLen, Sign, Value:Len/unit:8>>.

encode_date(Date) ->
    calendar:date_to_gregorian_days(Date) - 693961.

encode_time(Time, seconds) ->
    calendar:time_to_seconds(Time) div 60;
encode_time(Time, milliseconds) ->
    calendar:time_to_seconds(Time) * 300.

encode_binary(Value, MaxLen, basic) when is_binary(Value) ->
    Length = byte_size(Value),
    case {Length, MaxLen} of
        {0, _}              -> <<1, " ">>;
        {L, M} when L < M   -> <<Length, Value:Length/binary>>;
        {_, _}              -> <<MaxLen, Value:MaxLen/binary>>
    end;
encode_binary(Value, MaxLen, long) when is_binary(Value) ->
    Length = byte_size(Value),
    case {Length, MaxLen} of
        {0, _}              -> <<1:32, " ">>;
        {L, M} when L < M   -> <<Length:32, Value:Length/binary>>;
        {_, _}              -> <<MaxLen:32, Value:MaxLen/binary>>
    end;
encode_binary(Value, MaxLen, Type) when is_list(Value) ->
    Bin = unicode:characters_to_binary(Value),
    encode_binary(Bin, MaxLen, Type);
encode_binary(Value, MaxLen, Type) when is_atom(Value) ->
    Bin = atom_to_binary(Value, utf8),
    encode_binary(Bin, MaxLen, Type);
encode_binary(Value, MaxLen, Type) when is_integer(Value) ->
    Bin = unicode:characters_to_binary(integer_to_list(Value)),
    encode_binary(Bin, MaxLen, Type).

%% The TDS_DBRPC token will be used by clients if the TDS_REQ_PARAM capability bit is true
%% The TDS_DBRPC2 token will be used by clients only if the TDS_REQ_DBRPC2 capability bit is true.
%% Return parameters will be returned to a client using the TDS_PARAMFMT/PARAMS tokens if the TDS_RES_NOPARAM capability bit is false.
encode_dbrpc_token({dbrpc, RpcName, Options}) ->
    RpcNameLen = byte_size(RpcName),
    case RpcNameLen < 255 of
        true ->
            <<
                ?TDS_TOKEN_DBRPC,
                (3 + RpcNameLen):16,            %% Token Length
                RpcNameLen:8,                   %% RPC name length, in bytes
                RpcName:RpcNameLen/binary,      %% RPC Name
                (encode_bitmask(Options)):16    %% Options bit mask
            >>;
        false ->
            <<
                ?TDS_TOKEN_DBRPC2,
                (4 + RpcNameLen):16,            %% Token Length
                RpcNameLen:16,                  %% RPC name length, in bytes
                RpcName:RpcNameLen/binary,      %% RPC Name
                (encode_bitmask(Options)):16    %% Options
            >>
    end.

encode_lrempw(Password) ->
    <<
    0:8,    %% ServerName length: zero mean universal password
    (encode_data(Password, ?USER_TYPE_VARCHAR, 1))/binary   %% Password
    >>.

encode_token_capability() ->
    ReqGroup     = encode_capability_group(?TDS_CAP_REQUEST, ?JAMDB_REQ_CAP),
    RespGroup    = encode_capability_group(?TDS_CAP_RESPONSE, ?JAMDB_RESP_CAP),
    ReqGroupLen  = byte_size(ReqGroup),
    RespGroupLen = byte_size(RespGroup),
    <<
        ?TDS_TOKEN_CAPABILITY,
        (ReqGroupLen + RespGroupLen):16,
        ReqGroup/binary,
        RespGroup/binary
    >>.

encode_capability_group(Type, Capabilities) ->
    IntValueMask = encode_valuemask(Capabilities),
    BinValueMask = binary:encode_unsigned(IntValueMask),
    ValueMaskLen = byte_size(BinValueMask),
    <<
        Type,
        ValueMaskLen,
        BinValueMask:ValueMaskLen/binary
    >>.

encode_bitmask(List) ->
    lists:foldl(fun(X, Result) -> X bor Result end, 0, List).

encode_valuemask(List) ->
    lists:foldl(fun(X, Result) -> (1 bsl X) bor Result end, 0, List).

%% TODO data coded by user type or tds type?
encode_data(Data, ?USER_TYPE_VARCHAR, Multiplier) when is_binary(Data) ->
    Length = byte_size(Data),
    MaxLength = 8 * Multiplier,
    case Length < MaxLength of
        true -> 
            <<Length:MaxLength, Data:Length/binary>>;
        false ->
            <<MaxLength:MaxLength, Data:MaxLength/binary>>
    end;
encode_data(Data, ?USER_TYPE_CHAR, MaxLength) when is_binary(Data) ->
    %% TODO any char data coded with length at the end 
    %%   or login record fields only????
    Length = byte_size(Data),
    case Length < MaxLength of
        true ->
            EmptyLength = 8 * (MaxLength - Length),
            <<Data:Length/binary, 0:EmptyLength, Length>>;
        false ->
            <<Data:MaxLength/binary, MaxLength>>
    end;
encode_data(Data, UserType, MaxLength) when is_list(Data) ->
    Binary = unicode:characters_to_binary(Data),
    encode_data(Binary, UserType, MaxLength);
encode_data(Data, UserType, MaxLength) when is_atom(Data) ->
    Binary = atom_to_binary(Data, utf8),
    encode_data(Binary, UserType, MaxLength);
encode_data(Data, UserType, MaxLength) when is_integer(Data) ->
    List = integer_to_list(Data),
    encode_data(List, UserType, MaxLength).


prepare_input_dataformat({paramsformat, ParamsAmount, ParamsFormat}) ->
    ParamsFormat2 = [replace_format(Format) || Format <- ParamsFormat],
    {paramsformat, ParamsAmount, ParamsFormat2};
prepare_input_dataformat(undefined) ->
    undefined.

replace_format(F = #format{datatype=?TDS_TYPE_INT1}) -> 
    F#format{
        datatype=?TDS_TYPE_INTN,
        datatype_group=variable, 
        datatype_max_len=1
    };
replace_format(F = #format{datatype=?TDS_TYPE_INT2}) -> 
    F#format{
        datatype=?TDS_TYPE_INTN,
        datatype_group=variable, 
        datatype_max_len=2
    };
replace_format(F = #format{datatype=?TDS_TYPE_INT4}) -> 
    F#format{
        datatype=?TDS_TYPE_INTN,
        datatype_group=variable, 
        datatype_max_len=4
    };
replace_format(F = #format{datatype=?TDS_TYPE_INT8}) -> 
    F#format{
        datatype=?TDS_TYPE_INTN,
        datatype_group=variable, 
        datatype_max_len=8
    };
replace_format(F = #format{datatype=?TDS_TYPE_UINT2}) -> 
    F#format{
        datatype=?TDS_TYPE_UINTN,
        datatype_group=variable, 
        datatype_max_len=2
    };
replace_format(F = #format{datatype=?TDS_TYPE_UINT4}) -> 
    F#format{
        datatype=?TDS_TYPE_UINTN,
        datatype_group=variable, 
        datatype_max_len=4
    };
replace_format(F = #format{datatype=?TDS_TYPE_UINT8}) -> 
    F#format{
        datatype=?TDS_TYPE_UINTN,
        datatype_group=variable, 
        datatype_max_len=8
    };
replace_format(F = #format{datatype=?TDS_TYPE_FLT4}) -> 
    F#format{
        datatype=?TDS_TYPE_FLTN,
        datatype_group=variable, 
        datatype_max_len=4
    };
replace_format(F = #format{datatype=?TDS_TYPE_FLT8}) -> 
    F#format{
        datatype=?TDS_TYPE_FLTN,
        datatype_group=variable, 
        datatype_max_len=8
    };
replace_format(F = #format{datatype=?TDS_TYPE_SHORTDATE}) -> 
    F#format{
        datatype=?TDS_TYPE_DATETIMEN,
        datatype_group=variable, 
        datatype_max_len=4
    };
replace_format(F = #format{datatype=?TDS_TYPE_DATETIME}) -> 
    F#format{
        datatype=?TDS_TYPE_DATETIMEN,
        datatype_group=variable, 
        datatype_max_len=8
    };
replace_format(F = #format{datatype=?TDS_TYPE_DATE}) -> 
    F#format{
        datatype=?TDS_TYPE_DATEN,
        datatype_group=variable, 
        datatype_max_len=4
    };
replace_format(F = #format{datatype=?TDS_TYPE_TIME}) -> 
    F#format{
        datatype=?TDS_TYPE_TIMEN,
        datatype_group=variable, 
        datatype_max_len=4
    };
replace_format(F = #format{datatype=?TDS_TYPE_SHORTMONEY}) -> 
    F#format{
        datatype=?TDS_TYPE_MONEYN,
        datatype_group=variable, 
        datatype_max_len=4
    };
replace_format(F = #format{datatype=?TDS_TYPE_MONEY}) -> 
    F#format{
        datatype=?TDS_TYPE_MONEYN,
        datatype_group=variable, 
        datatype_max_len=8
    };
replace_format(F = #format{datatype=?TDS_TYPE_TEXT}) ->
    F#format{
        datatype=?TDS_TYPE_LONGCHAR, 
        datatype_group=long,
        datatype_max_len=32768
    };
replace_format(F = #format{datatype=?TDS_TYPE_UNITEXT}) ->
    F#format{
        datatype=?TDS_TYPE_LONGCHAR, 
        datatype_group=long,
        datatype_max_len=32768
    };
replace_format(F) -> 
    F.
