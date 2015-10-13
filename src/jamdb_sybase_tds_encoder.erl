-module(jamdb_sybase_tds_encoder).

%% API
-export([encode_packet/3]).
-export([encode_login_record/1]).
-export([encode_token/1]).

-include("TDS_5_0.hrl").
-include("jamdb_sybase.hrl").

%% API
encode_packet(Type, PacketSize, Data) ->
    DataSize = PacketSize - 8,
    case Data of
        <<PacketData:DataSize/binary, Rest/binary>> ->
            {<<Type:8, 0:8, PacketSize:16, 0:32, PacketData/binary>>, Rest};
        _ ->
            LastPacketSize = byte_size(Data) + 8,
            {<<Type:8, 1:8, LastPacketSize:16, 0:32, Data/binary>>, <<>>}
    end.

encode_login_record(EnvOpts) ->
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

encode_token(Token) ->
    case Token of
        {language, Query} ->
            encode_language_token(Query);
        {dynamic, Type, Status, Id, Stmt} ->
            encode_dynamic_token(Type, Status, Id, Stmt);
        {dbrpc, RpcName, Options} ->
            encode_dbrpc_token(RpcName, Options);
        {logout, Opts} ->
            encode_logout_token(Opts)
    end.

encode_logout_token(_Opts) ->
    <<
        ?TDS_TOKEN_LOGOUT, 
        0   %% Options: no options
    >>.

encode_language_token(Query) ->
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
encode_dynamic_token(Type, Status, Id, Stmt) ->
    StmtLen = byte_size(Stmt),
    IdLen = byte_size(Id),
    case (StmtLen < 32765 - IdLen) of
        true ->
            <<
                ?TDS_TOKEN_DYNAMIC,
                (5 + IdLen + StmtLen):16,   %% Token Length
                Type:8,                     %% type of dynamic operation
                (encode_bit_mask(Status)):8,%% status
                IdLen:8,                    %% Length of statement id
                Id:IdLen/binary,            %% statement id
                StmtLen:16,                 %% Length of statement
                Stmt:StmtLen/binary         %% statement
            >>;
        false ->
            <<
                ?TDS_TOKEN_DYNAMIC2,
                (7 + IdLen + StmtLen):32,   %% Token Length
                Type:8,                     %% type of dynamic operation
                (encode_bit_mask(Status)):8,%% status
                IdLen:8,                    %% Length of statement id
                Id:IdLen/binary,            %% statement id
                StmtLen:32,                 %% Length of statement
                Stmt:StmtLen/binary         %% statement
            >>
    end.

%-define(ENCODE_RPC_PARAM(ParamsList), <<
%    (encode_data(varchar, ParamName, 1)),   %% ParamName
%    0:8,                                    %% Status: status argument is not used
%    DataType:8,
%    MaxLen
%>>).
%
%-define(ENCODE_TOKEN_RPC(RpcName, ParamsList), <<
%    ?TDS_RPC,
%    TokenLength:16,                     %% Token Length
%    (encode_data(varchar, RpcName, 1)), %% RPC Name
%    0:16,                               %% Options: no options
%    ?ENCODE_RPC_PARAMS(ParamsList)
%>>).

%% The TDS_DBRPC token will be used by clients if the TDS_REQ_PARAM capability bit is true
%% The TDS_DBRPC2 token will be used by clients only if the TDS_REQ_DBRPC2 capability bit is true.
%% Return parameters will be returned to a client using the TDS_PARAMFMT/PARAMS tokens if the TDS_RES_NOPARAM capability bit is false.
encode_dbrpc_token(RpcName, Options) ->
    RpcNameLen = byte_size(RpcName),
    case RpcNameLen < 255 of
        true ->
            <<
                ?TDS_TOKEN_DBRPC,
                (3 + RpcNameLen):16,            %% Token Length
                RpcNameLen:8,                   %% RPC name length, in bytes
                RpcName:RpcNameLen/binary,      %% RPC Name
                (encode_bit_mask(Options)):16   %% Options bit mask
            >>;
        false ->
            <<
                ?TDS_TOKEN_DBRPC2,
                (4 + RpcNameLen):16,            %% Token Length
                RpcNameLen:16,                  %% RPC name length, in bytes
                RpcName:RpcNameLen/binary,      %% RPC Name
                (encode_bit_mask(Options)):16   %% Options
            >>
    end.

%-define(ENCODE_TOKEN_PARAMFMT(ParamsNumber, ParamsFormat),
%begin
%    ParamsLen = byte_size(ParamsList),
%    case ParamsLen < 65280 of
%        true ->
%            ?ENCODE_TOKEN_PARAMFMT2(ParamsNumber, ParamsLen, ParamsFormat);
%        false ->
%            ?ENCODE_TOKEN_PARAMFMT4(ParamsNumber, ParamsLen, ParamsFormat)
%    end
%end
%).
%
%-define(ENCODE_TOKEN_PARAMFMT2(ParamsNumber, ParamsLen, ParamsFormat), <<
%    ?TDS_TOKEN_PARAMFMT,
%    (1 + ParamsLen):16,                 %% This length specifies the number of bytes remaining in the datastream
%    ParamsNumber:16                     %% This argument specifies the number of parameters being described
%    ParamsFormat:ParamsLen/binary
%>>).
%
%-define(ENCODE_TOKEN_PARAMFMT4(ParamsNumber, ParamsLen, ParamsFormat), <<
%    ?TDS_TOKEN_PARAMFMT2,
%    (1 + ParamsLen):32,                 %% This length specifies the number of bytes remaining in the datastream
%    ParamsNumber:16                     %% This argument specifies the number of parameters being described
%    ParamsFormat:ParamsLen/binary
%>>).
%
%-define(ENCODE_TOKEN_PARAMS(ParamsList, FormatList), <<
%    ?TDS_TOKEN_PARAMS,
%    <<(encode_data(SQLType, Value, Size)) || 
%        {SQLType, Value, Size} <- ParamsList>>/binary
%>>).


%% internal
encode_lrempw(Password) ->
    <<
    0:8,    %% ServerName length: zero mean universal password
    (encode_data(Password, ?USER_TYPE_VARCHAR, 1))/binary   %% Password
    >>.

encode_token_capability() ->
    <<
        ?TDS_TOKEN_CAPABILITY,
        27:16,  %% Token Length
    %% Header %%%%%%%%%%%%%%%%%%%%%%%
        1,      %% Capabilities type: request 
        14,     %% Request capabilities length: 14 bytes
    %% Header %%%%%%%%%%%%%%%%%%%%%%%
    
        0:1,    %% 111:?
        0:1,    %% 110:?
        0:1,    %% 109:?
        0:1,    %% 108:?
        0:1,    %% 107:?
        0:1,    %% 106:?
        0:1,    %% 105:?
        0:1,    %% 104:?
    
        0:1,    %% 103:?
        0:1,    %% 102:?
        0:1,    %% 101:?
        0:1,    %% 100:?
        0:1,    %% 99:?
        0:1,    %% 98:?
        0:1,    %% 97:?
        0:1,    %% 96:?
    
        0:1,    %% 95:?
        0:1,    %% 94:?
        0:1,    %% 93:?
        0:1,    %% 92:?
        0:1,    %% 91:?
        0:1,    %% 90:?
        0:1,    %% 89:Client can be migrated to another server
        0:1,    %% 88:?
    
        0:1,    %% 87:Support for TDS_DBRPC2 token
        0:1,    %% 86:Support for TDS_CURINFO3 token
        0:1,    %% 85:Support for XML datatype
        0:1,    %% 84:Support for BLOB subtype 0x05 (unichar) with serialization type 0
                %%      Replaces TDS_BLOB_NCHAR_16. Added to deal with ASE coding issue in old servers.
        1:1,    %% 83:Support for large identifiers
        1:1,    %% 82:Support for 1 byte signed integer
        0:1,    %% 81:Support Cluster Failover Extensions
        1:1,    %% 80:Support for Unicode UTF-16 Text
    
        1:1,    %% 79:Support for server specified packet size (CS_REQ_PKTSIZE)
        0:1,    %% 78:Support for Scrollable Keyset-driven Cursor
        0:1,    %% 77:Support for Scrollable Semi-sensitive Cursor
        0:1,    %% 76:Support for Scrollable Insensitive Cursor
        0:1,    %% 75:Support for Scrollable Sensitive Cursor
        0:1,    %% 74:Support for Scrollable Cursor
                %%      This bit must be on for the following four capability bits to have meaning.
        1:1,    %% 73:Support for Interval
        1:1,    %% 72:Support for Time
    
        1:1,    %% 71:Support for Date
        0:1,    %% 70:Support for BLOB subtype 0x05 (unichar) with serialization type 2
        0:1,    %% 69:Support for BLOB subtype 0x05 (unichar) with serialization type 1
        0:1,    %% 68:Support for BLOB subtype 0x05 (unichar) with serialization type 0
        1:1,    %% 67:Support for IMAGE data containing UTF-16 encoded data (usertype 36)
        1:1,    %% 66:Support for LONGBINARY data containing UTF-16 encoded data (usertypes 34 and 35)
        0:1,    %% 65:Support for TDS_CUR_DOPT_IMPLICIT cursor declare option
        1:1,    %% 64:Support for NULL unsigned integers
    
        1:1,    %% 63:Support for unsigned 8-byte integers
        1:1,    %% 62:Support for unsigned 4-byte integers
        1:1,    %% 61:Support for unsigned 2-byte integers
        0:1,    %% 60:Reserved
        1:1,    %% 59:The client may send requests using the CURDECLARE2, 
                %%      DYNAMIC2, PARAMFMT2 tokens (CAP_WIDETABLE)
        0:1,    %% 58:Indicates that a one-byte status field can follow any length or data (etc.)
                %%      for every column within a row using TDS_ROW or TDS_PARAMS. 
                %%      Note that when this capability is on, the ROWFMT* and PARAMFMT* tokens 
                %%      indicate in their status byte fields whether a particular column will 
                %%      contain the columnstatus byte (CAP_DATA_COLUMNSTATUS)
        0:1,    %% 57:Streaming Binary data
        0:1,    %% 56:Reserved for future use
    
        0:1,    %% 55:Support Streaming character data
        0:1,    %% 54:Support Serialized Java Objects
        0:1,    %% 53:? (CS_DOL_BULK)
        0:1,    %% 52:? (CS_DATA_VOID)
        1:1,    %% 51:Support 8 byte integers (CS_DATA_INT8)
        1:1,    %% 50:Support NULL bits (CS_DATA_BITN)
        1:1,    %% 49:Support NULL floats (CS_DATA_FLTN)
        1:1,    %% 48:Pre-pend “create proc” to dynamic prepare statements (CS_PROTO_DYNPROC)
    
        1:1,    %% 47:Use DESCIN/DESCOUT dynamic protocol (CS_PROTO_DYNAMIC)
        0:1,    %% 46:Support boundary security data types (CS_DATA_BOUNDARY)
        0:1,    %% 45:Support sensitivity security data types (CS_DATA_SENSITIVITY)
        0:1,    %% 44:Use new event notification protocol (CS_REQ_URGNOTIF)
        0:1,    %% 43:Support tokenized bulk copy (CS_PROTO_BULK)
        0:1,    %% 42:Support tokenized text and image (CS_PROTO_TEXT)
        1:1,    %% 41:Support logical logout (CS_CON_LOGICAL)
        0:1,    %% 40:Support non-expedited attentions (CS_CON_INBAND)
    
        0:1,    %% 39:Support expedited attentions (CS_CON_OOB)
        0:1,    %% 38:This is possibly obsolete (CS_CSR_MULTI)
        0:1,    %% 37:Obsolete, will not be used (CS_CSR_REL)
        0:1,    %% 36:Obsolete, will not be used (CS_CSR_ABS)
        0:1,    %% 35:Obsolete, will not be used (CS_CSR_LAST)
        0:1,    %% 34:Obsolete, will not be used (CS_CSR_FIRST)
        0:1,    %% 33:Obsolete, will not be used (CS_CSR_PREV)
        1:1,    %% 32:Support NULL money (CS_DATA_MONEYN)
    
        1:1,    %% 31:Support NULL date/time (CS_DATA_DATETIMEN)
        1:1,    %% 30:Support NULL integers (CS_DATA_INTN)
        1:1,    %% 29:Support long variable length binary data types (CS_DATA_LBIN)
        1:1,    %% 28:Support long variable length character data types (CS_DATA_LCHAR)
        1:1,    %% 27:Support decimal data types (CS_DATA_DEC)
        1:1,    %% 26:Support image data types (CS_DATA_IMAGE)
        1:1,    %% 25:Support text data types (CS_DATA_TEXT)
        1:1,    %% 24:Support numeric data types (CS_DATA_NUM)
    
        1:1,    %% 23:Support 8 byte floating point data types (CS_DATA_FLT8)
        1:1,    %% 22:Support 4 byte floating point data types (CS_DATA_FLT4)
        1:1,    %% 21:Support 4 byte date/time data types (CS_DATA_DATE4)
        1:1,    %% 20:Support 8 byte date/time data types (CS_DATA_DATE8)
        1:1,    %% 19:Support 4 byte money data types (CS_DATA_MNY4)
        1:1,    %% 18:Support 8 byte money data types (CS_DATA_MNY8)
        1:1,    %% 17:Support variable length binary data types (CS_DATA_VBIN)
        1:1,    %% 16:Support fixed length binary data types (CS_DATA_BIN)
    
        1:1,    %% 15:Support variable length character data types (CS_DATA_VCHAR)
        1:1,    %% 14:Support fixed length character data types (CS_DATA_CHAR)
        1:1,    %% 13:Support bit data types (CS_DATA_BIT)
        1:1,    %% 12:Support 4 byte integers (CS_DATA_INT4)
        1:1,    %% 11:Support 2 byte integers (CS_DATA_INT2)
        1:1,    %% 10:Support 1 byte unsigned integers (CS_DATA_INT1)
        1:1,    %% 09:RPC requests will use the TDS_DBRPC token 
                %%      and TDS_PARAMFMT/TDS_PARAM to send parameters (CS_REQ_PARAM)
        1:1,    %% 08:TDS_MSG requests (CS_REQ_MSG)
    
        1:1,    %% 07:Dynamic SQL requests (CS_REQ_DYN)
        1:1,    %% 06:Cursor command requests (CS_REQ_CURSOR)
        0:1,    %% 05:Bulk copy requests (CS_REQ_BCP)
        1:1,    %% 04:Support multiple commands per request (CS_REQ_MSTMT)
        0:1,    %% 03:Registered procedure event notification (CS_REQ_NOTIF)
        1:1,    %% 02:RPC requests (CS_REQ_RPC)
        1:1,    %% 01:Language requests (CS_REQ_LANG)
        0:1,    %% 00:(bits shifted to one)
    
    %% Header %%%%%%%%%%%%%%%%%%%%%%%
        2,      %% Capabilities type: response
        9,      %% Response capabilities length: 9 bytes
    %% Header %%%%%%%%%%%%%%%%%%%%%%%
    
        0:1,    %% 71:?
        0:1,    %% 70:?
        0:1,    %% 69:?
        0:1,    %% 68:?
        0:1,    %% 67:?
        0:1,    %% 66:?
        0:1,    %% 65:?
        0:1,    %% 64:Force usage of TDS_ROWFMT2 even if TDS_ROWFMT could be used
    
        1:1,    %% 63:Server can suppress TDS_DONEINPROC tokens
                %%      This allows TDS_ROW to be followed by TDS_CAP_ROWFMT(2) 
                %%      instead of TDS_DONEINPROC/TDS_ROWFMT(2)
                %%      Final done count will be provided by TDS_CAP_DONEPROC
        0:1,    %% 62:Server will suppress TDS_ROWFMT (or TDS_ROWFMT2) tokens
                %%      if the appropriate status bit is set in CAP_DYNAMIC
        0:1,    %% 61:Client does not need additional metadata in TDS_ROWFMT2
                %%      Also, server should use TDS_CAP_ROWFMT instead of TDS_ROWFMT2
        1:1,    %% 60:Client support for non-integer return values from TDS_RETURNVALUE
        1:1,    %% 59:No Support for XML datatype
        0:1,    %% 58:No Support for Server specified packet size larger than the client requested
                %%      If the CS_REQ_PKTSIZE capability is set, the server may still specify 
                %%      the packet size as long as it is not larger than suggested by the client
        0:1,    %% 57:No Support for BLOB subtype 0x05/0
                %%      Replaces CAP_BLOB_NONCHAR_16
                %%      Added to work around ASE coding issue
        0:1,    %% 56:No Support for Large Identifiers
    
        0:1,    %% 55:No Support for 1 byte signed integers
        0:1,    %% 54:No Support for Unicode UTF-16 Text
        0:1,    %% 53:No Support for Interval
        0:1,    %% 52:No Support for Time
        0:1,    %% 51:No Support for Date
        0:1,    %% 50:No Support for BLOB subtype 0x05/2
        0:1,    %% 49:No Support for BLOB subtype 0x05/1
        1:1,    %% 48:No Support for BLOB subtype 0x05/0 (CS_BLOB_NONCHAR_16)
    
        0:1,    %% 47:No Support for IMAGE data containing UTF-16 encoded data (usertype 36)
        0:1,    %% 46:No Support for LONGBINARY data containing UTF-16 encoded data (usertypes 34 and 35)
        0:1,    %% 45:Client cannot process the TDS_ORDERBY2, TDS_PARAMFMT2, and TDS_ROWFMT2 tokens
                %%      required to support tables with a LARGE number of columns. 
                %%      The server should not send them (CS_NO_WIDETABLES)
        0:1,    %% 44:No Support for NULL unsigned integers
        0:1,    %% 43:No Support for unsigned 8-byte integers
        0:1,    %% 42:No Support for unsigned 4-byte integers
        0:1,    %% 41:No Support for unsigned 2-byte integers
        0:1,    %% 40:Reserved for future use
    
        0:1,    %% 39:No Support Streaming binary data
        0:1,    %% 38:No Support for the columnstatus byte
        0:1,    %% 37:No Support Streaming character data
        0:1,    %% 36:No Support Serialized Java Objects
        0:1,    %% 35:No support for 8 byte integers
        1:1,    %% 34:Do not strip blank from fixed length character data
        0:1,    %% 33:No support for TDS_DEBUG token. Use image data instead
        0:1,    %% 32:No support for the security boundary data type
    
        1:1,    %% 31:No support for the security sensitivity data type
        0:1,    %% 30:No support for tokenized bulk copy
        0:1,    %% 29:No support for tokenized text and image
        1:1,    %% 28:No support for non-expedited attentions
        0:1,    %% 27:No support for expedited attentions
        0:1,    %% 26:No support for nullable money data types
        0:1,    %% 25:No support for nullable date/time data types
        0:1,    %% 24:No support for nullable integers
    
        0:1,    %% 23:No support for long variable length binary data types
        0:1,    %% 22:No support for long variable length character data types
        0:1,    %% 21:No support for decimal data types
        0:1,    %% 20:No support for image data types
        0:1,    %% 19:No support for text data types
        0:1,    %% 18:No support for numeric data types
        0:1,    %% 17:No support for 8 byte float data types
        0:1,    %% 16:No support for 4 byte float data types
    
        0:1,    %% 15:No support for 4 byte date/time data types
        0:1,    %% 14:No support for 8 byte date/time data types
        0:1,    %% 13:No support for 4 byte money data types
        0:1,    %% 12:No support for 8 byte money data types
        0:1,    %% 11:No support for variable length binary data types
        0:1,    %% 10:No support for fixed length binary data types
        0:1,    %% 09:No support for variable length character data types
        0:1,    %% 08:No support for fixed length character data types
    
        0:1,    %% 07:No support for bit data types
        0:1,    %% 06:No support for 4 byte integers
        0:1,    %% 05:No support for 2 byte integers
        0:1,    %% 04:No support for 1 byte unsigned integers
        0:1,    %% 03:No support for TDS_PARAM/TDS_PARAMFMT for return parameter
                %%      use TDS_CAP_RETURNVALUE to return parameters to this client
        0:1,    %% 02:No support for TDS_EED token
        0:1,    %% 01:No support for TDS_MSG results
        0:1     %% 00:(bits shifted to one)
    >>.

encode_bit_mask(List) ->
    lists:foldl(fun(X, Result) -> X bor Result end, 0, List).

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
