-define(JAMDB_REQ_CAP, [

    %% byte 12
%    ?TDS_REQ_MIGRATE                %% 89:Client can be migrated to another server

    %% byte 11
%    ?TDS_REQ_DBRPC2,                %% 87:Support for TDS_DBRPC2 token
%    ?TDS_REQ_CURINFO3,              %% 86:Support for TDS_CURINFO3 token
%    ?TDS_DATA_XML,                  %% 85:Support for XML datatype
%    ?TDS_REQ_BLOB_NCHAR_16,         %% 84:Support for BLOB subtype 0x05 (unichar) with serialization type 0
                                                %%      Replaces TDS_BLOB_NCHAR_16.
                                                %%      Added to deal with ASE coding issue in old servers
    ?TDS_REQ_LARGEIDENT,            %% 83:Support for large identifiers
    ?TDS_DATA_SINT1,                %% 82:Support for 1 byte signed integer
%    ?TDS_CAP_CLUSTERFAILOVER,       %% 81:Support Cluster Failover Extensions
    ?TDS_DATA_UNITEXT,              %% 80:Support for Unicode UTF-16 Text

    %% byte 10
    ?TDS_REQ_SRVPKTSIZE,            %% 79:Support for server specified packet size
%    ?TDS_CSR_KEYSETDRIVEN,          %% 78:Support for Scrollable Keysetdriven Cursor
%    ?TDS_CSR_SEMISENSITIVE,         %% 77:Support for Scrollable Semisensitive Cursor
%    ?TDS_CSR_INSENSITIVE,           %% 76:Support for Scrollable Insensitive Cursor
%    ?TDS_CSR_SENSITIVE,             %% 75:Support for Scrollable Sensitive Cursor
%    ?TDS_CSR_SCROLL,                %% 74:Support for Scrollable Cursor. This bit must be 
                                    %%      on for the following four capability bits to have meaning
    ?TDS_DATA_INTERVAL,             %% 73:Support for Interval
    ?TDS_DATA_TIME,                 %% 72:Support for Time

    %% byte 9
    ?TDS_DATA_DATE,                 %% 71:Support for Date
%    ?TDS_BLOB_NCHAR_SCSU,           %% 70:Support for BLOB subtype 0x05 (unichar) with serialization type 2
%    ?TDS_BLOB_NCHAR_8,              %% 69:Support for BLOB subtype 0x05 (unichar) with serialization type 1
%    ?TDS_BLOB_NCHAR_16,             %% 68:Support for BLOB subtype 0x05 (unichar) with serialization type 0
    ?TDS_IMAGE_NCHAR,               %% 67:Support for IMAGE data containing 
                                    %%      UTF-16 encoded data (USER_TYPE_UNITEXT)
    ?TDS_DATA_NLBIN,                %% 66:Support for LONGBINARY data containing 
                                    %%      UTF-16 encoded data (USER_TYPE_UNICHAR and USER_TYPE_UNIVARCHAR)
%    ?TDS_CUR_IMPLICIT,              %% 65:Support for TDS_CUR_DOPT_IMPLICIT cursor declare option
    ?TDS_DATA_UINTN,                %% 64:Support for NULL unsigned integers

    %% byte 8
    ?TDS_DATA_UINT8,                %% 63:Support for unsigned 8-byte integers
    ?TDS_DATA_UINT4,                %% 62:Support for unsigned 4-byte integers
    ?TDS_DATA_UINT2,                %% 61:Support for unsigned 2-byte integers
    ?TDS_WIDETABLE,                 %% 59:The client may send requests using the CURDECLARE2, DYNAMIC2, PARAMFMT2 tokens
%    ?TDS_DATA_COLUMNSTATUS,         %% 58:Indicates that a one-byte status field can follow any length or data (etc.) 
                                    %%      for every column within a row using TDS_ROW or TDS_PARAMS.
                                    %%      Note that when this capability is on, the ROWFMT* and PARAMFMT* tokens 
                                    %%      indicate in their status byte fields whether a particular column will 
                                    %%      contain the columnstatus byte.
%    ?TDS_OBJECT_BINARY,             %% 57:Streaming Binary data

    %% byte 7
%    ?TDS_OBJECT_CHAR,               %% 55:Support Streaming character data
%    ?TDS_OBJECT_JAVA1,              %% 54:Support Serialized Java Objects
%    ?TDS_DOL_BULK,                  %% 53:?
%    ?TDS_DATA_VOID,                 %% 52:?
    ?TDS_DATA_INT8,                 %% 51:Support 8 byte integers
    ?TDS_DATA_BITN,                 %% 50:Support NULL bits
    ?TDS_DATA_FLTN,                 %% 49:Support NULL floats
    ?TDS_PROTO_DYNPROC,             %% 48:Pre-pend “create proc” to dynamic prepare statements

    %% byte 6
%    ?TDS_PROTO_DYNAMIC,             %% 47:Use DESCIN/DESCOUT dynamic protocol
%    ?TDS_DATA_BOUNDARY,             %% 46:Support boundary security data types
%    ?TDS_DATA_SENSITIVITY,          %% 45:Support sensitivity security data types
%    ?TDS_REQ_URGEVT,                %% 44:Use new event notification protocol
%    ?TDS_PROTO_BULK,                %% 43:Support tokenized bulk copy (not supported this release)
%    ?TDS_PROTO_TEXT,                %% 42:Support tokenized text and image (not supported in this release)
    ?TDS_CON_LOGICAL,               %% 41:Support logical logout
%    ?TDS_CON_INBAND,                %% 40:Support non-expedited attentions


    %% byte 5
%    ?TDS_CON_OOB,                   %% 39:Support expedited attentions
%    ?TDS_CSR_MULTI,                 %% 38:This is possibly obsolete
%    ?TDS_CSR_REL,                   %% 37:Obsolete, will not be used
%    ?TDS_CSR_ABS,                   %% 36:Obsolete, will not be used
%    ?TDS_CSR_LAST,                  %% 35:Obsolete, will not be used
%    ?TDS_CSR_FIRST,                 %% 34:Obsolete, will not be used
%    ?TDS_CSR_PREV,                  %% 33:Obsolete, will not be used
    ?TDS_DATA_MONEYN,               %% 32:Support NULL money

    %% byte 4
    ?TDS_DATA_DATETIMEN,            %% 31:Support NULL date/time
    ?TDS_DATA_INTN,                 %% 30:Support NULL integers
    ?TDS_DATA_LBIN,                 %% 29:Support long variable length binary data types.
    ?TDS_DATA_LCHAR,                %% 28:Support long variable length character data types
    ?TDS_DATA_DEC,                  %% 27:Support decimal data types
    ?TDS_DATA_IMAGE,                %% 26:Support image data types
    ?TDS_DATA_TEXT,                 %% 25:Support text data types
    ?TDS_DATA_NUM,                  %% 24:Support numeric data types

    %% byte 3
    ?TDS_DATA_FLT8,                 %% 23:Support 8 byte floating point data types
    ?TDS_DATA_FLT4,                 %% 22:Support 4 byte floating point data types
    ?TDS_DATA_DATE4,                %% 21:Support 4 byte date/time data types
    ?TDS_DATA_DATE8,                %% 20:Support 8 byte date/time data types
    ?TDS_DATA_MNY4,                 %% 19:Support 4 byte money data types
    ?TDS_DATA_MNY8,                 %% 18:Support 8 byte money data types
    ?TDS_DATA_VBIN,                 %% 17:Support variable length binary data types
    ?TDS_DATA_BIN,                  %% 16:Support fixed length character data types

    %% byte 2
    ?TDS_DATA_VCHAR,                %% 15:Support variable length character data types
    ?TDS_DATA_CHAR,                 %% 14:Support fixed length character data types
    ?TDS_DATA_BIT,                  %% 13:Support bit data types
    ?TDS_DATA_INT4,                 %% 12:Support 4 byte integers
    ?TDS_DATA_INT2,                 %% 11:Support 2 byte integers
    ?TDS_DATA_INT1,                 %% 10:Support 1 byte unsigned integers
    ?TDS_REQ_PARAM,                 %% 09:RPC requests will use the TDS_DBRPC token and TDS_PARAMFMT / TDS_PARAM 
                                    %%      to send parameters.
    ?TDS_REQ_MSG,                   %% 08:TDS_MSG requests
 
    %% byte 1
    ?TDS_REQ_DYNF,                  %% 07:Dynamic SQL requests
    ?TDS_REQ_CURSOR,                %% 06:Cursor command requests
%    ?TDS_REQ_BCP,                   %% 05:Bulk copy requests
    ?TDS_REQ_MSTMT,                 %% 04:Support multiple commands per request
%    ?TDS_REQ_EVT,                   %% 03:Registered procedure event notification
    ?TDS_REQ_RPC,                   %% 02:RPC requests
    ?TDS_REQ_LANG                   %% 01:Language requests
]).

-define(JAMDB_RESP_CAP, [

    %% byte 9
%    ?TDS_RES_FORCE_ROWFMT2          %% 64:Force usage of TDS_ROWFMT2 even if TDS_ROWFMT could be used

    %% byte 8
%    ?TDS_RES_SUPPRESS_DONEINPROC    %% 63:Server can suppress TDS_DONEINPROC tokens
                                    %% This allows TDS_ROW to be followed by TDS_ROWFMT(2) instead of 
                                    %% TDS_DONEINPROC / TDS_ROWFMT(2)
                                    %% Final done count will be provided by TDS_DONEPROC
%    ?TDS_RES_SUPPRESS_FMT,          %% 62:Server will suppress TDS_ROWFMT (or TDS_ROWFMT2) tokens 
                                    %%      if the appropriate status bit is set in TDS_DYNAMIC
%    ?TDS_RES_NOXNLDATA,             %% 61:Client does not need additional metadata in TDS_ROWFMT2
                                    %%      Also, server should use TDS_ROWFMT instead of TDS_ROWFMT2
%    ?TDS_NONINT_RETURN_VALUE,       %% 60:Client support for non-integer return values from TDS_RETURNVALUE
    ?TDS_DATA_NOXML,                %% 59:No Support for XML datatype
%    ?TDS_NO_SRVPKTSIZE,             %% 58:No Support for Server specified packet size larger than the client requested
                                    %%      If the TDS_REQ_PKTSIZE capability is set, the server may still specify 
                                    %%      the packet size as long as it is not larger than suggested by the client
    ?TDS_NO_BLOB_NCHAR_16,          %% 57:No Support for BLOB subtype 0x05/0. Replaces TDS_BLOB_NONCHAR_16
                                    %%      Added to work around ASE coding issue
%    ?TDS_NO_LARGEIDENT,             %% 56:No Support for Large Identifiers

    %% byte 7
%    ?TDS_DATA_NOSINT1,              %% 55:No Support for 1 byte signed integers
%    ?TDS_DATA_NOUNITEXT,            %% 54:No Support for Unicode UTF-16 Text
%    ?TDS_DATA_NOINTERVAL,           %% 53:No Support for Interval
%    ?TDS_DATA_NOTIME,               %% 52:No Support for Time
%    ?TDS_DATA_NODATE,               %% 51:No Support for Date
    ?TDS_BLOB_NONCHAR_SCSU,         %% 50:No Support for BLOB subtype 0x05/2
    ?TDS_BLOB_NONCHAR_8,            %% 49:No Support for BLOB subtype 0x05/1
    ?TDS_BLOB_NONCHAR_16,           %% 48:No Support for BLOB subtype 0x05/0

    %% byte 6
%    ?TDS_IMAGE_NONCHAR,             %% 47:No Support for IMAGE data containing 
                                     %%      UTF-16 encoded data (USER_TYPE_UNITEXT)
%    ?TDS_DATA_NONLBIN,              %% 46:No Support for LONGBINARY data containing 
                                    %%      UTF-16 encoded data (USER_TYPE_UNICHAR and USER_TYPE_UNIVARCHAR)
%    ?TDS_NO_WIDETABLES,             %% 45:Client cannot process the TDS_ORDERBY2, TDS_PARAMFMT2, and TDS_ROWFMT2 
                                     %%      tokens required to support tables with a LARGE number of columns
                                     %%      The server should not send them.
%    ?TDS_DATA_NOUINTN,              %% 44:No Support for NULL unsigned integers
%    ?TDS_DATA_NOUINT8,              %% 43:No Support for unsigned 8-byte integers
%    ?TDS_DATA_NOUINT4,              %% 42:No Support for unsigned 4-byte integers
%    ?TDS_DATA_NOUINT2,              %% 41:No Support for unsigned 2-byte integers

    %% byte 5
%    ?TDS_OBJECT_NOBINARY,           %% 39:No Streaming Binary data
%    ?TDS_DATA_NOCOLUMNSTATUS,       %% 38:No Support for the columnstatus byte
%    ?TDS_OBJECT_NOCHAR,             %% 37:No Support Streaming character data
%    ?TDS_OBJECT_NOJAVA1,            %% 36:No Support Serialized Java Objects
%    ?TDS_DATA_NOINT8,               %% 35:No support for 8 byte integers
    ?TDS_RES_NOSTRIPBLANKS,         %% 34:Do not strip blank from fixed length character data
    ?TDS_RES_NOTDSDEBUG,            %% 33:No support for TDS_DEBUG token. Use image data instead
    ?TDS_DATA_NOBOUNDARY,           %% 32:No support for the security boundary data type

    %% byte 4
    ?TDS_DATA_NOSENSITIVITY,        %% 31:No support for the security sensitivity data type
    ?TDS_PROTO_NOBULK,              %% 30:No support for tokenized bulk copy
%    ?TDS_PROTO_NOTEXT,              %% 29:No support for tokenized text and image
    ?TDS_CON_NOINBAND,              %% 28:No support for non-expedited attentions
    ?TDS_CON_NOOOB                  %% 27:No support for expedited attentions
%    ?TDS_DATA_NOMONEYN,             %% 26:No support for nullable money data types
%    ?TDS_DATA_NODATETIMEN,          %% 25:No support for nullable date/time data types
%    ?TDS_DATA_NOINTN,               %% 24:No support for nullable integers

    %% byte 3
%    ?TDS_DATA_NOLBIN,               %% 23:No support for long variable length binary data types
%    ?TDS_DATA_NOLCHAR,              %% 22:No support for long variable length character data types
%    ?TDS_DATA_NODEC,                %% 21:No support for decimal data types
%    ?TDS_DATA_NOIMAGE,              %% 20:No support for image data types
%    ?TDS_DATA_NOTEXT,               %% 19:No support for text data types
%    ?TDS_DATA_NONUM,                %% 18:No support for numeric data types
%    ?TDS_DATA_NOFLT8,               %% 17:No support for 8 byte float data types
%    ?TDS_DATA_NOFLT4,               %% 16:No support for 4 byte float data types

    %% byte 2
%    ?TDS_DATA_NODATE4,              %% 15:No support for 4 byte date/time data types
%    ?TDS_DATA_NODATE8,              %% 14:No support for 8 byte date/time data types
%    ?TDS_DATA_NOMNY4,               %% 13:No support for 4 byte money data types
%    ?TDS_DATA_NOMNY8,               %% 12:No support for 8 byte money data types
%    ?TDS_DATA_NOVBIN,               %% 11:No support for variable length binary data types
%    ?TDS_DATA_NOBIN,                %% 10:No support for fixed length binary data types
%    ?TDS_DATA_NOVCHAR,              %% 09:No support for variable length character data types
%    ?TDS_DATA_NOCHAR,               %% 08:No support for fixed length character data types


    %% byte 1
%    ?TDS_DATA_NOBIT,                %% 07:No support for bit data types
%    ?TDS_DATA_NOINT4,               %% 06:No support for 4 byte integers
%    ?TDS_DATA_NOINT2,               %% 05:No support for 2 byte integers
%    ?TDS_DATA_NOINT1,               %% 04:No support for 1 byte unsigned integers
%    ?TDS_RES_NOPARAM,               %% 03:No support for TDS_PARAM / TDS_PARAMFMT for return parameter. 
                                     %%      Use TDS_RETURNVALUE to return parameters to this client.
%    ?TDS_RES_NOEED,                 %% 02:No support for TDS_EED token
%    ?TDS_RES_NOMSG                  %% 01:No support for TDS_MSG results
]).

-record(format, {
    column_name = <<>>,
    status,
    usertype,
    datatype,
    datatype_group,
    datatype_max_len,
    datatype_precision,
    datatype_scale,
    datatype_locale,
    datatype_class_id,
    datatype_name,
    db_name,
    owner_name,
    table_name,
    label_name  = <<>>
}).

-record(message, {
    msg_number, 
    msg_state, 
    class, 
    sql_state,
    status, 
    transaction_state, 
    msg_body, 
    server_name, 
    procedure_name, 
    line_number
}).
