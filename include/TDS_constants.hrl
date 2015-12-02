
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Packet types   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_PKT_LANG, 1).
-define(TDS_PKT_LOGIN, 2).
-define(TDS_PKT_RPC, 3).
-define(TDS_PKT_RESPONSE, 4).
-define(TDS_PKT_UNFMT, 5).
-define(TDS_PKT_ATTN, 6).
-define(TDS_PKT_BULK, 7).
-define(TDS_PKT_SETUP, 8).
-define(TDS_PKT_CLOSE, 9).
-define(TDS_PKT_ERROR, 10).
-define(TDS_PKT_PROTACK, 11).
-define(TDS_PKT_ECHO, 12).
-define(TDS_PKT_LOGOUT, 13).
-define(TDS_PKT_ENDPARAM, 14).
-define(TDS_PKT_NORMAL, 15).
-define(TDS_PKT_URGENT, 16).
-define(TDS_PKT_MIGRATE, 17).
-define(TDS_PKT_CMDSEQ_NORMAL, 24).
-define(TDS_PKT_CMDSEQ_LOGIN, 25).
-define(TDS_PKT_CMDSEQ_LIVENESS, 26).
-define(TDS_PKT_CMDSEQ_RESERVED1, 27).
-define(TDS_PKT_CMDSEQ_RESERVED2, 28).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Tokens %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_TOKEN_CURDECLARE3, 16).
-define(TDS_TOKEN_PARAMFMT2, 32).
-define(TDS_TOKEN_LANGUAGE, 33).
-define(TDS_TOKEN_ORDERBY2, 34).
-define(TDS_TOKEN_CURDECLARE2, 35).
-define(TDS_TOKEN_COLFMTOLD, 42).       %% obsolete (tds 5.0 rev 3.8)
-define(TDS_TOKEN_DEBUGCMD,  96).       %% obsolete (tds 5.0 rev 3.8)
-define(TDS_TOKEN_ROWFMT2,  97).
-define(TDS_TOKEN_DYNAMIC2,  98).
-define(TDS_TOKEN_OPTIONCMD2, 99).      %% reserved (tds 5.0 rev 3.8)
-define(TDS_TOKEN_MSG,  101).
-define(TDS_TOKEN_LOGOUT, 113).
-define(TDS_TOKEN_OFFSET, 120).
-define(TDS_TOKEN_RETURNSTATUS, 121).
-define(TDS_TOKEN_PROCID, 124).         %% obsolete (tds 5.0 rev 3.8), dropped. never used.
-define(TDS_TOKEN_CURCLOSE, 128).
-define(TDS_TOKEN_CURDELETE, 129).
-define(TDS_TOKEN_CURFETCH, 130).
-define(TDS_TOKEN_CURINFO, 131).
-define(TDS_TOKEN_CUROPEN, 132).
-define(TDS_TOKEN_CURUPDATE, 133).
-define(TDS_TOKEN_CURDECLARE, 134).
-define(TDS_TOKEN_CURINFO2, 135).
-define(TDS_TOKEN_CURINFO3, 136).
-define(TDS_TOKEN_COLNAME, 160).        %% obsolete (tds 5.0 rev 3.8), replaced by rowfmt
-define(TDS_TOKEN_COLFMT, 161).         %% obsolete (tds 5.0 rev 3.8), replaced by rowfmt
-define(TDS_TOKEN_EVENTNOTICE, 162).
-define(TDS_TOKEN_TABNAME, 164).
-define(TDS_TOKEN_COLINFO, 165).        %% obsolete (tds 5.0 rev 3.8)
-define(TDS_TOKEN_OPTIONCMD, 166).
-define(TDS_TOKEN_ALTNAME, 167).
-define(TDS_TOKEN_ALTFMT, 168).
-define(TDS_TOKEN_ORDERBY, 169).
-define(TDS_TOKEN_ERROR, 170).          %% obsolete (tds 5.0 rev 3.8)
-define(TDS_TOKEN_INFO, 171).           %% obsolete (tds 5.0 rev 3.8)
-define(TDS_TOKEN_RETURNVALUE, 172).    %% obsolete (tds 5.0 rev 3.8)
-define(TDS_TOKEN_LOGINACK, 173).
-define(TDS_TOKEN_CONTROL, 174).
-define(TDS_TOKEN_ALTCONTROL, 175).     %% obsolete (tds 5.0 rev 3.8), Was never implemented.
-define(TDS_TOKEN_KEY, 202).
-define(TDS_TOKEN_ROW, 209).
-define(TDS_TOKEN_ALTROW, 211).
-define(TDS_TOKEN_PARAMS, 215).
-define(TDS_TOKEN_RPC,  224).           %% obsolete (tds 5.0 rev 3.8)
-define(TDS_TOKEN_CAPABILITY,  226).
-define(TDS_TOKEN_ENVCHANGE,  227).
-define(TDS_TOKEN_EED,  229).           %% Extended Error Data token
-define(TDS_TOKEN_DBRPC,  230).
-define(TDS_TOKEN_DYNAMIC,  231).
-define(TDS_TOKEN_DBRPC2,  232).
-define(TDS_TOKEN_PARAMFMT,  236).
-define(TDS_TOKEN_ROWFMT,  238).
-define(TDS_TOKEN_DONE,  253).
-define(TDS_TOKEN_DONEPROC,  254).
-define(TDS_TOKEN_DONEINPROC,  255).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Data types %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_TYPE_VOID, 31).
-define(TDS_TYPE_IMAGE, 34).
-define(TDS_TYPE_TEXT, 35).
-define(TDS_TYPE_BLOB, 36).
-define(TDS_TYPE_VARBINARY, 37).
-define(TDS_TYPE_INTN, 38).
-define(TDS_TYPE_VARCHAR, 39).
-define(TDS_TYPE_BINARY, 45).
-define(TDS_TYPE_INTERVAL, 46).
-define(TDS_TYPE_CHAR, 47).
-define(TDS_TYPE_INT1, 48).
-define(TDS_TYPE_DATE, 49).
-define(TDS_TYPE_BIT, 50).
-define(TDS_TYPE_TIME, 51).
-define(TDS_TYPE_INT2, 52).
-define(TDS_TYPE_INT4, 56).
-define(TDS_TYPE_SHORTDATE, 58).
-define(TDS_TYPE_FLT4, 59).
-define(TDS_TYPE_MONEY, 60).
-define(TDS_TYPE_DATETIME, 61).
-define(TDS_TYPE_FLT8, 62).
-define(TDS_TYPE_UINT2, 65).
-define(TDS_TYPE_UINT4, 66).
-define(TDS_TYPE_UINT8, 67).
-define(TDS_TYPE_UINTN, 68).
-define(TDS_TYPE_SENSITIVITY, 103).
-define(TDS_TYPE_BOUNDARY, 104).
-define(TDS_TYPE_DECN, 106).
-define(TDS_TYPE_NUMN, 108).
-define(TDS_TYPE_FLTN, 109).
-define(TDS_TYPE_MONEYN, 110).
-define(TDS_TYPE_DATETIMEN, 111).
-define(TDS_TYPE_SHORTMONEY, 122).
-define(TDS_TYPE_DATEN, 123).
-define(TDS_TYPE_TIMEN, 147).
-define(TDS_TYPE_XML, 163).
-define(TDS_TYPE_UNITEXT, 174).
-define(TDS_TYPE_LONGCHAR, 175).
-define(TDS_TYPE_SINT1, 176).
-define(TDS_TYPE_INT8, 191).
-define(TDS_TYPE_LONGBINARY, 225).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 User types %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(USER_TYPE_CHAR, 1).                     %% blank pad to the length in the format
-define(USER_TYPE_VARCHAR, 2).
-define(USER_TYPE_BINARY, 3).
-define(USER_TYPE_VARBINARY, 4).                %% null pad to the length in the format
-define(USER_TYPE_TINYINT, 5).
-define(USER_TYPE_SMALLINT, 6).
-define(USER_TYPE_INT, 7).
-define(USER_TYPE_FLOAT, 8).
-define(USER_TYPE_NUMERIC, 10).
-define(USER_TYPE_MONEY, 11).
-define(USER_TYPE_DATETIME, 12).
-define(USER_TYPE_INTN, 13).
-define(USER_TYPE_FLOATN, 14).
-define(USER_TYPE_DATETIMEN, 15).
-define(USER_TYPE_BIT, 16).
-define(USER_TYPE_MONEYN, 17).
-define(USER_TYPE_SYSNAME, 18).                 %% Internal ASE datatype
-define(USER_TYPE_TEXT, 19).
-define(USER_TYPE_IMAGE, 20).
-define(USER_TYPE_SMALLMONEY, 21).
-define(USER_TYPE_SMALLDATETIME, 22).
-define(USER_TYPE_REAL, 23).
-define(USER_TYPE_NCHAR, 24).
-define(USER_TYPE_NVARCHAR, 25).
-define(USER_TYPE_DECIMAL, 26).                 %% decimal and numeric datatypes
                                                %%   are identical on ASE, but we maintain 
                                                %%   the distinction on how they were 
                                                %%   declared so that clients can report 
                                                %%   column types in a way that is 
                                                %%   consistent with how they were declared
-define(USER_TYPE_DECIMALN, 27).
-define(USER_TYPE_NUMERICN, 28).
-define(USER_TYPE_UNICHAR, 34).                 %% fixed length UTF-16 encoded data
-define(USER_TYPE_UNIVARCHAR, 35).              %% variable length UTF-16 encoded data
-define(USER_TYPE_UNITEXT, 36).                 %% UTF-16 encoded data
-define(USER_TYPE_DATE, 37).
-define(USER_TYPE_TIME, 38).
-define(USER_TYPE_INTERVAL, 39).
-define(USER_TYPE_DATEN, 40).
-define(USER_TYPE_TIMEN, 41).
%-define(USER_TYPE_BIGINT, 42). shifted to one
-define(USER_TYPE_BIGINT, 43).
-define(USER_TYPE_USMALLINT, 44).
-define(USER_TYPE_UINT, 45).
-define(USER_TYPE_UBIGINT, 46).
-define(USER_TYPE_XML, 47).                     %% Treated like text and image.
-define(USER_TYPE_LONGDATE, 50).                %% The hh:mm:ss.nnnn information (should be ignored)
-define(USER_TYPE_LONGTIME, 51).                %% The mm/dd/yyyy information (should be ignored)
-define(USER_TYPE_UNSIGNED_SHORT, 52).          %% Deprecated
-define(USER_TYPE_UNSIGNED_INT, 53).            %% Deprecate
-define(USER_TYPE_UNSIGNED_LONG, 54).           %% Deprecate
-define(USER_TYPE_SERIALIZATION, 55).           %% serialized java object or instance (i.e. java object)
-define(USER_TYPE_SERIALIZED_JAVA_CLASS, 56).   %% serialized java class (i.e. byte code)
-define(USER_TYPE_STRING, 57).                  %% internally generated varchar strings 
                                                %%    (e.g. select @@version),
                                                %%    not table columns
-define(USER_TYPE_UNKNOWN, 58).                 %% a describe input will return
                                                %%   TDS_INT4 (as a simple placeholder) 
                                                %%   for all columns where it does not know the datatype. 
                                                %%   This usertype indicates that the actual type is unknown
-define(USER_TYPE_SMALLBINARY, 59).             %% 64K max length binary data (ASA)
-define(USER_TYPE_SMALLCHAR, 60).               %% 64K maximum length char data (ASA)
-define(USER_TYPE_TIMESTAMP, 80).               %% This has nothing to do with date or time, 
                                                %%   it is an ASE unique value for use with optimistic concurrency

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Environment Variables  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(ENV_DB, 1).
-define(ENV_LANG, 2).
-define(ENV_CHARSET, 3).
-define(ENV_PACKETSIZE, 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Capability Token Types %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_CAP_REQUEST, 1).                %% Requests and data types that can be sent on this dialog
-define(TDS_CAP_RESPONSE, 2).               %% Responses and data types that should not be sent on this dialog

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                                %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Capability Token Request Capabilities  %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                                %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% byte 1
-define(TDS_REQ_LANG, 1).                   %% Language requests
-define(TDS_REQ_RPC, 2).                    %% RPC requests
-define(TDS_REQ_EVT, 3).                    %% Registered procedure event notification
-define(TDS_REQ_MSTMT, 4).                  %% Support multiple commands per request
-define(TDS_REQ_BCP, 5).                    %% Bulk copy requests
-define(TDS_REQ_CURSOR, 6).                 %% Cursor command requests
-define(TDS_REQ_DYNF, 7).                   %% Dynamic SQL requests

%% byte 2
-define(TDS_REQ_MSG, 8).                    %% TDS_MSG requests
-define(TDS_REQ_PARAM, 9).                  %% RPC requests will use the TDS_DBRPC token and TDS_PARAMFMT / TDS_PARAM 
                                            %%      to send parameters.
-define(TDS_DATA_INT1, 10).                 %% Support 1 byte unsigned integers
-define(TDS_DATA_INT2, 11).                 %% Support 2 byte integers
-define(TDS_DATA_INT4, 12).                 %% Support 4 byte integers
-define(TDS_DATA_BIT, 13).                  %% Support bit data types
-define(TDS_DATA_CHAR, 14).                 %% Support fixed length character data types
-define(TDS_DATA_VCHAR, 15).                %% Support variable length character data types

%% byte 3
-define(TDS_DATA_BIN, 16).                  %% Support fixed length character data types
-define(TDS_DATA_VBIN, 17).                 %% Support variable length binary data types
-define(TDS_DATA_MNY8, 18).                 %% Support 8 byte money data types
-define(TDS_DATA_MNY4, 19).                 %% Support 4 byte money data types
-define(TDS_DATA_DATE8, 20).                %% Support 8 byte date/time data types
-define(TDS_DATA_DATE4, 21).                %% Support 4 byte date/time data types
-define(TDS_DATA_FLT4, 22).                 %% Support 4 byte floating point data types
-define(TDS_DATA_FLT8, 23).                 %% Support 8 byte floating point data types

%% byte 4
-define(TDS_DATA_NUM, 24).                  %% Support numeric data types
-define(TDS_DATA_TEXT, 25).                 %% Support text data types
-define(TDS_DATA_IMAGE, 26).                %% Support image data types
-define(TDS_DATA_DEC, 27).                  %% Support decimal data types
-define(TDS_DATA_LCHAR, 28).                %% Support long variable length character data types
-define(TDS_DATA_LBIN, 29).                 %% Support long variable length binary data types.
-define(TDS_DATA_INTN, 30).                 %% Support NULL integers
-define(TDS_DATA_DATETIMEN, 31).            %% Support NULL date/time

%% byte 5
-define(TDS_DATA_MONEYN, 32).               %% Support NULL money
-define(TDS_CSR_PREV, 33).                  %% Obsolete, will not be used
-define(TDS_CSR_FIRST, 34).                 %% Obsolete, will not be used
-define(TDS_CSR_LAST, 35).                  %% Obsolete, will not be used
-define(TDS_CSR_ABS, 36).                   %% Obsolete, will not be used
-define(TDS_CSR_REL, 37).                   %% Obsolete, will not be used
-define(TDS_CSR_MULTI, 38).                 %% This is possibly obsolete
-define(TDS_CON_OOB, 39).                   %% Support expedited attentions

%% byte 6
-define(TDS_CON_INBAND, 40).                %% Support non-expedited attentions
-define(TDS_CON_LOGICAL, 41).               %% Support logical logout
-define(TDS_PROTO_TEXT, 42).                %% Support tokenized text and image
-define(TDS_PROTO_BULK, 43).                %% Support tokenized bulk copy
-define(TDS_REQ_URGEVT, 44).                %% Use new event notification protocol
-define(TDS_DATA_SENSITIVITY, 45).          %% Support sensitivity security data types
-define(TDS_DATA_BOUNDARY, 46).             %% Support boundary security data types
-define(TDS_PROTO_DYNAMIC, 47).             %% Use DESCIN/DESCOUT dynamic protocol

%% byte 7
-define(TDS_PROTO_DYNPROC, 48).             %% Pre-pend “create proc” to dynamic prepare statements
-define(TDS_DATA_FLTN, 49).                 %% Support NULL floats
-define(TDS_DATA_BITN, 50).                 %% Support NULL bits
-define(TDS_DATA_INT8, 51).                 %% Support 8 byte integers
-define(TDS_DATA_VOID, 52).                 %% ?
-define(TDS_DOL_BULK, 53).                  %% ?
-define(TDS_OBJECT_JAVA1, 54).              %% Support Serialized Java Objects
-define(TDS_OBJECT_CHAR, 55).               %% Support Streaming character data

%% byte 8
%-define(RESERVED, 56).                     %% Reserved for future use
-define(TDS_OBJECT_BINARY, 57).             %% Streaming Binary data
-define(TDS_DATA_COLUMNSTATUS, 58).         %% Indicates that a one-byte status field can follow any length or data (etc.) 
                                            %%      for every column within a row using TDS_ROW or TDS_PARAMS.
                                            %%      Note that when this capability is on, the ROWFMT* and PARAMFMT* tokens 
                                            %%      indicate in their status byte fields whether a particular column will 
                                            %%      contain the columnstatus byte.
-define(TDS_WIDETABLE, 59).                 %% The client may send requests using the CURDECLARE2, DYNAMIC2, PARAMFMT2 tokens
%-define(RESERVED, 60).                     %% Reserved
-define(TDS_DATA_UINT2, 61).                %% Support for unsigned 2-byte integers
-define(TDS_DATA_UINT4, 62).                %% Support for unsigned 4-byte integers
-define(TDS_DATA_UINT8, 63).                %% Support for unsigned 8-byte integers

%% byte 9
-define(TDS_DATA_UINTN, 64).                %% Support for NULL unsigned integers
-define(TDS_CUR_IMPLICIT, 65).              %% Support for TDS_CUR_DOPT_IMPLICIT cursor declare option
-define(TDS_DATA_NLBIN, 66).                %% Support for LONGBINARY data containing 
                                            %%      UTF-16 encoded data (USER_TYPE_UNICHAR and USER_TYPE_UNIVARCHAR)
-define(TDS_IMAGE_NCHAR, 67).               %% Support for IMAGE data containing 
                                            %%      UTF-16 encoded data (USER_TYPE_UNITEXT)
-define(TDS_BLOB_NCHAR_16, 68).             %% Support for BLOB subtype 0x05 (unichar) with serialization type 0
-define(TDS_BLOB_NCHAR_8, 69).              %% Support for BLOB subtype 0x05 (unichar) with serialization type 1
-define(TDS_BLOB_NCHAR_SCSU, 70).           %% Support for BLOB subtype 0x05 (unichar) with serialization type2
-define(TDS_DATA_DATE, 71).                 %% Support for Date

%% byte 10
-define(TDS_DATA_TIME, 72).                 %% Support for Time.
-define(TDS_DATA_INTERVAL, 73).             %% Support for Interval
-define(TDS_CSR_SCROLL, 74).                %% Support for Scrollable Cursor. This bit must be 
                                            %%      on for the following four capability bits to have meaning
-define(TDS_CSR_SENSITIVE, 75).             %% Support for Scrollable Sensitive Cursor
-define(TDS_CSR_INSENSITIVE, 76).           %% Support for Scrollable Insensitive Cursor
-define(TDS_CSR_SEMISENSITIVE, 77).         %% Support for Scrollable Semisensitive Cursor
-define(TDS_CSR_KEYSETDRIVEN, 78).          %% Support for Scrollable Keysetdriven Cursor
-define(TDS_REQ_SRVPKTSIZE, 79).            %% Support for server specified packet size

%% byte 11
-define(TDS_DATA_UNITEXT, 80).              %% Support for Unicode UTF-16 Text
-define(TDS_CAP_CLUSTERFAILOVER, 81).       %% Support Cluster Failover Extensions
-define(TDS_DATA_SINT1, 82).                %% Support for 1 byte signed integer
-define(TDS_REQ_LARGEIDENT, 83).            %% Support for large identifiers
-define(TDS_REQ_BLOB_NCHAR_16, 84).         %% Support for BLOB subtype 0x05 (unichar) with serialization type 0
                                            %%      Replaces TDS_BLOB_NCHAR_16.
                                            %%      Added to deal with ASE coding issue in old servers
-define(TDS_DATA_XML, 85).                  %% Support for XML datatype
-define(TDS_REQ_CURINFO3, 86).              %% Support for TDS_CURINFO3 token
-define(TDS_REQ_DBRPC2, 87).                %% Support for TDS_DBRPC2 token

%% byte 12
%-define(UNKNOWN, 88).                      %% Unknown
-define(TDS_REQ_MIGRATE, 89).               %% Client can be migrated to another server

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                                    %%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Capability Token Response Capabilities     %%%%%%%%%%%%%%%%%%%%%%%
%%%%                                                    %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% byte 1
-define(TDS_RES_NOMSG, 1).                  %% No support for TDS_MSG results
-define(TDS_RES_NOEED, 2).                  %% No support for TDS_EED token
-define(TDS_RES_NOPARAM, 3).                %% No support for TDS_PARAM / TDS_PARAMFMT for return parameter. 
                                            %%      Use TDS_RETURNVALUE to return parameters to this client.
-define(TDS_DATA_NOINT1, 4).                %% No support for 1 byte unsigned integers
-define(TDS_DATA_NOINT2, 5).                %% No support for 2 byte integers
-define(TDS_DATA_NOINT4, 6).                %% No support for 4 byte integers
-define(TDS_DATA_NOBIT, 7).                 %% No support for bit data types

%% byte 2
-define(TDS_DATA_NOCHAR, 8).                %% No support for fixed length character data types
-define(TDS_DATA_NOVCHAR, 9).               %% No support for variable length character data types
-define(TDS_DATA_NOBIN, 10).                %% No support for fixed length binary data types
-define(TDS_DATA_NOVBIN, 11).               %% No support for variable length binary data types
-define(TDS_DATA_NOMNY8, 12).               %% No support for 8 byte money data types
-define(TDS_DATA_NOMNY4, 13).               %% No support for 4 byte money data types
-define(TDS_DATA_NODATE8, 14).              %% No support for 8 byte date/time data types
-define(TDS_DATA_NODATE4, 15).              %% No support for 4 byte date/time data types

%% byte 3
-define(TDS_DATA_NOFLT4, 16).               %% No support for 4 byte float data types
-define(TDS_DATA_NOFLT8, 17).               %% No support for 8 byte float data types
-define(TDS_DATA_NONUM, 18).                %% No support for numeric data types
-define(TDS_DATA_NOTEXT, 19).               %% No support for text data types
-define(TDS_DATA_NOIMAGE, 20).              %% No support for image data types
-define(TDS_DATA_NODEC, 21).                %% No support for decimal data types
-define(TDS_DATA_NOLCHAR, 22).              %% No support for long variable length character data types
-define(TDS_DATA_NOLBIN, 23).               %% No support for long variable length binary data types

%% byte 4
-define(TDS_DATA_NOINTN, 24).               %% No support for nullable integers
-define(TDS_DATA_NODATETIMEN, 25).          %% No support for nullable date/time data types
-define(TDS_DATA_NOMONEYN, 26).             %% No support for nullable money data types
-define(TDS_CON_NOOOB, 27).                 %% No support for expedited attentions
-define(TDS_CON_NOINBAND, 28).              %% No support for non-expedited attentions
-define(TDS_PROTO_NOTEXT, 29).              %% No support for tokenized text and image
-define(TDS_PROTO_NOBULK, 30).              %% No support for tokenized bulk copy
-define(TDS_DATA_NOSENSITIVITY, 31).        %% No support for the security sensitivity data type

%% byte 5
-define(TDS_DATA_NOBOUNDARY, 32).           %% No support for the security boundary data type
-define(TDS_RES_NOTDSDEBUG, 33).            %% No support for TDS_DEBUG token. Use image data instead
-define(TDS_RES_NOSTRIPBLANKS, 34).         %% Do not strip blank from fixed length character data
-define(TDS_DATA_NOINT8, 35).               %% No support for 8 byte integers
-define(TDS_OBJECT_NOJAVA1, 36).            %% No Support Serialized Java Objects
-define(TDS_OBJECT_NOCHAR, 37).             %% No Support Streaming character data
-define(TDS_DATA_NOCOLUMNSTATUS, 38).       %% No Support for the columnstatus byte
-define(TDS_OBJECT_NOBINARY, 39).           %% No Streaming Binary data

%% byte 5
%-define(RESERVED, 40).                      %% Reserved for future use
-define(TDS_DATA_NOUINT2, 41).              %% No Support for unsigned 2-byte integers
-define(TDS_DATA_NOUINT4, 42).              %% No Support for unsigned 4-byte integers
-define(TDS_DATA_NOUINT8, 43).              %% No Support for unsigned 8-byte integers
-define(TDS_DATA_NOUINTN, 44).              %% No Support for NULL unsigned integers
-define(TDS_NO_WIDETABLES, 45).             %% Client cannot process the TDS_ORDERBY2, TDS_PARAMFMT2, and TDS_ROWFMT2 
                                            %%      tokens required to support tables with a LARGE number of columns
                                            %%      The server should not send them.
-define(TDS_DATA_NONLBIN, 46).              %% No Support for LONGBINARY data containing 
                                            %%      UTF-16 encoded data (USER_TYPE_UNICHAR and USER_TYPE_UNIVARCHAR)
-define(TDS_IMAGE_NONCHAR, 47).             %% No Support for IMAGE data containing 
                                            %%      UTF-16 encoded data (USER_TYPE_UNITEXT)

%% byte 6
-define(TDS_BLOB_NONCHAR_16, 48).           %% No Support for BLOB subtype 0x05/0
-define(TDS_BLOB_NONCHAR_8, 49).            %% No Support for BLOB subtype 0x05/1
-define(TDS_BLOB_NONCHAR_SCSU, 50).         %% No Support for BLOB subtype 0x05/2
-define(TDS_DATA_NODATE, 51).               %% No Support for Date
-define(TDS_DATA_NOTIME, 52).               %% No Support for Time
-define(TDS_DATA_NOINTERVAL, 53).           %% No Support for Interval
-define(TDS_DATA_NOUNITEXT, 54).            %% No Support for Unicode UTF-16 Text
-define(TDS_DATA_NOSINT1, 55).              %% No Support for 1 byte signed integers

%% byte 7
-define(TDS_NO_LARGEIDENT, 56).             %% No Support for Large Identifiers
-define(TDS_NO_BLOB_NCHAR_16, 57).          %% No Support for BLOB subtype 0x05/0. Replaces TDS_BLOB_NONCHAR_16
                                            %%      Added to work around ASE coding issue
-define(TDS_NO_SRVPKTSIZE, 58).             %% No Support for Server specified packet size larger than the client requested
                                            %%      If the TDS_REQ_PKTSIZE capability is set, the server may still specify 
                                            %%      the packet size as long as it is not larger than suggested by the client
-define(TDS_DATA_NOXML, 59).                %% No Support for XML datatype
-define(TDS_NONINT_RETURN_VALUE, 60).       %% Client support for non-integer return values from TDS_RETURNVALUE
-define(TDS_RES_NOXNLDATA, 61).             %% Client does not need additional metadata in TDS_ROWFMT2
                                            %%      Also, server should use TDS_ROWFMT instead of TDS_ROWFMT2
-define(TDS_RES_SUPPRESS_FMT, 62).          %% Server will suppress TDS_ROWFMT (or TDS_ROWFMT2) tokens 
                                            %%      if the appropriate status bit is set in TDS_DYNAMIC
-define(TDS_RES_SUPPRESS_DONEINPROC, 63).   %% Server can suppress TDS_DONEINPROC tokens
                                            %% This allows TDS_ROW to be followed by TDS_ROWFMT(2) instead of 
                                            %% TDS_DONEINPROC / TDS_ROWFMT(2)
                                            %% Final done count will be provided by TDS_DONEPROC

%% byte 8
-define(TDS_RES_FORCE_ROWFMT2, 64).         %% Force usage of TDS_ROWFMT2 even if TDS_ROWFMT could be used

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Done Token Status Flags    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_DONE_FINAL, 0).
-define(TDS_DONE_MORE, 1).
-define(TDS_DONE_ERROR, 2).
-define(TDS_DONE_INXACT, 4).
-define(TDS_DONE_PROC, 8).
-define(TDS_DONE_COUNT, 16).
-define(TDS_DONE_ATTN, 32).
-define(TDS_DONE_EVENT, 64).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 LoginAck Token Statuses    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_LOG_SUCCEED, 5).
-define(TDS_LOG_FAIL, 6).
-define(TDS_LOG_NEGOTIATE, 7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 RowFmt Token Status Flags  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_ROW_HIDDEN, 1).
-define(TDS_ROW_KEY, 2).
-define(TDS_ROW_VERSION, 4).
-define(TDS_ROW_COLUMNSTATUS, 8).
-define(TDS_ROW_UPDATABLE, 16).
-define(TDS_ROW_NULLALLOWED, 32).
-define(TDS_ROW_IDENTITY, 64).
-define(TDS_ROW_PADCHAR, 128).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 ParamFmt Token Status Flags    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_PARAM_RETURN, 1).
-define(TDS_PARAM_COLUMNSTATUS, 8).
-define(TDS_PARAM_NULLALLOWED, 32).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Dynamic Token Operation Types  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_DYN_PREPARE, 1).    %% This is a request to prepare stmt
-define(TDS_DYN_EXEC, 2).       %% This is a request to execute a prepared statement
-define(TDS_DYN_DEALLOC, 4).    %% Request to deallocate a prepared statement
-define(TDS_DYN_EXEC_IMMED, 8). %% This a request to prepare and execute stmt immediately
-define(TDS_DYN_PROCNAME, 16).  %% It this used? If so what for?
-define(TDS_DYN_ACK, 32).       %% Acknowledge a dynamic command
-define(TDS_DYN_DESCIN, 64).    %% Send input format description
-define(TDS_DYN_DESCOUT, 128).  %% Send output format description

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Dynamic Token Status Flags     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_DYNAMIC_UNUSED, 0).         %% No status associated with this dynamic command
-define(TDS_DYNAMIC_HASARGS, 1).        %% Parameter data stream follows the dynamic command
-define(TDS_DYNAMIC_SUPPRESS_FMT, 2).   %% If this statement, as identified by id, 
                                        %%   has previously sent TDS_ROWFMT information 
                                        %%   and this information has not changed, 
                                        %%   do not resend TDS_ROWFMT
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 DBRPC Token Options    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_RPC_UNUSED, 0).
-define(TDS_RPC_RECOMPILE, 1).
-define(TDS_RPC_PARAMS, 2).
