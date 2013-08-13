
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Packet types   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(LOGIN_PKT, 2).
-define(RESPONSE_PKT, 4).
-define(QUERRY_PKT, 15).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Tokens %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TOKEN_CURDECLARE3, 16).
%-define(TOKEN_COLINFO2, 32). %% TODO ???
-define(TOKEN_PARAMFMT2, 32). %% TODO ???
-define(TOKEN_LANGUAGE, 33).
-define(TOKEN_ORDERBY2, 34).
-define(TOKEN_CURDECLARE2, 35).
-define(TOKEN_ROWFMT2,  97).
-define(TOKEN_DYNAMIC2,  98).
-define(TOKEN_MSG,  101).
-define(TOKEN_LOGOUT, 113).
-define(TOKEN_OFFSET, 120).
-define(TOKEN_RETURNSTATUS, 121).
-define(TOKEN_CURCLOSE, 128).
-define(TOKEN_CURDELETE, 129).
-define(TOKEN_CURFETCH, 130).
-define(TOKEN_CURINFO, 131).
-define(TOKEN_CUROPEN, 132).
-define(TOKEN_CURUPDATE, 133).
-define(TOKEN_CURDECLARE, 134).
-define(TOKEN_CURINFO2, 135).
-define(TOKEN_CURINFO3, 136).
-define(TOKEN_EVENTNOTICE, 162).
-define(TOKEN_TABNAME, 164).
-define(TOKEN_COLINFO, 165).
-define(TOKEN_OPTIONCMD, 166).
-define(TOKEN_ALTNAME, 167).
-define(TOKEN_ALTFMT, 168).         %% Computed result set token
-define(TOKEN_ORDERBY, 169).
-define(TOKEN_ERROR, 170).
-define(TOKEN_INFO, 171).
-define(TOKEN_RETURNVALUE, 172).
-define(TOKEN_LOGINACK, 173).
-define(TOKEN_CONTROL, 174).
-define(TOKEN_KEY, 202).
-define(TOKEN_ROW, 209).
-define(TOKEN_ALTROW, 211).
-define(TOKEN_PARAMS, 215).
-define(TOKEN_RPC,  224).
-define(TOKEN_CAPABILITY,  226).    %% Capabilities token
-define(TOKEN_ENVCHANGE,  227).
-define(TOKEN_EED,  229).           %% Extended Error Data token
-define(TOKEN_DBRPC,  230).
-define(TOKEN_DYNAMIC,  231).
-define(TOKEN_DBRPC2,  232).
-define(TOKEN_PARAMFMT,  236).
-define(TOKEN_ROWFMT,  238).
-define(TOKEN_DONE,  253).
-define(TOKEN_DONEPROC,  254).
-define(TOKEN_DONEINPROC,  255).

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

-define(IS_FIXED_LENGTH_TYPE(Type),
    Type =:= ?TDS_TYPE_INT1; 
    Type =:= ?TDS_TYPE_INT2; 
    Type =:= ?TDS_TYPE_INT4; 
    Type =:= ?TDS_TYPE_INT8;
    Type =:= ?TDS_TYPE_FLT8;
    Type =:= ?TDS_TYPE_DATETIME;
    Type =:= ?TDS_TYPE_DATE;
    Type =:= ?TDS_TYPE_TIME;
    Type =:= ?TDS_TYPE_MONEY
).

-define(IS_VAR_LENGTH_TYPE(Type),
    Type =:= ?TDS_TYPE_CHAR;
    Type =:= ?TDS_TYPE_VARCHAR;
    Type =:= ?TDS_TYPE_VARBINARY;
    Type =:= ?TDS_TYPE_INTN;
    Type =:= ?TDS_TYPE_FLTN;
    Type =:= ?TDS_TYPE_DATETIMEN;
    Type =:= ?TDS_TYPE_DATEN;
    Type =:= ?TDS_TYPE_TIMEN;
    Type =:= ?TDS_TYPE_MONEYN
).

-define(IS_NUMERIC_TYPE(Type),
    Type =:= ?TDS_TYPE_NUMN;
    Type =:= ?TDS_TYPE_DECN
).

-define(IS_TEXT_OR_IMAGE_TYPE(Type), 
    Type =:= ?TDS_TYPE_TEXT;
    Type =:= ?TDS_TYPE_IMAGE
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 User types %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(USER_TYPE_CHAR, 1).
-define(USER_TYPE_VARCHAR, 2).
-define(USER_TYPE_BINARY, 3).
-define(USER_TYPE_VARBINARY, 4).
-define(USER_TYPE_TINYINT, 5).
-define(USER_TYPE_SMALLINT, 6).
-define(USER_TYPE_INT, 7).
-define(USER_TYPE_FLOAT, 8).
-define(USER_TYPE_NUMERIC, 10).
-define(USER_TYPE_MONEY, 11).
-define(USER_TYPE_DATETIME, 12).
-define(USER_TYPE_INTN, 13).
-define(USER_TYPE_FLOATN, 14).
-define(USER_TYPE_BIT, 16).
-define(USER_TYPE_MONEYN, 17).
-define(USER_TYPE_SYSNAME, 18).
-define(USER_TYPE_TEXT, 19).
-define(USER_TYPE_IMAGE, 20).
-define(USER_TYPE_SMALLMONEY, 21).
-define(USER_TYPE_SMALLDATETIME, 22).
-define(USER_TYPE_REAL, 23).
-define(USER_TYPE_NCHAR, 24).
-define(USER_TYPE_NVARCHAR, 25).
-define(USER_TYPE_DECIMAL, 26).
-define(USER_TYPE_DECIMALN, 27).
-define(USER_TYPE_NUMERICN, 28).
-define(USER_TYPE_UNICHAR, 34).
-define(USER_TYPE_UNIVARCHAR, 35).
-define(USER_TYPE_UNITEXT, 36).
-define(USER_TYPE_DATE1, 37).
-define(USER_TYPE_TIME1, 38).
-define(USER_TYPE_INTERVAL, 39).
-define(USER_TYPE_DATEN, 40).
-define(USER_TYPE_TIMEN, 41).
-define(USER_TYPE_BIGINT, 43).
-define(USER_TYPE_USMALLINT, 44).
-define(USER_TYPE_UINT, 45).
-define(USER_TYPE_UBIGINT, 46).
-define(USER_TYPE_XML, 47).
-define(USER_TYPE_DATE2, 50).
-define(USER_TYPE_TIME2, 51).
-define(USER_TYPE_UNSIGNED_SHORT, 52).
-define(USER_TYPE_UNSIGNED_INT, 53).
-define(USER_TYPE_UNSIGNED_LONG, 54).
-define(USER_TYPE_SERIALIZATION, 55).
-define(USER_TYPE_SERIALIZED_JAVA_CLASS, 56).
-define(USER_TYPE_STRING, 57).
-define(USER_TYPE_UNKNOWN, 58).
-define(USER_TYPE_SMALLBINARY, 59).
-define(USER_TYPE_SMALLCHAR, 60).
-define(USER_TYPE_TIMESTAMP, 80).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Login Statuses %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(LOG_SUCCEED, 5).
-define(LOG_FAIL, 6).
-define(LOG_NEGOTIATE, 7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Row Statuses   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(ROW_HIDDEN, 1).
-define(ROW_KEY, 2).
-define(ROW_VERSION, 4).
-define(ROW_COLUMNSTATUS, 8).
-define(ROW_UPDATABLE, 16).
-define(ROW_NULLALLOWED, 32).
-define(ROW_IDENTITY, 64).
-define(ROW_PADCHAR, 128).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Dynamic Operation Types    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DYN_PREPARE, 1).    %% This is a request to prepare stmt
-define(DYN_EXEC, 2).       %% This is a request to execute a prepared statement
-define(DYN_DEALLOC, 4).    %% Request to deallocate a prepared statement
-define(DYN_EXEC_IMMED, 8). %% This a request to prepare and execute stmt immediately
%-define(DYN_PROCNAME, 16).
-define(DYN_ACK, 32).       %% Acknowledge a dynamic command
-define(DYN_DESCIN, 64).    %% Send input format description
-define(DYN_DESCOUT, 128).  %% Send output format description

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Dynamic Status Values  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DYNAMIC_UNUSED, 0).         %% No status associated with this dynamic command
-define(DYNAMIC_HASARGS, 1).        %% Parameter data stream follows the dynamic command
-define(DYNAMIC_SUPPRESS_FMT, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Done Statuses  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DONE_FINAL, 0).
-define(DONE_MORE, 1).
-define(DONE_ERROR, 2).
-define(DONE_INXACT, 4).
-define(DONE_PROC, 8).
-define(DONE_COUNT, 16).
-define(DONE_ATTN, 32).
-define(DONE_EVENT, 64).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Environment Variables  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(ENV_DB, 1).
-define(ENV_LANG, 2).
-define(ENV_CHARSET, 3).
-define(ENV_PACKETSIZE, 4).
