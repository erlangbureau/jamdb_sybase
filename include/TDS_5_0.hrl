
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Packet types   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_PKT_LOGIN, 2).
-define(TDS_PKT_RESPONSE, 4).
-define(TDS_PKT_QUERY, 15).

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
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 Login Statuses %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_LOG_SUCCEED, 5).
-define(TDS_LOG_FAIL, 6).
-define(TDS_LOG_NEGOTIATE, 7).

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
%%%% TDS 5.0 Dynamic Token Status Values    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%% TDS 5.0 Environment Variables  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(ENV_DB, 1).
-define(ENV_LANG, 2).
-define(ENV_CHARSET, 3).
-define(ENV_PACKETSIZE, 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TDS 5.0 DBRPC Token Options    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TDS_RPC_UNUSED, 0).
-define(TDS_RPC_RECOMPILE, 1).
-define(TDS_RPC_PARAMS, 2).
