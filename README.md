JamDB Sybase 
============
[![Build Status](https://travis-ci.org/erlangbureau/jamdb_sybase.svg?branch=master)](https://travis-ci.org/erlangbureau/jamdb_sybase)

JamDB Sybase is a small and fast Erlang driver for SAP Sybase Adaptive Server Enterprise (ASE).

Goals
=====

* No third-party dependencies.
* No parameterized module.
* No process dictionary.
* No ports.
* No NIF's.
* All code written exclusively in Erlang.

Getting Started
===============

```erl

%% Set connection options
1> Opts = [
    {host, "jamdb-sybase-dev.erlangbureau.dp.ua"},
    {port, 5000},
    {user, "jamdbtest"},
    {password, "jamdbtest"},
    {database, "jamdbtest"}
].

%% Connect
2> {ok, Pid} = jamdb_sybase:start_link(Opts).
{ok,<0.36.0>}

%% Simple select
3> {ok, Result} = jamdb_sybase:sql_query(Pid, "select 1 as one, 2 as two, 3 as three").
{ok,[{result_set,[
        <<"one">>,<<"two">>,<<"three">>],
        [],
        [[1,2,3]]}]}

```

Character Encodings
========
The default encoding in Erlang is utf8. So jamdb_sybase sending all strings to the server in utf8. They should by automatically converted to the corresponding encoding by the server. If the database server cannot do so, it generates an error message indicating that character conversion cannot be properly completed.

TDS Protocol References
=======================
* [Official TDS 5.0 Specification, version 3.8, January 2006](http://ondoc.logand.com/d/2219/pdf)
* [FreeTDS Documentation (C)](http://www.freetds.org)
* [jTDS Documentation (Java)](http://jtds.sourceforge.net/doc.html)

Alternatives
============
* [Erlang ODBC](http://www.erlang.org/doc/man/odbc.html)
* [erlang-db-driver](https://github.com/denglf/erlang-db-driver)
* [erldb-driver](https://github.com/RYTong/erldb-driver)
* [ErlSybase](https://github.com/VanyaDNDZ/ErlSybase)

Project Chat Room
=================
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/erlangbureau/jamdb_sybase?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

