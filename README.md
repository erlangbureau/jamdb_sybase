jamdb_sybase
===============

jamdb_sybase is a small and fast driver for Sybase ASE writen in Erlang. It is also part of the JamDB  project.

<h3>Examples:</h3>

Connect:
```
{ok, State} = jamdb_sybase:connect(Host, Port, Login, Password, Database).
```

Select:
```
{ok, State} = jamdb_sybase:sql_query("select 1, 'test', null", State).
[{result_set, Columns, Rows}] = jamdb_sybase:get_resultsets(State),
Where:
[<<>>,<<>>,<<>>] = Columns.
[[1,<<"test">>,null]] = Rows.
```

Update:
```
{ok, State} = jamdb_sybase:sql_query("update ....", State).
1 = get_rowscount(State).
```

Get out parameters:
```
{ok, State} = jamdb_sybase:sql_query("exec proc @par1 out, @par2 out", State).
[<<"Text">>, 1] = get_returnvalues(State).

```
