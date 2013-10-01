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
{result, ResultSets, State} = jamdb_sybase:sql_query("select 1, 'test', null", State).
[{result_set, Columns,Rows}] = ResultSets.
[<<>>,<<>>,<<>>] = Columns.
[[1,<<"test">>,null]] = Rows.
```

Update:
```
{ok, UpdatedRows, State} = jamdb_sybase:sql_query("update ....", State).
```
