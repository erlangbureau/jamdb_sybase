-module(jamdb_sybase).
-behaviour(gen_server).

%% API
-export([start_link/1, start/1]).
-export([stop/1]).
-export([sql_query/2, sql_query/3]).
-export([prepare/3, unprepare/2]).
-export([execute/2, execute/3, execute/4]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-include("jamdb_sybase_defaults.hrl").
-include("jamdb_sybase.hrl").

%% API
-spec start_link(jamdb_sybase_conn:options()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec start(jamdb_sybase_conn:options()) -> {ok, pid()} | {error, term()}.
start(Opts) when is_list(Opts) ->
    gen_server:start(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    call_infinity(Pid, stop).

sql_query(Pid, Query) ->
    sql_query(Pid, Query, ?DEF_TIMEOUT).

sql_query(Pid, Query, Timeout) ->
    call_infinity(Pid, {sql_query, Query, Timeout}).

prepare(Pid, Stmt, Query) ->
    call_infinity(Pid, {prepare, Stmt, Query}).

unprepare(Pid, Stmt) ->
    call_infinity(Pid, {unprepare, Stmt}).

execute(Pid, Stmt) ->
    execute(Pid, Stmt, []).

execute(Pid, Stmt, Args) ->
    execute(Pid, Stmt, Args, ?DEF_TIMEOUT).

execute(Pid, Stmt, Args, Timeout) ->
    call_infinity(Pid, {execute, Stmt, Args, Timeout}).

%% gen_server callbacks
init(Opts) ->
    {ok, State} = jamdb_sybase_conn:connect(Opts),
    {ok, State}.

%% Error types: socket, remote, local
handle_call({execute, Stmt, Args, Timeout} = Operation, _From, State) ->
    try jamdb_sybase_conn:execute(State, Stmt, Args, Timeout) of
        {ok, Result, State2} -> 
            {reply, {ok, Result}, State2};
        {error, Type, Message, State2} ->
            {reply, {error, format_error(Type, Message)}, State2}
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            Err = {Operation, State, Class, Reason, Stacktrace},
            ErrDesc = format_error(exception, Err),
            {ok, State2} = jamdb_sybase_conn:reconnect(State),
            {reply, {error, ErrDesc}, State2}
    end;
handle_call({sql_query, Query, Timeout} = Operation, _From, State) ->
    try jamdb_sybase_conn:sql_query(State, Query, Timeout) of
        {ok, Result, State2} -> 
            {reply, {ok, Result}, State2};
        {error, Type, Message, State2} ->
            {reply, {error, format_error(Type, Message)}, State2}
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            Err = {Operation, State, Class, Reason, Stacktrace},
            ErrDesc = format_error(exception, Err),
            {ok, State2} = jamdb_sybase_conn:reconnect(State),
            {reply, {error, ErrDesc}, State2}
    end;
handle_call({prepare, Stmt, Query} = Operation, _From, State) ->
    try jamdb_sybase_conn:prepare(State, Stmt, Query) of
        {ok, State2} -> 
            {reply, ok, State2};
        {error, Type, Message, State2} ->
            {reply, {error, format_error(Type, Message)}, State2}
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            Err = {Operation, State, Class, Reason, Stacktrace},
            ErrDesc = format_error(exception, Err),
            {ok, State2} = jamdb_sybase_conn:reconnect(State),
            {reply, {error, ErrDesc}, State2}
    end;
handle_call({unprepare, Stmt} = Operation, _From, State) ->
    try jamdb_sybase_conn:unprepare(State, Stmt) of
        {ok, State2} -> 
            {reply, ok, State2};
        {error, Type, Message, State2} ->
            {reply, {error, format_error(Type, Message)}, State2}
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            Err = {Operation, State, Class, Reason, Stacktrace},
            ErrDesc = format_error(exception, Err),
            {ok, State2} = jamdb_sybase_conn:reconnect(State),
            {reply, {error, ErrDesc}, State2}
    end;
handle_call(stop, _From, State) ->
    {ok, _InitOpts} = jamdb_sybase_conn:disconnect(State),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
call_infinity(Pid, Msg) ->
    gen_server:call(Pid, Msg, infinity).

format_error(remote, Message) ->
    {remote, [
        {msg_number,        Message#message.msg_number},
        {msg_state,         Message#message.msg_state},
        {class,             Message#message.class},
        {sql_state,         Message#message.sql_state},
        {status,            Message#message.status},
        {transaction_state, Message#message.transaction_state},
        {msg_body,          Message#message.msg_body},
        {server_name,       Message#message.server_name},
        {procedure_name,    Message#message.procedure_name},
        {line_number,       Message#message.line_number}
    ]};
format_error(exception, {Operation, State, Class, Reason, Stacktrace}) ->
    {local, [
        {operation,         Operation},
        {state,             State},
        {exception_class,   Class},
        {exception_reason,  Reason},
        {stacktrace,        Stacktrace}
    ]};
format_error(Type, Msg) ->
    {Type, Msg}.
