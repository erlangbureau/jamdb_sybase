-module(jamdb_sybase).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([sql_query/2, sql_query/3]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-include("jamdb_sybase_defaults.hrl").

%% API
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

stop(Pid) ->
    call_infinity(Pid, stop).

sql_query(Pid, Query) ->
    sql_query(Pid, Query, ?DEF_TIMEOUT).

sql_query(Pid, Query, Timeout) ->
    call_infinity(Pid, {sql_query, Query, Timeout}).

%% gen_server callbacks
init(Opts) ->
    {ok, State} = jamdb_sybase_conn:connect(Opts),
    {ok, State}.

%% Error types: socket, remote, local
handle_call({sql_query, Query, Timeout}, _From, State) ->
    case jamdb_sybase_conn:sql_query(State, Query, Timeout) of
        {ok, Result, State2} -> 
            {reply, {ok, Result}, State2};
        {error, Type, Reason, State2} ->
            {reply, {error, Type, Reason}, State2}
    end;
%handle_call({prepare, Query}, _From, State) ->
%handle_call({execute, Query}, _From, State) ->
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
