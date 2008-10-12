%%% File    : ifastcgi_master.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : 
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>

-module(ifastcgi_master).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3, terminate/2]).
-export([start_link/0, start_link/1]).

-record(master, {servers}).
-record(server, {name, pid, monitor}).

%% API

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% Callbacks

init(_) ->
    Servers = dict:new(),
    {ok, #master{servers=Servers} }.

handle_call({add_server, StarterPid, Name, Port, CA}, _From, S0) ->
    {Result, S1} = add_server(S0, StarterPid, Name, Port, CA),
    {reply, Result, S1};
handle_call({del_server, Name}, _From, S0) ->
    {Result, S1} = del_server(S0, Name),
    {reply, Result, S1};
handle_call(which_servers, _From, S) ->
    #master{servers=Dict} = S,
    Result = which_servers(Dict),
    {reply, Result, S};
handle_call({info, ServerName}, _From, S) ->
    Result = info_server(ServerName, S),
    {reply, Result, S};
handle_call(_, _From, S) ->
    {reply, ok, S}.

handle_info(_Msg, S) ->
    {noreply, S}.

handle_cast(_, S) ->
    {noreply, S}.

code_change(_, _, S) ->
    {ok, S}.

terminate(_, _) ->
    ok.


%% Internal functions

add_server(#master{servers=Dict0}=Master0, StarterPid, 
	   Name, Port, {Mod, Args}) ->
    case supervisor:start_child(ifastcgi_server_sup, 
					[StarterPid, Port, Mod, Args]) of
	{ok, Pid} ->
	    Monitor = erlang:monitor(process, Pid),
	    Server = #server{name=Name, pid=Pid, monitor=Monitor},
	    Dict1 = dict:store(Name, Server, Dict0),
	    Master1 = Master0#master{servers=Dict1},
	    Result = {ok, Pid},
	    {Result, Master1};
	{error, _Reason}=E ->
	    {E, Master0}
    end.

del_server(#master{servers=Dict0}=Master0, Name) ->
    case dict:find(Name, Dict0) of
	{ok, Server} ->
	    ifastcgi_server:stop(Server#server.pid),
	    Dict1 = dict:erase(Name, Dict0),
	    Master1 = Master0#master{servers=Dict1},
	    {ok, Master1};
	error ->
	    {{error, no_server}, Master0}
    end.
	    
which_servers(Dict) ->
    [Server || {_Name, Server} <- dict:to_list(Dict)].

info_server(ServerName, S) ->
    case dict:find(ServerName, S#master.servers) of
	{ok, Server} ->
	    {ok, info(Server)};
	error ->
	    error
    end.

info(Server) ->
    gen_server:call(Server#server.pid, info).
    
