%%% File    : ifastcgi_server.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : Manages a iserver_connection worker
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>

-module(ifastcgi_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, 
	 code_change/3, terminate/2]).
-export([start_link/5]).
-export([setup/2, kick/1, stop/1]).

-record(server, {master, starter, listener, acceptor, port, count=0, callback, cargs}).

%% API

start_link(Master, StarterPid, Port, CallbackMod, CallbackArg) ->
    InitState = #server{master=Master, port=Port,
			starter = StarterPid,
			callback=CallbackMod, cargs=CallbackArg},
    Parent = self(),
    proc_lib:start_link(?MODULE, setup, [Parent, InitState]).

kick(_Serv) ->
    ok.
stop(Serv) ->
    gen_server:call(Serv, stop).

%% Setup

setup(Parent, #server{port=Port}=S0) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    erlang:link(S0#server.starter),
    {ok, AcceptSock} = listener(Port),
    #server{callback=CM,cargs=CA}=S0,
    Acceptor = start_acceptor(AcceptSock, CM, CA),
    S1 = S0#server{listener=AcceptSock, acceptor=Acceptor},
    gen_server:enter_loop(?MODULE, [{debug, [trace]}], S1).

listener(Port) ->
    listener(Port, 3).
listener(Port, N) when N > 0 ->
    case gen_tcp:listen(Port, 
			[binary, 
			 {reuseaddr, true},
			 {packet, fcgi}, 
			 {active, false}]) of
	{ok, Sock} ->
	    {ok, Sock};
	{error, eaddrinuse} ->
	    timer:sleep(5000),
	    listener(Port, N-1)
    end;
listener(_, _) ->
    fail.


    
%% Callbacks

%% init -- not needed when using enter_loop
init(_) ->
    not_needed.


handle_call(stop, _From, S) ->
    {stop, normal, ok, S};
handle_call(info, _From, S) ->
    {reply, S, S};
handle_call(get_count, _, #server{count=C}=S) ->
    {reply, C, S};
handle_call(_, _From, S) ->
    {reply, ok, S}.

handle_info(_Msg, S) ->
    {noreply, S}.

handle_cast({accepted, _PeerName}, 
	    #server{listener=L,callback=CM,cargs=CA}=S) ->
    %%erlang:display(_PeerName),
    Pid = start_acceptor(L, CM, CA),
    Count = S#server.count,
    {noreply, S#server{acceptor=Pid, count=Count+1}};
handle_cast(_, S) ->
    {noreply, S}.

code_change(_, _, S) ->
    {ok, S}.

terminate(_, _) ->
    ok.

%% Internal functions


start_acceptor(L, CM, CA) ->
    {ok, Pid} = ifastcgi_connection:start_link(
		  {socket, L, self(), CM, CA}),
    Pid.
