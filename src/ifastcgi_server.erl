%%% File    : ifastcgi_server.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : Manages a iserver_connection worker
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>

-module(ifastcgi_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3, terminate/2]).
-export([start_link/4, kick/1]).

-record(server, {master, listener, current, count=0, callback, cargs}).

%% Start 

start_link(Master,Port, CallbackMod, CallbackArg) ->
    gen_server:start_link(?MODULE, [Master, Port,CallbackMod,CallbackArg], []).

kick(Serv) ->
    gen_server:cast(Serv, {accepted, foo}).

%% Callbacks

init([Master, Port,CallbackMod,CallbackArg]) ->
    {ok, Sock} = gen_tcp:listen(Port, 
				[binary, 
				 {packet, fcgi}, 
				 {active, false}]),
    {ok, #server{master=Master,
		 listener=Sock,
		 callback = CallbackMod,
		 cargs = CallbackArg
		}}.

handle_call(get_count, _, #server{count=C}=S) ->
    {reply, C, S};
handle_call(_, _From, S) ->
    {reply, ok, S}.

handle_info(_Msg, S) ->
    {noreply, S}.

handle_cast({accepted, PeerName}, 
	    #server{listener=L,callback=CM,cargs=CA}=S) ->
    %%erlang:display(PeerName),
    {ok, Pid} = ifastcgi_connection:start_link(
		  {socket, L, self(), CM, CA}),
    Count = S#server.count,
    {noreply, S#server{current=Pid, count=Count+1}};
handle_cast(_, S) ->
    {noreply, S}.

code_change(_, _, S) ->
    {ok, S}.

terminate(_, _) ->
    ok.



