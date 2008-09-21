%%% File    : ifastcgi_server.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : Manages a iserver_connection worker
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>

-module(ifastcgi_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3, terminate/2]).
-export([start_link/1, kick/1]).

-record(server, {listener, current, count=0}).

%% Start 

start_link(Port) ->
    gen_server:start_link(?MODULE, [Port], []).

kick(Serv) ->
    gen_server:cast(Serv, {accepted, foo}).

%% Callbacks

init([Port]) ->
    {ok, Sock} = gen_tcp:listen(Port, 
				[binary, 
				 {packet, fcgi}, 
				 {active, false}]),
    {ok, #server{listener=Sock}}.

handle_call(get_count, _, #server{count=C}=S) ->
    {reply, C, S};
handle_call(_, _From, S) ->
    {reply, ok, S}.

handle_info(_Msg, S) ->
    {noreply, S}.

handle_cast({accepted, PeerName}, #server{listener=L}=S) ->
    erlang:display(PeerName),
    {ok, Pid} = ifastcgi_connection:start_link({socket, L, self(), ifastcgi_test, args}),
    Count = S#server.count,
    {noreply, S#server{current=Pid, count=Count+1}};
handle_cast(_, S) ->
    {noreply, S}.

code_change(_, _, S) ->
    {ok, S}.

terminate(_, _) ->
    ok.



