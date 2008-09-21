%%% File    : ifastcgi_master.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : 
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>

-module(ifastcgi_master).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3, terminate/2]).
-export([start_link/0, start_link/1]).

-record(master, {}).

%% Start 

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% Callbacks

init(_) ->
    {ok, #master{} }.

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




