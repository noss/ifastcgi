%%%-------------------------------------------------------------------
%%% File    : ifastcgi_sup.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : 
%%%
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>
%%%-------------------------------------------------------------------
-module(ifastcgi_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

%% API

start_link() ->
    start_link([]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%% Supervisor callbacks
init(_Args) ->
    %% A Simple supervisor for each port listener
    ServerSup = {server_supervisor, {ifastcgi_server_sup, start_link, []},
		 permanent, 2000, worker, [ifastcgi_server_sup]},

    %% A master to rule them all
    Master = {master, {ifastcgi_master, start_link, []},
	      permanent, 2000, worker, [ifastcgi_master]},
    
    %% Restart both on failure
    {ok,{{one_for_all,0,1}, [ServerSup, Master]}}.

