%%%-------------------------------------------------------------------
%%% File    : ifastcgi_server_sup.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : A simple supervisor for each fastcgi port listener
%%%
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>
%%%-------------------------------------------------------------------
-module(ifastcgi_server_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% API
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    AChild = {fastcgi_server,{ifastcgi_server,start_link,[ifastcgi_master]},
	      temporary,2000,worker,[ifastcgi_server]},
    
    {ok,{{simple_one_for_one,10,300}, [AChild]}}.
