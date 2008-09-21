%%%-------------------------------------------------------------------
%%% File    : ifastcgi_app.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : 
%%%
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>
%%%-------------------------------------------------------------------
-module(ifastcgi_app).
-behaviour(application).
-export([start/2, stop/1]).

%% Application callbacks

start(_Type, StartArgs) ->
    case ifastcgi_sup:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.
