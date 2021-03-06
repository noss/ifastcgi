%%% File    : ifastcgi_test.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : Test callback module
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>

-module(ifastcgi_test).
-export([run/0, request/3]).

run() ->
    application:start(sasl),
    application:start(ifastcgi),
    ifastcgi:add_server(test, 6464, ?MODULE, foo).


request(Args, Params, Input) ->
    %% erlang:display({?MODULE, Args, Params, Input}),
    Data = [ [atom_to_list(Key), ": ", Value, $\n] 
	     || {Key, Value} <- Params],
    {ok,
     [{'Content-Type', "text/plain"}],
     [<<"Hello world!\n">>,
      Data]}.

