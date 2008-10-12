%%% File    : ifastcgi_crash.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : A crashing callback module
%%% Created : 11 Oct 2008 by Christian <chsu79@gmail.com>

-module(ifastcgi_crash).

-export([run/0, request/3]).

run() ->
    application:start(sasl),
    application:start(ifastcgi),
    ifastcgi:add_server(crash, 6464, ?MODULE, foo).


request(Args, Params, Input) ->
    %% erlang:display({?MODULE, Args, Params, Input}),
    Data = [ [atom_to_list(Key), ": ", Value, $\n] 
	     || {Key, Value} <- Params],
    a = b,
    {ok,
     [{'Content-Type', "text/plain"}],
     [<<"Hello world!\n">>,
      Data]}.


