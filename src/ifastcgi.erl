-module(ifastcgi).
-compile(export_all).

%% API

-define(MASTER, ifastcgi_master).

add_server(Name, Port, Callback, Args) ->
    Timeout = 120000,
    gen_server:call(?MASTER, 
		    {add_server, self(), Name, Port, 
		     {Callback, Args}}, 
		    Timeout).


del_server(Name) ->
    gen_server:call(?MASTER, {del_server, Name}).


which_servers() ->
    gen_server:call(?MASTER, which_servers).

info_server(ServerName) ->
    gen_server:call(?MASTER, {info, ServerName}).

    
