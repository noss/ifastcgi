-module(ifastcgi).
-compile(export_all).

%% API

add_server(Name, Port, Callback, Args) ->
    gen_server:call(ifastcgi_master, {add_server, Name, Port, {Callback, Args}}).

del_server(Name) ->
    gen_server:call(ifastcgi_master, {del_server, Name}).

