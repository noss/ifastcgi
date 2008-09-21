-module(ifastcgi).
-compile(export_all).

%% API

add_server(Port, Callback, Args) ->
    gen_server:call(ifastcgi_master, {add_server, Port, Callback, Args}).

