%%% File    : ifastcgi_connection.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : FastCGI protocol implementation
%%% Created : 21 Sep 2008 by Christian <chsu79@gmail.com>

%% FastCGI is a multiplexing protocol that carries multiple,
%% and concurrent, http requests over one and the same transport
%% protocol.
%%
%% FastCGI is transport protocol agnostic.  It could be used over
%% IPC pipes as well as over unix domain sockets. This implementation
%% here will only focus on TCP though.
%%
%% The implementation will make use of gen_tcp's {packet, fcgi} support
%% for the framing of FastCGI.
%%
%% A process will be started for each started http requests over FastCGI.
%% This process will relay all information directly to that process.

-module(ifastcgi_connection).
-export([start/0, start_link/1]).
-record(state, {server, 
		listen, conn, 
		keep_connection=false, 
		callback, cargs,
		requests
	       }).
-record(req, {r=0, role=0, params=[], input= <<>> }).

%% Worker start

start_link(Args) ->
    Pid = erlang:spawn_link(fun () -> loop(init(Args)) end),
    {ok, Pid}.

start() ->
    {ok, Sock} = gen_tcp:listen(6464, 
				[binary, 
				 {packet, fcgi}, 
				 {active, false}]),
    ifastcgi_connection:start_link({socket, Sock}).

%% Process setup

init({socket, Socket, Server, CMod, CArgs}) ->
    Reqs = dict:new(),
    accept(#state{server=Server, 
		  listen=Socket,
		  callback=CMod,
		  cargs=CArgs,
		  requests=Reqs
		 }).

accept(#state{server=Serv, listen=L}=S) ->
    {ok, Connection} = gen_tcp:accept(L),
    {ok, Peer} = inet:peername(Connection),
    gen_server:cast(Serv, {accepted, Peer}),
    S#state{conn=Connection}.

%% Main loop 

loop(#state{conn=C}=S) ->
    inet:setopts(C, [{active, once}]),
    receive 
	{tcp, C, Data} ->
	    request(S, Data);
	{tcp_close, C} ->
	    exit(tcp_close);
	{tcp_error, C, Reason} ->
	    exit({fastcgi_protocol_failure, Reason});
	Unknown ->
	    exit(Unknown)
    end.

%% Handle input fastcgi package

request(S, <<1:8, 
	    Type:8, 
	    Request:16/big, 
	    CLen:16/big,
	    _PLen:8,
	    _Reserved:8,
	    Content:CLen/binary,
	    _Padding/binary>>) ->
    request(S, type(Type), Request, Content).

request(S, Type, R, Content) ->
    Reqs = S#state.requests,
    case Type of
	begin_request ->
	    <<Role:16/big, _Flags:7/big, KeepConn:1, _Reserved:40/big>> = Content,
	    %% erlang:display({Type, R, {Role, _Flags, KeepConn, _Reserved}}),
	    Req = #req{r=R, role=Role},
	    loop(S#state{keep_connection= (KeepConn=:=1), 
			 requests=dict:store(R, Req, Reqs)});
	params ->
	    case dict:find(R, Reqs) of
		error ->
		    %% ignore
		    loop(S);
		{ok, #req{params=Params0}=Req0} ->
		    MoreParams = params(Content),
		    Params1 = Params0 ++ MoreParams,
		    Req1 = Req0#req{params=Params1},
		    loop(S#state{requests=dict:store(R, Req1, Reqs)})
	    end;
	stdin ->
	    %% erlang:display({Type, R, Content}),
	    case dict:find(R, Reqs) of
		error ->
		    %% ignore
		    loop(S);
		{ok, #req{input=Data0}=Req0} ->
		    case Content of
			<<>> ->
			    CallbackArgs = S#state.cargs,
			    Callback = S#state.callback,
			    {ok, Headers, Data} = 
				Callback:request(CallbackArgs, 
						 Req0#req.params, 
						 Req0#req.input),
			    
			    Out = [
				   stdout(R, Headers, Data),
				   stdout(R, <<>>),
				   end_request(R, 0, request_complete)
				  ],
			    gen_tcp:send(S#state.conn, Out),
			    case S#state.keep_connection of
				true ->
				    loop(S);
				false ->
			    gen_tcp:close(S#state.conn)
			    end;
			MoreData ->
			    Data1 = <<Data0/binary, MoreData/binary>>,
			    Req1 = Req0#req{input=Data1},
			    loop(S#state{requests=dict:store(R, Req1, Reqs)})
		    end
	    end
    end.

%% Build responses packets

respond(Type, R, Content) ->
    TypeCode = typecode(Type),
    Pad = pad(Content),
    CLen = byte_size(Content),
    PLen = byte_size(Pad),
    P = [<<1:8,
	  TypeCode:8,
	  R:16/big,
	  CLen:16/big,
	  PLen:8,
	  0:8>>,
	 Content,
	 Pad],
    %% erlang:display({out, Type, R, CLen, PLen, P}),
    P.

pad(Bin) ->
    case byte_size(Bin) rem 8 of
	0 -> <<>>;
	N -> list_to_binary(lists:duplicate(N, 0))
    end.

%% Wrapper functions for creating responses

end_request(R, AppStatus, ProtoStatus) ->
    ProtoStatusCode = protocode(ProtoStatus),
    Payload = <<AppStatus:32/big, ProtoStatusCode:8/big,0:24/big>>,
    respond(end_request, R, Payload).

stdout(R, Data) ->
    respond(stdout, R, erlang:iolist_to_binary(Data)).

stdout(R, Headers, Data) ->
    HData = [[atom_to_list(Key), $:, Value, $\r, $\n] || {Key, Value} <- Headers],
    stdout(R, [HData, "\r\n", Data]).


%% Constants

type(1) -> begin_request;
type(2) -> abort_request;
type(3) -> end_request;
type(4) -> params;
type(5) -> stdin;
type(6) -> stdout;
type(7) -> stderr;
type(8) -> data;
type(9) -> get_values;
type(10) -> get_values_result;
type(11) -> unknown_type.

typecode(stdout) -> 6;
typecode(end_request) -> 3.

%% Not needed yet
%proto(0) -> request_complete;
%proto(1) -> cant_mpx_conn;
%proto(2) -> overloaded;
%proto(3) -> unknown_role.

protocode(request_complete) -> 0;
protocode(cant_mpx_conn) -> 1;
protocode(overloaded) -> 2;
protocode(unknown_role) -> 3.
    

%% Decode params

params(<<>>) ->
    [];
params(<<KLen:8,VLen:8,Key:KLen/binary,Value:VLen/binary, Rest/binary>>) ->
    [{list_to_atom(binary_to_list(Key)), binary_to_list(Value)} | params(Rest)].

