-module(dht_node).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, ping/3, find_node/2, get_peers/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sock, node_id, requests, t = 0}).
-record(request, {t, host, port, resend, caller, last_sent, ntry = 0}).

-define(PKT_TIMEOUT, 1000).
-define(MAX_TRIES, 3).

-define(CALL_TIMEOUT, 40000).
-define(TICK_INTERVAL, 100).

%%====================================================================
%% API
%%====================================================================
ip_port_to_addr({A, B, C, D}, Port) ->
    <<A:8, B:8, C:8, D:8, Port:16/big>>.

addr_to_ip_port(<<A, B, C, D, Port:16/big>>) ->
    {{A, B, C, D}, Port}.

start_link(Port) ->
    start_link(generate_node_id(), Port).

start_link(NodeId, Port) ->
    gen_server:start_link(?MODULE, [NodeId, Port], []).

ping(Pid, Host, Port) ->
    gen_server:call(Pid, {request, Host, Port, <<"ping">>}, ?CALL_TIMEOUT).

find_node(Pid, InfoHash) ->
    Ask = fun({Host1, Port1}) ->
		  io:format("Asking ~p:~p~n", [Host1, Port1]),
		  catch gen_server:call(Pid,
					{request, Host1, Port1,
					 <<"find_node">>, [{<<"target">>, InfoHash}]},
					?CALL_TIMEOUT)
	  end,
    find_node1(Ask, InfoHash, []).

find_node1(Ask, InfoHash, Seen) ->
    Nearest = nodes:get_nearest(InfoHash),
    %% Generate a list of those who are nearest but not Seen yet
    ToAsk = lists:foldl(fun(IpPort, ToAsk) ->
				case lists:member(IpPort, Seen) of
				    true -> ToAsk;
				    false -> [IpPort | ToAsk]
				end
			end, [], Nearest),
    case ToAsk of
	%% Asked all nearest, but no answer
	[] -> Nearest;
	%% Further nodes
	_ ->
	    util:pmap(Ask, ToAsk),
	    find_node1(Ask, InfoHash, ToAsk ++ Seen)
    end.

get_peers(Pid, InfoHash) ->
    Nodes = find_node(Pid, InfoHash),
    io:format("get_peers: ~p nodes to ask for peers~n", [length(Nodes)]),
    lists:append(
      util:pmap(
	fun({Host, Port}) ->
		case catch gen_server:call(Pid,
					   {request, Host, Port,
					    <<"get_peers">>, [{<<"info_hash">>, InfoHash}]},
					   ?CALL_TIMEOUT) of
		    {ok, R} ->
			case lists:keysearch(<<"values">>, 1, R) of
			    {value, {_, Values}} ->
				[{{A, B, C, D}, Port1}
				 || <<A:8, B:8, C:8, D:8, Port1:16/big>> <- Values];
			    _ -> []
			end;
		    _ -> []
		end
	end, Nodes)
     ).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([NodeId, Port]) ->
    {ok, Sock} = gen_udp:open(Port, [binary, {active, true}]),
    Requests = ets:new(requests, [set, private, {keypos, #request.t}]),
    {ok, #state{sock = Sock, node_id = NodeId, requests = Requests}, ?TICK_INTERVAL}.

handle_call({request, Host, Port, Q}, From, State) ->
    handle_call({request, Host, Port, Q, []}, From, State);
handle_call({request, Host, Port, Q, As}, From,
	    #state{sock = Sock, node_id = NodeId, requests = Requests, t = T1} = State) ->
    %% {"t":"aa", "y":"q", "q":"ping", "a":{"id":"abcdefghij0123456789"}}
    {T, T2} = next_t(T1),
    Pkt = [{<<"t">>, T},
	   {<<"y">>, <<"q">>},
	   {<<"q">>, Q},
	   {<<"a">>,
	    [{<<"id">>, NodeId} | As]
	   }],
    PktBin = benc:to_binary(Pkt),
    Send = fun() ->
		   io:format("Sending to ~p:~p: ~p~n", [Host, Port, Pkt]),
		   gen_udp:send(Sock, Host, Port, PktBin)
	   end,
    Request = #request{t = T,
		       host = Host,
		       port = Port,
		       resend = Send,
		       caller = From,
		       last_sent = util:mk_timestamp_ms()},
    ets:insert(Requests, Request),
    Send(),
    {noreply, State#state{t = T2}, ?TICK_INTERVAL}.

handle_cast(_Msg, State) ->
    {noreply, State, ?TICK_INTERVAL}.

handle_info({udp, Sock, IP, Port, Packet},
	    #state{sock = Sock, requests = Requests} = State) ->
    case (catch benc:parse(Packet)) of
	{'EXIT', Reason} ->
	    io:format("Received garbage from ~p:~p~n~p", [IP, Port, Reason]);
	Reply ->
	    io:format("Received from ~p:~p: ~p~n", [IP, Port, Reply]),
	    Result =
		case lists:keysearch(<<"y">>, 1, Reply) of
		    {value, {_, <<"r">>}} ->
			case lists:keysearch(<<"r">>, 1, Reply) of
			    {value, {_, R}} ->
				see_r(IP, Port, R),
				see_r_nodes(R),
				{ok, R};
			    _ -> {ok, []}
			end;
		    {value, {_, <<"e">>}} ->
			case lists:keysearch(<<"e">>, 1, Reply) of
			    {value, {_, E}} -> {error, E};
			    _ -> {error, []}
			end;
		    _ -> {error, []}
		end,
	    case lists:keysearch(<<"t">>, 1, Reply) of
		{value, {_, T}} ->
		    case ets:lookup(Requests, T) of
			[#request{caller = Caller}] ->
			    ets:delete(Requests, T),
			    gen_server:reply(Caller, Result);
			[] ->
			    %% Late?
			    io:format("Unexpected packet from ~p:~p~n", [IP, Port]),
			    ignore
		    end;
		_ ->
		    ignore
	    end
    end,
    {noreply, State, ?TICK_INTERVAL};

handle_info(timeout, #state{requests = Requests} = State) ->
    Now = util:mk_timestamp_ms(),
    TimedOut = ets:foldl(fun(#request{last_sent = LastSent} = Req, L)
			    when LastSent + ?PKT_TIMEOUT =< Now ->
				 [Req | L];
			    (_, L) -> L
			 end, [], Requests),
    lists:foreach(fun(#request{caller = Caller, ntry = Ntry} = Req)
		     when Ntry >= ?MAX_TRIES ->
			  ets:delete(Requests, Req#request.t),
			  gen_server:reply(Caller, timeout);
		     (#request{ntry = Ntry} = Req) ->
			  ets:insert(Requests, Req#request{ntry = Ntry + 1,
							   last_sent = Now}),
			  (Req#request.resend)()
		  end, TimedOut),
    {noreply, State, ?TICK_INTERVAL};

handle_info(_Info, State) ->
    {noreply, State, ?TICK_INTERVAL}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

generate_node_id() ->
    list_to_binary(
      [random:uniform(256) - 1
       || _ <- lists:seq(1, 20)]
     ).

next_t(T1) when T1 > 16#FFFF ->
    next_t(0);
next_t(T1) ->
    T = <<((T1 bsr 16) band 16#FF):8, (T1 band 16#FF):8>>,
    {T, T1 + 1}.

see_r(IP, Port, R) ->
    case lists:keysearch(<<"id">>, 1, R) of
	{value, {_, NodeId}} ->
	    nodes:seen(IP, Port, NodeId);
	_ -> ignore
    end.

see_r_nodes(R) ->
    case lists:keysearch(<<"nodes">>, 1, R) of
	{value, {_, Nodes}} ->
	    lists:foreach(fun({Addr, NodeId}) ->
				  {IP, Port} = addr_to_ip_port(Addr),
				  nodes:seen(IP, Port, NodeId, unknown)
			  end, split_contact_nodes(Nodes));
	_ -> ignore
    end.


split_contact_nodes(<<PeerId:20/binary, Addr:6/binary, Rest/binary>>) ->
    [{Addr, PeerId} | split_contact_nodes(Rest)];
split_contact_nodes(_) ->
    [].

