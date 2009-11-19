-module(dht_node).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, ping/2, find_node/2, get_peers/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sock, node_id, requests, t = 0}).
-record(request, {t, addr, resend, caller, last_sent, ntry = 0}).

-define(PKT_TIMEOUT, 2000).
-define(MAX_TRIES, 3).

-define(CALL_TIMEOUT, 8000).
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

ping(Pid, Addr) ->
    gen_server:call(Pid, {request, Addr, <<"ping">>}, ?CALL_TIMEOUT).

find_node(Pid, InfoHash) ->
    Ask = fun(Addr) ->
		  catch gen_server:call(Pid,
					{request, Addr,
					 <<"find_node">>, [{<<"target">>, InfoHash}]},
					?CALL_TIMEOUT)
	  end,
    find_node1(Ask, InfoHash, []).

find_node1(Ask, InfoHash, Seen) ->
    Nearest = nodes:get_nearest(InfoHash),
    %% Generate a list of those who are nearest but not Seen yet
    ToAsk = lists:foldl(fun(Addr, ToAsk) ->
				case lists:member(Addr, Seen) of
				    true -> ToAsk;
				    false -> [Addr | ToAsk]
				end
			end, [], Nearest),
    io:format("finde node: ~p seen, ~p to ask~n",[length(Seen), length(ToAsk)]),
    case ToAsk of
	%% Asked all nearest, but no answer
	[] -> Nearest;
	%% Further nodes
	_ ->
	    util:pmap(Ask, ToAsk),
	    find_node1(Ask, InfoHash, ToAsk ++ Seen)
    end.

get_peers(Pid, InfoHash) ->
    find_node(Pid, InfoHash),
    Nodes = nodes:get_nearest(InfoHash, good),
    io:format("get_peers: ~p nodes to ask for peers~n", [length(Nodes)]),
    lists:append(
      util:pmap(
	fun(Addr) ->
		case catch gen_server:call(Pid,
					   {request, Addr,
					    <<"get_peers">>, [{<<"info_hash">>, InfoHash}]},
					   ?CALL_TIMEOUT) of
		    {ok, R} ->
			case dict_get(<<"values">>, R, false) of
			    false -> [];
			    Values ->
				lists:filter(fun(<<_:6/binary>>) -> true;
						(_) -> false
					     end, Values)
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

handle_call({request, Addr, Q}, From, State) ->
    handle_call({request, Addr, Q, []}, From, State);
handle_call({request, {Host, Port}, Q, As}, From, State) ->
    I = self(),
    spawn_link(fun() ->
		       case inet:getaddrs(Host, inet) of
			   {ok, IPs} ->
			       IP = lists:nth(random:uniform(length(IPs)), IPs),
			       Addr = ip_port_to_addr(IP, Port),
			       Result = gen_server:call(I, {request, Addr, Q, As}, ?CALL_TIMEOUT),
			       gen_server:reply(From, Result);
			   _ ->
			       gen_server:reply(From, {error, nxdomain})
		       end
	       end),
    {noreply, State};
handle_call({request, Addr, Q, As}, From,
	    #state{sock = Sock, node_id = NodeId, requests = Requests, t = T1} = State) ->
    io:format("request ~p to ~p~n", [Q, Addr]),
    %% {"t":"aa", "y":"q", "q":"ping", "a":{"id":"abcdefghij0123456789"}}
    {T, T2} = next_t(T1),
    Pkt = [{<<"t">>, T},
	   {<<"y">>, <<"q">>},
	   {<<"q">>, Q},
	   {<<"a">>,
	    [{<<"id">>, NodeId} | As]
	   }],
    PktBin = benc:to_binary(Pkt),
    {IP, Port} = addr_to_ip_port(Addr),
    Send = fun() ->
		   gen_udp:send(Sock, IP, Port, PktBin)
	   end,
    Request = #request{t = T,
		       addr = Addr,
		       resend = Send,
		       caller = From,
		       last_sent = util:mk_timestamp_ms()},
    ets:insert(Requests, Request),
    Send(),
    {noreply, State#state{t = T2}, ?TICK_INTERVAL}.

handle_cast(_Msg, State) ->
    {noreply, State, ?TICK_INTERVAL}.

handle_info({udp, Sock, IP, Port, Packet},
	    #state{sock = Sock, requests = Requests, node_id = NodeId} = State) ->
    Addr = ip_port_to_addr(IP, Port),
    case (catch benc:parse(Packet)) of
	{'EXIT', Reason} ->
	    io:format("Received garbage from ~p: ~p", [Addr, Reason]);
	Reply ->
	    T = dict_get(<<"t">>, Reply, <<>>),
	    Result =
		case dict_get(<<"y">>, Reply, <<>>) of
		    <<"r">> ->
			case dict_get(<<"r">>, Reply, false) of
			    false -> {ok, []};
			    R ->
				see_r(Addr, R),
				see_r_nodes(R),
				{ok, R}
			end;
		    <<"e">> ->
			{error, dict_get(<<"e">>, Reply, [])};
		    <<"q">> ->
			Q = dict_get(<<"q">>, Reply, <<>>),
			As1 = dict_get(<<"a">>, Reply, []),
			io:format("question from ~p: ~p~n", [Addr, Q]),
			{question, Q, As1};
		    _ -> {error, []}
		end,

	    case Result of

		{question, <<"ping">>, As2} ->
		    NodeId1 = dict_get(<<"id">>, As2, <<>>),
		    nodes:seen(Addr, NodeId1, unsure),
		    Pkt = [{<<"t">>, T},
			   {<<"y">>, <<"r">>},
			   {<<"r">>,
			    [{<<"id">>, NodeId}]
			   }],
		    gen_udp:send(Sock, IP, Port, benc:to_binary(Pkt));

		{question, <<"find_node">>, As2} ->
		    NodeId1 = dict_get(<<"id">>, As2, <<>>),
		    nodes:seen(Addr, NodeId1, unsure),
		    Target = dict_get(<<"target">>, As2, <<>>),
		    CompactNodes = list_to_binary(nodes:get_nearest(Target, good)),
		    Pkt = [{<<"t">>, T},
			   {<<"y">>, <<"r">>},
			   {<<"r">>,
			    [{<<"id">>, NodeId},
			     {<<"nodes">>, CompactNodes}]
			   }],
		    gen_udp:send(Sock, IP, Port, benc:to_binary(Pkt));

		{question, _, _} ->
		    Pkt = [{<<"t">>, T},
			   {<<"y">>, <<"e">>},
			   {<<"e">>, [204, <<"I didn't hear you. Lala lala la.">>]
			   }],
		    gen_udp:send(Sock, IP, Port, benc:to_binary(Pkt));

		_ ->
		    case ets:lookup(Requests, T) of
			[#request{caller = Caller}] ->
			    ets:delete(Requests, T),
			    gen_server:reply(Caller, Result);
			[] ->
			    %% Late?
			    io:format("Unexpected packet from ~p with T=~p~n", [Addr, T]),
			    ignore
		    end

	    end
    end,
    {noreply, State, ?TICK_INTERVAL};

handle_info(timeout, #state{requests = Requests, node_id = NodeId} = State) ->
    Now = util:mk_timestamp_ms(),
    TimedOut = ets:foldl(fun(#request{last_sent = LastSent} = Req, L)
			    when LastSent + ?PKT_TIMEOUT =< Now ->
				 [Req | L];
			    (_, L) -> L
			 end, [], Requests),
    Resends = lists:foldl(
		fun(#request{caller = Caller, ntry = Ntry,
			     addr = Addr} = Req, Resends)
		   when Ntry >= ?MAX_TRIES ->
			nodes:seen(Addr, <<>>, bad),
			ets:delete(Requests, Req#request.t),
			gen_server:reply(Caller, timeout),
			Resends;
		   (#request{ntry = Ntry} = Req, Resends) ->
			ets:insert(Requests, Req#request{ntry = Ntry + 1,
							 last_sent = Now}),
			(Req#request.resend)(),
			Resends + 1
		end, 0, TimedOut),
    if
	Resends > 0 ->
	    nothing;
	true ->
	    lists:foreach(
	      fun({Addr1, NodeId1}) ->
		      I = self(),
		      spawn_link(
			fun() ->
				catch gen_server:call(I,
						      {request, Addr1,
						       <<"find_node">>, [{<<"target">>, NodeId1}]},
						      ?CALL_TIMEOUT)
			end)
	      end, nodes:neighbors_to_maintain(NodeId, 8))
    end,
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

dict_get(Key, Dict, Default) ->
    case lists:keysearch(Key, 1, Dict) of
	{value, {_, Value}} -> Value;
	_ -> Default
    end.

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

see_r(Addr, R) ->
    case dict_get(<<"id">>, R, false) of
	false -> ignore;
	NodeId -> nodes:seen(Addr, NodeId)
    end.

see_r_nodes(R) ->
    case dict_get(<<"nodes">>, R, false) of
	false -> ignore;
	Nodes ->
	    lists:foreach(fun({Addr, NodeId}) ->
				  nodes:seen(Addr, NodeId, unsure)
			  end, split_contact_nodes(Nodes))
    end.


split_contact_nodes(<<PeerId:20/binary, Addr:6/binary, Rest/binary>>) ->
    [{Addr, PeerId} | split_contact_nodes(Rest)];
split_contact_nodes(_) ->
    [].

