-module(dht_node).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sock, node_id, requests, t = 0, routes}).
-record(request, {t, addr, resend, caller, last_sent, ntry = 0}).

-define(PKT_TIMEOUT, 2000).
-define(MAX_TRIES, 3).

-define(CALL_TIMEOUT, 8000).

%%====================================================================
%% API
%%====================================================================
ip_port_to_addr({A, B, C, D}, Port) ->
    <<A:8, B:8, C:8, D:8, Port:16/big>>.

addr_to_ip_port(<<A, B, C, D, Port:16/big>>) ->
    {{A, B, C, D}, Port}.

addr_to_s(<<A, B, C, D, Port:16/big>>) ->
    io_lib:format("~B.~B.~B.~B:~B", [A, B, C, D, Port]).

start_link(Port) ->
    start_link(generate_node_id(), Port).

start_link(NodeId, Port) ->
    gen_server:start_link(?MODULE, [NodeId, Port], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([NodeId, Port]) ->
    {ok, Sock} = gen_udp:open(Port, [binary, {active, true}]),
    Requests = ets:new(requests, [set, private, {keypos, #request.t}]),
    next_state(ok, #state{sock = Sock, node_id = NodeId, requests = Requests, routes = routes:new(NodeId)}).

handle_call({request, Addr, Q}, From, State) ->
    handle_call({request, Addr, Q, []}, From, State);
handle_call({request, Addr, Q, As}, From,
	    #state{sock = Sock, node_id = NodeId, t = T1} = State) ->
    I = self(),
    {T, T2} = next_t(T1),
    spawn_link(fun() ->
		       io:format("request ~p to ~s~n", [Q, addr_to_s(Addr)]),
		       %% {"t":"aa", "y":"q", "q":"ping", "a":{"id":"abcdefghij0123456789"}}
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
		       gen_server:cast(I, {insert_request, Request}),
		       Send()
	       end),
    next_state(noreply, State#state{t = T2}).

handle_cast({insert_request, Request}, #state{requests = Requests} = State) ->
    ets:insert(Requests, Request),
    next_state(noreply, State);

handle_cast({packet, Addr, Pkt}, State) ->
    T = dict_get(<<"t">>, Pkt, <<>>),
    case dict_get(<<"y">>, Pkt, <<>>) of
	<<"r">> ->
	    R = dict_get(<<"r">>, Pkt, []),
	    handle_reply(Addr, T, {ok, R}, State);
	<<"e">> ->
	    E = dict_get(<<"e">>, Pkt, []),
	    handle_reply(Addr, T, {error, E}, State);
	<<"q">> ->
	    spawn_link(fun() ->
			       Q = dict_get(<<"q">>, Pkt, <<>>),
			       A = dict_get(<<"a">>, Pkt, []),
			       io:format("question from ~s: ~p~n", [addr_to_s(Addr), Q]),
			       handle_question(Addr, T, Q, A, State)
		       end),
	    next_state(noreply, State);
	_ ->
	    next_state(noreply, State)
    end.

handle_info({udp, Sock, IP, Port, PktBin}, #state{sock = Sock} = State) ->
    I = self(),
    spawn_link(
      fun() ->
	      Addr = ip_port_to_addr(IP, Port),
	      case (catch benc:parse(PktBin)) of
		  {'EXIT', Reason} ->
		      io:format("Received garbage from ~s: ~p", [addr_to_s(Addr), Reason]);
		  Pkt ->
		      gen_server:cast(I, {packet, Addr, Pkt})
	      end
      end),
    next_state(noreply, State);

handle_info(timeout, #state{requests = Requests} = State) ->
    Now = util:mk_timestamp_ms(),
    TimedOut = ets:foldl(fun(#request{last_sent = LastSent} = Req, L)
			    when LastSent + ?PKT_TIMEOUT =< Now ->
				 [Req | L];
			    (_, L) -> L
			 end, [], Requests),
    Resends = lists:foldl(
		fun(#request{caller = Caller, ntry = Ntry} = Req, Resends)
		   when Ntry >= ?MAX_TRIES ->
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
	    next_state(noreply, State);
	true ->
	    next_state(noreply, State)
    end;

handle_info(_Info, State) ->
    next_state(noreply, State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

next_state(Result, State) ->
    {Result, State, 100}.

handle_reply(Addr, T, Result, #state{requests = Requests} = State) ->
    case ets:lookup(Requests, T) of
	[#request{caller = Caller}] ->
	    ets:delete(Requests, T),
	    gen_server:reply(Caller, Result);
	[] ->
	    %% Late?
	    io:format("Unexpected packet from ~p with T=~p~n", [addr_to_s(Addr), T]),
	    ignore
    end,
    next_state(noreply, State).


handle_question(Addr, T, <<"ping">>, _As, #state{node_id = NodeId,
						 sock = Sock}) ->
    {IP, Port} = addr_to_ip_port(Addr),
    Pkt = [{<<"t">>, T},
	   {<<"y">>, <<"r">>},
	   {<<"r">>,
	    [{<<"id">>, NodeId}]
	   }],
    gen_udp:send(Sock, IP, Port, benc:to_binary(Pkt));

handle_question(Addr, T, _Q, _As, #state{sock = Sock}) ->
    {IP, Port} = addr_to_ip_port(Addr),
    Pkt = [{<<"t">>, T},
	   {<<"y">>, <<"e">>},
	   {<<"e">>, [204, <<"I didn't hear you. Lala lala la.">>]
	   }],
    gen_udp:send(Sock, IP, Port, benc:to_binary(Pkt)).

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


split_contact_nodes(<<PeerId:20/binary, Addr:6/binary, Rest/binary>>) ->
    [{Addr, PeerId} | split_contact_nodes(Rest)];
split_contact_nodes(_) ->
    [].

