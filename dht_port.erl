-module(dht_port).

-behaviour(gen_server).

%% API
-export([start_link/3, ping/2, find_node/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sock, node_id, requests, t = 0, question_cb}).
-record(request, {t, addr, resend, caller, last_sent, ntry = 0}).

-define(PKT_TIMEOUT, 2000).
-define(MAX_TRIES, 3).

-define(CALL_TIMEOUT, 9000).

%%====================================================================
%% API
%%====================================================================
start_link(NodeId, Port, QuestionCB) ->
    gen_server:start_link(?MODULE, [NodeId, Port, QuestionCB], []).

ping(Pid, Addr) ->
    io:format("ping ~p ~s~n",[Pid,addr:to_s(Addr)]),
    case gen_server:call(Pid, {request, Addr, <<"ping">>}, ?CALL_TIMEOUT) of
	{ok, R} ->
	    case dict_get(<<"id">>, R, false) of
		<<NodeId:20/binary>> -> {ok, NodeId};
		_ -> error
	    end;
	_ -> error
    end.

find_node(Pid, Addr, NodeId) ->
    io:format("find_node ~p ~s ~p~n",[Pid,addr:to_s(Addr),NodeId]),
    case gen_server:call(Pid, {request, Addr,
			       <<"find_node">>, [<<"target">>, NodeId]},
			 ?CALL_TIMEOUT) of
	{ok, R} ->
	    Nodes = dict_get(<<"nodes">>, R, <<>>),
	    NodeIdsAddrs = split_contact_nodes(Nodes),
	    {ok, NodeIdsAddrs};
	E -> E
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([NodeId, Port, QuestionCB]) ->
    {ok, Sock} = gen_udp:open(Port, [binary, {active, true}]),
    Requests = ets:new(requests, [set, private, {keypos, #request.t}]),
    {ok, #state{sock = Sock,
		requests = Requests,
		node_id = NodeId,
		question_cb = QuestionCB}}.

handle_call({request, Addr, Q}, From, State) ->
    handle_call({request, Addr, Q, []}, From, State);
handle_call({request, Addr, Q, As}, From,
	    #state{sock = Sock, node_id = NodeId, t = T1} = State) ->
    I = self(),
    {T, T2} = next_t(T1),
    spawn_link(fun() ->
		       io:format("request ~p to ~s~n", [Q, addr:to_s(Addr)]),
		       %% {"t":"aa", "y":"q", "q":"ping", "a":{"id":"abcdefghij0123456789"}}
		       Pkt = [{<<"t">>, T},
			      {<<"y">>, <<"q">>},
			      {<<"q">>, Q},
			      {<<"a">>,
			       [{<<"id">>, NodeId} | As]
			      }],
		       PktBin = benc:to_binary(Pkt),
		       {IP, Port} = addr:to_ip_port(Addr),
		       Send = fun() ->
				      gen_udp:send(Sock, IP, Port, PktBin)
			      end,
		       Request = #request{t = T,
					  addr = Addr,
					  resend = Send,
					  caller = From,
					  last_sent = util:mk_timestamp_ms()},
		       gen_server:cast(I, {insert_request, Request}),
		       io:format("send ~p~n",[Pkt]),
		       Send()
	       end),
    next_state(noreply, State#state{t = T2}).

handle_cast({insert_request, Request}, #state{requests = Requests} = State) ->
    ets:insert(Requests, Request),
    next_state(noreply, State);

handle_cast({packet, Addr, Pkt}, #state{question_cb = QuestionCB} = State) ->
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
			       io:format("question from ~s: ~p~n", [addr:to_s(Addr), Q]),
			       QuestionCB(Addr, T, Q, A)
		       end),
	    next_state(noreply, State);
	_ ->
	    next_state(noreply, State)
    end.

handle_info({udp, Sock, IP, Port, PktBin}, #state{sock = Sock} = State) ->
    I = self(),
    spawn_link(
      fun() ->
	      Addr = addr:from_ip_port(IP, Port),
	      case (catch benc:parse(PktBin)) of
		  {'EXIT', Reason} ->
		      io:format("Received garbage from ~s: ~p~n", [addr:to_s(Addr), Reason]);
		  Pkt ->
		      io:format("Received from ~s: ~p~n", [addr:to_s(Addr), Pkt]),
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
    lists:foreach(
      fun(#request{caller = Caller, ntry = Ntry} = Req)
	 when Ntry >= ?MAX_TRIES ->
	      ets:delete(Requests, Req#request.t),
	      gen_server:reply(Caller, timeout);
	 (#request{ntry = Ntry} = Req) ->
	      ets:insert(Requests, Req#request{ntry = Ntry + 1,
					       last_sent = Now}),
	      io:format("Resending~n"),
	      (Req#request.resend)()
      end, TimedOut),
    next_state(noreply, State);

handle_info(_Info, State) ->
    next_state(noreply, State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

next_state(Result, #state{requests = Requests} = State) ->
    Now = util:mk_timestamp_ms(),
    NextTimeout = ets:foldl(fun(#request{last_sent = LastSent}, NextTimeout1) ->
				    NextTimeout2 = LastSent + ?PKT_TIMEOUT,
				    if
					NextTimeout1 == infinity ->
					    NextTimeout2;
					NextTimeout1 < NextTimeout2 ->
					    NextTimeout1;
					true ->
					    NextTimeout2
				    end
			    end, infinity, Requests),
    Delay = case NextTimeout of
		infinity -> infinity;
		_ when NextTimeout =< Now -> 1;
		_ -> NextTimeout - Now
	    end,
    {Result, State, Delay}.

handle_reply(Addr, T, Result, #state{requests = Requests} = State) ->
    case ets:lookup(Requests, T) of
	[#request{caller = Caller}] ->
	    ets:delete(Requests, T),
	    gen_server:reply(Caller, Result);
	[] ->
	    %% Late?
	    io:format("Unexpected packet from ~p with T=~p~n", [addr:to_s(Addr), T]),
	    ignore
    end,
    next_state(noreply, State).

dict_get(Key, Dict, Default) ->
    case lists:keysearch(Key, 1, Dict) of
	{value, {_, Value}} -> Value;
	_ -> Default
    end.

next_t(T1) when T1 > 16#FFFF ->
    next_t(0);
next_t(T1) ->
    T = <<((T1 bsr 16) band 16#FF):8, (T1 band 16#FF):8>>,
    {T, T1 + 1}.

split_contact_nodes(<<PeerId:20/binary, Addr:6/binary, Rest/binary>>) ->
    [{PeerId, Addr} | split_contact_nodes(Rest)];
split_contact_nodes(_) ->
    [].
