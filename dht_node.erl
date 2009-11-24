-module(dht_node).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, hint/3, generate_node_id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {node_id, port, routes}).

-define(MIN_TICK, 50).  %% 50 ms, no more than 20 pkt/s

%%====================================================================
%% API
%%====================================================================
start_link(Port) ->
    NodeId = generate_node_id(),
    start_link(Port, NodeId).
start_link(Port, NodeId) ->
    gen_server:start_link(?MODULE, [NodeId, Port], []).

hint(Pid, Host, Port) when is_list(Host) ->
    case inet:getaddrs(Host, inet) of
	{ok, Addrs} ->
	    Addr = lists:nth(random:uniform(length(Addrs)), Addrs),
	    hint(Pid, Addr, Port);
	E -> E
    end;
hint(Pid, {_, _, _, _} = Host, Port) ->
    Addr = addr:from_ip_port(Host, Port),
    gen_server:call(Pid, {hint, Addr}, 10000).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([NodeId, UdpPort]) ->
    I = self(),
    QuestionCB = fun(Addr, T, Q, A) ->
			 %% TODO: implement replying, insert peer from incoming pings
			 gen_server:call(I, {question, Addr, T, Q, A})
		 end,
    {ok, DhtPort} = dht_port:start_link(NodeId, UdpPort, QuestionCB),
    {ok, #state{port = DhtPort,
		node_id = NodeId,
		routes = dht_routes:new(NodeId)}}.

handle_call({question, Addr, T, Q, As}, From, State) ->
    next_state(noreply, State);

handle_call({hint, Addr}, From, #state{node_id = NodeId,
				       port = Port} = State) ->
    I = self(),
    spawn_link(fun() ->
		       R = run_discovery(I, Addr, NodeId, Port),
		       gen_server:reply(From, R)
	       end),
    next_state(noreply, State).

handle_cast({mark, Addr, NodeId, Status}, #state{routes = Routes1} = State) ->
    Routes2 = dht_routes:mark(Routes1, Addr, NodeId, Status),
    next_state(noreply, State#state{routes = Routes2});

handle_cast({insert, Addr, NodeId}, #state{routes = Routes1} = State) ->
    Routes2 = dht_routes:insert(Routes1, Addr, NodeId),
    next_state(noreply, State#state{routes = Routes2}).

handle_info(timeout, State) ->
    next_state(noreply, State).

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

next_state(Result, #state{routes = Routes1,
			  port = Port} = State) ->
    NA = dht_routes:next_action(Routes1),
    io:format("next_action: ~p~n", [NA]),
    case NA of
	{wait, Delay} ->
	    io:format("Waiting ~p ms~n", [Delay]),
	    {Result, State, Delay};
	{ping, Addr, NodeId} ->
	    Routes2 = dht_routes:pinged(Routes1, NodeId),
	    I = self(),
	    spawn_link(fun() ->
			       Status = case catch dht_port:ping(Port, Addr) of
					    {ok, NodeId} -> good;
					    _ -> bad
					end,
			       io:format("~s status=~p~n", [addr:to_s(Addr), Status]),
			       gen_server:cast(I, {mark, Addr, NodeId, Status})
		       end),
	    {Result, State#state{routes = Routes2}, ?MIN_TICK};
	{discover, NodeId, Addr1, NodeId1} ->
	    Routes2 = dht_routes:discovered(Routes1, NodeId1),
	    I = self(),
	    spawn_link(
	      fun() ->
		      case run_discovery(I, Addr1, NodeId, Port) of
			  ok ->
			      gen_server:cast(I, {mark, Addr1, NodeId1, good});
			  error ->
			      gen_server:cast(I, {mark, Addr1, NodeId1, bad})
		      end
	      end),
	    {Result, State#state{routes = Routes2}, ?MIN_TICK}
    end.


run_discovery(Pid, Addr1, NodeId, Port) ->
    case catch dht_port:find_node(Port, Addr1, NodeId) of
	{ok, [_ | _] = NodeIdsAddrs} ->
	    lists:foreach(fun({NodeId2, Addr2}) ->
				  gen_server:cast(Pid, {insert, Addr2, NodeId2})
			  end, NodeIdsAddrs),
	    ok;
	_ ->
	    error
    end.
