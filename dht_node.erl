-module(dht_node).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, hint/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {node_id, port, routes}).

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
    gen_server:call(Pid, {hint, Addr}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([NodeId, UdpPort]) ->
    I = self(),
    QuestionCB = fun(Addr, T, Q, A) ->
			 gen_server:call(I, {question, Addr, T, Q, A})
		 end,
    {ok, DhtPort} = dht_port:start_link(NodeId, UdpPort, QuestionCB),
    {ok, #state{port = DhtPort,
		node_id = NodeId,
		routes = dht_routes:new(NodeId)}}.

handle_call({hint, Addr}, From, #state{port = Port} = State) ->
    I = self(),
    spawn_link(fun() ->
		       case catch dht_port:ping(Port, Addr) of
			   {ok, NodeId} ->
			       gen_server:reply(From, ok),
			       gen_server:cast(I, {hinted, Addr, NodeId});
			   error ->
			       gen_server:reply(From, error)
		       end
	       end),
    next_state(noreply, State).

handle_cast({mark, Addr, NodeId, Status}, #state{routes = Routes1} = State) ->
    Routes2 = dht_routes:mark(Routes1, Addr, NodeId, Status),
    next_state(noreply, State#state{routes = Routes2});

handle_cast({hinted, Addr, NodeId}, #state{routes = Routes1} = State) ->
    Routes2 = dht_routes:insert(Routes1, Addr, NodeId),
    Routes3 = dht_routes:mark(Routes2, Addr, NodeId, good),
    next_state(noreply, State#state{routes = Routes3}).   

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
    case dht_routes:next_action(Routes1) of
	{wait, Delay} ->
	    io:format("Waiting ~p ms~n", [Delay]),
	    {Result, State, Delay};
	{ping, NodeId, Addr} ->
	    I = self(),
	    spawn_link(fun() ->
			       Status = case catch dht_port:ping(Port, Addr) of
					    {ok, NodeId} -> good;
					    _ -> bad
					end,
			       gen_server:cast(I, {mark, Addr, NodeId, Status})
		       end),
	    Routes2 = dht_routes:pinged(Routes1, NodeId),
	    next_state(Result, State#state{routes = Routes2})
    end.
