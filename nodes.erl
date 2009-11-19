-module(nodes).

-export([init/0, seen/2, seen/3, get_nearest/1, get_nearest/2, neighbors_to_maintain/2]).

-record(node, {addr,
	       node_id,
	       last_seen = 0,
	       last_ping = 0,
	       repings = 0,
	       status = unsure}).

-define(MAX_NEAREST, 32).

init() ->
    mnesia:create_table(node, [{attributes, record_info(fields, node)}]).

seen(Addr, NodeId) ->
    seen(Addr, NodeId, good).
seen(Addr, <<NodeId/binary>>, NewStatus) ->
    {atomic, {OldStatus, NewStatus2}} =
	mnesia:transaction(
	  fun() ->
		  case mnesia:read(node, Addr) of
		      [#node{status = OldStatus} = Node] when NewStatus == bad ->
			  %% This is a special case, we've seen this
			  %% node before but now it has gone bad,
			  %% replace not so much here
			  mnesia:write(Node#node{status = bad}),
			  {OldStatus, bad};
		      [#node{status = OldStatus} = Node]
		      when NewStatus == unsure andalso
			   (OldStatus == good orelse OldStatus == bad) ->
			  mnesia:write(Node#node{node_id = NodeId,
						 status = OldStatus}),
			  {OldStatus, OldStatus};
		      [#node{status = OldStatus} = Node] ->
			  mnesia:write(Node#node{node_id = NodeId,
						 last_seen = util:mk_timestamp_ms(),
						 status = NewStatus}),
			  {OldStatus, NewStatus};
		      [] ->
			  mnesia:write(#node{addr = Addr,
					     node_id = NodeId,
					     last_seen = util:mk_timestamp_ms(),
					     status = NewStatus}),
			  {'NEW', NewStatus}
		  end
	  end),
    if
	OldStatus =/= NewStatus2 ->
	    io:format("seen ~p ~p->~p~n", [Addr, OldStatus, NewStatus2]);
	true ->
	    %%io:format("seen ~p stays ~p~n", [Addr, NewStatus2]),
	    ok
    end;
seen(_, _, _) ->
    ignored.

get_nearest(InfoHash) ->
    get_nearest(InfoHash, unsure).
get_nearest(InfoHash, MinStatus) ->
    {atomic, Nearest} =
	mnesia:transaction(
	  fun() ->
		  mnesia:foldl(
		    fun(#node{status = Status} = Node,
			Nearest1)
		       when Status == good orelse (Status == MinStatus) ->
			    Nearest2 = [Node | Nearest1],
			    Nearest3 = sort_by_distance(InfoHash, Nearest2),
			    cut(?MAX_NEAREST, Nearest3);
		       (_, Nearest1) ->
			    Nearest1
		    end, [], node)
	  end),
    [Addr
     || #node{addr = Addr} <- Nearest].

-define(SEEN_TIMEOUT, 60 * 1000).
-define(REPING_TIMEOUT, 30 * 1000).
-define(REPING_RETRIES, 5).

%% returns addrs to ping
neighbors_to_maintain(InfoHash, Amount) ->
    Now = util:mk_timestamp_ms(),
    {atomic, Addrs} =
	mnesia:transaction(
	  fun() ->
		  mnesia:write_lock_table(node),
		  ToPing =
		      mnesia:foldl(
			fun(#node{last_seen = LastSeen,
				  last_ping = LastPing,
				  repings = Repings} = Node, ToPing) ->
				if
				    LastSeen + ?SEEN_TIMEOUT > Now ->
					%% Not so long ago, we've seen this host
					ToPing;
				    LastPing + ?REPING_TIMEOUT =< Now
				    andalso Repings =< ?REPING_RETRIES ->
					[Node | ToPing];
				    true ->
					mnesia:delete_object(Node),
					ToPing
				end
			end, [], node),
		  ToPingCut = cut(Amount, sort_by_distance(InfoHash, ToPing)),
		  lists:foreach(fun(#node{repings = Repings} = Node) ->
					mnesia:write(node, Node#node{last_ping = Now,
								     repings = Repings + 1}, write)
				end, ToPingCut),
		  [{Addr, NodeId}
		   || #node{addr = Addr,
			    node_id = NodeId} <- ToPingCut]
	  end),
    Addrs.

sort_by_distance(InfoHash, Nodes) ->
    lists:sort(fun(#node{node_id = NodeId1},
		   #node{node_id = NodeId2}) ->
		       distance(InfoHash, NodeId1) =< distance(InfoHash, NodeId2)
	       end, Nodes).

distance(<<A:160/big-unsigned>>, <<B:160/big-unsigned>>) ->
    A bxor B;
distance(_, _) ->
    16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF.

cut(N, L) when length(L) =< N ->
    L;
cut(N, L) ->
    {L1, _} = lists:split(N, L),
    L1.
