-module(dht_routes).

-export([new/1, insert/3, mark/4, pinged/2, discovered/2, next_action/1]).

-record(routes, {node_id, buckets = []}).
-record(bucket, {order,
		 peers = [],
		 cached_peers = []
		}).
-record(peer, {addr,
	       node_id,
	       status = unsure,
	       last_ping = 0,
	       last_discover = 0}).

-define(BUCKET_SIZE, 8).
-define(CACHE_MAX, 32).
-define(PING_INTERVAL, 15 * 60 * 1000).
-define(DISCOVER_INTERVAL, 5 * 60 * 1000).
-define(LONG_DELAY, 60 * 1000).  %% infinity is not easily comparable

%%====================================================================
%% API
%%====================================================================

new(NodeId) ->
    Buckets = [#bucket{order = Order}
	       || Order <- lists:seq(1, 160)],
    #routes{node_id = NodeId,
	   buckets = Buckets}.

insert(Routes, Addr1, NodeId1) ->
    bucket_op(Routes, NodeId1,
	      fun(#bucket{peers = Peers,
			  cached_peers = CachedPeers} = Bucket) ->
		      if
			  length(CachedPeers) < ?CACHE_MAX ->
			      KnowAlready = lists:any(fun(#peer{node_id = NodeId2}) ->
							      NodeId1 == NodeId2
						      end, Peers ++ CachedPeers),
			      case KnowAlready of
				  false ->
				      Bucket#bucket{cached_peers = [#peer{addr = Addr1,
									  node_id = NodeId1} | CachedPeers]};
				  true -> Bucket
			      end;
			  true -> Bucket
		      end
	      end).

mark(Routes, Addr1, NodeId1, NewStatus)
  when NewStatus == good; NewStatus == bad ->
    update_all_peers(Routes, NodeId1,
		     fun(#peer{addr = Addr2,
			       node_id = NodeId2} = Peer)
			when Addr1 == Addr2 andalso NodeId1 == NodeId2 ->
			     Peer#peer{status = NewStatus};
			(Peer) -> Peer
		     end, true).

%% sent, nor received neither marked good/bad already
pinged(Routes, NodeId1) ->
    Now = util:mk_timestamp_ms(),
    update_all_peers(Routes, NodeId1,
		     fun(#peer{node_id = NodeId2} = Peer)
			when NodeId1 == NodeId2 ->
			     Peer#peer{last_ping = Now};
			(Peer) ->
			     Peer
		     end).

%% sent, nor received neither marked good/bad already
discovered(Routes, NodeId1) ->
    Now = util:mk_timestamp_ms(),
    update_all_peers(Routes, NodeId1,
		     fun(#peer{node_id = NodeId2} = Peer)
			when NodeId1 == NodeId2 ->
			     Peer#peer{last_discover = Now};
			(Peer) ->
			     Peer
		     end).

-record(action, {time = infinity, action = nothing}).

%% returns {wait, NextInMS} | {ping, Addr, NodeId} | {discover, NodeId, Addr, NodeId}
next_action(#routes{node_id = NodeId, buckets = Buckets}) ->
    %%io:format("next_discovery: ~p~n",[next_discovery(NodeId, Buckets)]),
    Actions = [next_discovery(NodeId, Buckets) | lists:map(fun next_ping/1, Buckets)],
    Now = util:mk_timestamp_ms(),
    case soonest_action(Actions) of
	#action{time = infinity} ->
	    {wait, infinity};
	#action{time = Time, action = Action} when Time =< Now ->
	    Action;
	#action{time = Time} ->
	    {wait, Time - Now}
    end.

next_ping(#bucket{peers = Peers, cached_peers = CachedPeers}) ->
    PeerActions = [#action{time = LastPing + ?PING_INTERVAL,
			   action = {ping, Addr, NodeId}}
		   || #peer{node_id = NodeId, addr = Addr,
			    last_ping = LastPing} <- Peers],
    CachedPeersActions = if
			     length(Peers) < ?BUCKET_SIZE ->
				 [#action{time = LastPing + ?PING_INTERVAL,
					  action = {ping, Addr, NodeId}}
				  || #peer{node_id = NodeId, addr = Addr,
					   last_ping = LastPing} <- CachedPeers];
			     true -> []
			 end,
    soonest_action([soonest_action(PeerActions) | CachedPeersActions]).

%% done
next_discovery(_, []) ->
    #action{};
%% skip if there's no next bucket to look up in
next_discovery(NodeId, [#bucket{} | [#bucket{peers = []} = _ | _] = Buckets]) ->
    next_discovery(NodeId, Buckets);
next_discovery(NodeId, [#bucket{peers = Peers,
				cached_peers = CachedPeers}
			| [#bucket{peers = [_ | _] = NextPeers} = _ | _] = Buckets])
  when length(Peers) < ?BUCKET_SIZE andalso length(CachedPeers) < ?BUCKET_SIZE ->
    soonest_action([next_discovery(NodeId, Buckets)
		    | [#action{time = LastDiscover1 + ?DISCOVER_INTERVAL,
			       action = {discover, NodeId, Addr1, NodeId1}}
		       || #peer{last_discover = LastDiscover1,
				node_id = NodeId1,
				addr = Addr1} <- NextPeers]
		   ]);
next_discovery(NodeId, [#bucket{} | Buckets]) ->
    next_discovery(NodeId, Buckets).
    

soonest_action(Actions) ->
    lists:foldl(fun(#action{time = infinity}, Action) ->
			Action;
		   (Action, #action{time = infinity}) ->
			Action;
		   (#action{time = Time1} = Action, #action{time = Time2})
		   when Time1 < Time2 ->
			Action;
		   (#action{}, #action{} = Action) ->
			Action
		end, #action{}, Actions).
			

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

bucket_op(#routes{node_id = NodeId,
		  buckets = Buckets1} = Routes, NodeId1, BucketFun) ->
    Order = distance_order(NodeId, NodeId1),
    Bucket1 = case lists:keysearch(Order, #bucket.order, Buckets1) of
		  {value, #bucket{} = B} -> B;
		  false -> #bucket{order = Order}
	      end,
    Bucket2 = BucketFun(Bucket1),
    Buckets2 = lists:keystore(Order, #bucket.order, Buckets1, Bucket2),
    Routes#routes{buckets = lists:sort(Buckets2)}.

update_all_peers(Routes, NodeId1, PeerFun) ->
    update_all_peers(Routes, NodeId1, PeerFun, false).
update_all_peers(Routes, NodeId1, PeerFun, Maintain) ->
    bucket_op(Routes, NodeId1,
	      fun(#bucket{peers = Peers,
			  cached_peers = CachedPeers} = Bucket1) ->
		      Bucket2 = Bucket1#bucket{peers = lists:map(PeerFun, Peers),
					       cached_peers = lists:map(PeerFun, CachedPeers)},
		      case Maintain of
			  true -> maintain_bucket(Bucket2);
			  false -> Bucket2
		      end
	      end).


maintain_bucket(#bucket{peers = Peers1,
			cached_peers = CachedPeers1} = Bucket) ->
    %% move not-good peers to cached_peers
    {Peers2, NewCachedPeers} =
	lists:foldl(fun(#peer{status = good} = Peer, {StillGood, NotGood}) ->
			    {[Peer | StillGood], NotGood};
		       (Peer, {StillGood, NotGood}) ->
			    {StillGood, [Peer | NotGood]}
		    end, {[], []}, Peers1),
    %% move good peers to peers
    CachedPeers2 = NewCachedPeers ++ CachedPeers1,
    {Peers3, CachedPeers3} =
	lists:foldl(fun(#peer{status = good} = Peer, {Peers, CachedPeers})
		       when length(Peers) < ?BUCKET_SIZE ->
			    {[Peer | Peers], CachedPeers};
		       (#peer{status = bad}, R) ->
			    R;
		       (Peer, {Peers, CachedPeers}) ->
			    {Peers, [Peer | CachedPeers]}
		    end, {Peers2, []}, CachedPeers2),

    Bucket#bucket{peers = Peers3,
		  cached_peers = CachedPeers3}.

%% sort_by_distance(InfoHash, Nodes) ->
%%     lists:sort(fun(#node{node_id = NodeId1},
%% 		   #node{node_id = NodeId2}) ->
%% 		       distance(InfoHash, NodeId1) =< distance(InfoHash, NodeId2)
%% 	       end, Nodes).

distance(<<A:160/big-unsigned>>, <<B:160/big-unsigned>>) ->
    A bxor B;
distance(_, _) ->
    16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF.

distance_order(A, B) ->
    Distance = distance(A, B),
    distance_order1(Distance, 0).
distance_order1(0, Order) -> Order;
distance_order1(Distance, Order) ->
    distance_order1(Distance bsr 1, Order + 1).
