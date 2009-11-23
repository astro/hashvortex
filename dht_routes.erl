-module(dht_routes).

-export([new/1, insert/3, mark/4, pinged/2, next_action/1]).

-record(routes, {node_id, buckets = []}).
-record(bucket, {order,
		 peers = [],
		 cached_peers = []
		}).
-record(peer, {addr,
	       node_id,
	       status = unsure,
	       last_ping = 0}).

-define(BUCKET_SIZE, 8).
-define(CACHE_MAX, 32).
-define(PING_INTERVAL, 15 * 60 * 1000).
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
			      KnowAlready = lists:any(fun(#peer{addr = Addr2,
								node_id = NodeId2}) ->
							      Addr1 == Addr2 andalso NodeId1 == NodeId2
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
    UpdateFun = fun(#peer{addr = Addr2,
			  node_id = NodeId2} = Peer)
		   when Addr1 == Addr2 andalso NodeId1 == NodeId2 ->
			Peer#peer{status = NewStatus};
		   (Peer) -> Peer
		end,
    bucket_op(Routes, NodeId1,
	      fun(#bucket{peers = Peers1,
			  cached_peers = CachedPeers1} = Bucket1) ->
		      Peers2 = lists:map(UpdateFun, Peers1),
		      CachedPeers2 = lists:map(UpdateFun, CachedPeers1),

		      maintain_bucket(Bucket1#bucket{peers = Peers2,
						     cached_peers = CachedPeers2})
	      end).

pinged(Routes, NodeId1) ->
    Now = util:mk_timestamp_ms(),
    UpdateFun = fun(#peer{node_id = NodeId2} = Peer)
		   when NodeId1 == NodeId2 ->
			Peer#peer{last_ping = Now};
		   (Peer) ->
			Peer
		end,
    bucket_op(Routes, NodeId1,
	      fun(#bucket{peers = Peers,
			  cached_peers = CachedPeers} = Bucket) ->
		      Bucket#bucket{peers = lists:map(UpdateFun, Peers),
				    cached_peers = lists:map(UpdateFun, CachedPeers)}
	      end).

-record(action, {time, action}).

%% returns {wait, NextInMS} | {ping, NodeId, Addr}
next_action(#routes{buckets = Buckets}) ->
    Actions = lists:map(fun next_action1/1, Buckets),
    Now = util:mk_timestamp_ms(),
    case soonest_action(Actions) of
	#action{time = infinity} ->
	    {wait, infinity};
	#action{time = Time, action = Action} when Time =< Now ->
	    Action;
	#action{time = Time} ->
	    {wait, Time - Now}
    end.

next_action1(#bucket{peers = Peers}) ->
    PeerActions = [#action{time = LastPing + ?PING_INTERVAL,
			   action = {ping, NodeId, Addr}}
		   || #peer{node_id = NodeId, addr = Addr,
			    last_ping = LastPing} <- Peers],
    CachedPeersActions = if
			     length(Peers) < ?BUCKET_SIZE ->
				 [#action{time = LastPing + ?PING_INTERVAL,
					  action = {ping, NodeId, Addr}}
				  || #peer{node_id = NodeId, addr = Addr,
					   last_ping = LastPing} <- Peers];
			     true -> []
			 end,
    soonest_action([soonest_action(PeerActions) | CachedPeersActions]).

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
		end, #action{time = infinity, action = nothing}, Actions).
			

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
