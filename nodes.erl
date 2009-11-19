-module(nodes).

-export([init/0, seen/3, seen/4, get_nearest/1]).

-record(node, {ip_port,
	       node_id,
	       last_seen,
	       state = unknown}).

-define(MAX_NEAREST, 32).

init() ->
    mnesia:create_table(node, [{attributes, record_info(fields, node)}]).

seen(IP, Port, NodeId) ->
    seen(IP, Port, NodeId, good).
seen({_, _, _, _} = IP, Port, <<NodeId:20/binary>>, NewState) ->
    io:format("seen ~p:~p ~p~n", [IP, Port, NewState]),
    IpPort = {IP, Port},
    {atomic, _} =
	mnesia:transaction(
	  fun() ->
		  case mnesia:read(node, IpPort) of
		      [#node{} = Node] when NewState == bad ->
			  %% This is a special case, we've seen this
			  %% node before but now it has gone bad,
			  %% replace not so much here
			  mnesia:write(Node#node{state = bad});
		      [#node{} = Node] ->
			  mnesia:write(Node#node{node_id = NodeId,
						 last_seen = util:mk_timestamp_ms(),
						 state = NewState});
		      [] ->
			  mnesia:write(#node{ip_port = IpPort,
					     node_id = NodeId,
					     last_seen = util:mk_timestamp_ms(),
					     state = NewState})
		  end
	  end);
seen(_, _, _, _) ->
    ignored.

get_nearest(InfoHash) ->
    {atomic, Nearest} =
	mnesia:transaction(
	  fun() ->
		  mnesia:foldl(
		    fun(#node{state = bad}, Nearest1) ->
			    Nearest1;
		       (Node, Nearest1) ->
			    Nearest2 = [Node | Nearest1],
			    Nearest3 =
				lists:sort(fun(#node{node_id = NodeId1},
					       #node{node_id = NodeId2}) ->
						   distance(InfoHash, NodeId1) =< distance(InfoHash, NodeId2)
					   end, Nearest2),
			    cut(?MAX_NEAREST, Nearest3)
		    end, [], node)
	  end),
    [IpPort
     || #node{ip_port = IpPort} <- Nearest].

distance(<<A:160/big-unsigned>>, <<B:160/big-unsigned>>) ->
    A bxor B.

cut(N, L) when length(L) =< N ->
    L;
cut(N, L) ->
    {L1, _} = lists:split(N, L),
    L1.
