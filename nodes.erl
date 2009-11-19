-module(nodes).

-export([init/0, seen/3, seen/4, get_nearest/1, get_nearest/2]).

-record(node, {ip_port,
	       node_id,
	       last_seen,
	       status = unsure}).

-define(MAX_NEAREST, 32).

init() ->
    mnesia:create_table(node, [{attributes, record_info(fields, node)}]).

seen(IP, Port, NodeId) ->
    seen(IP, Port, NodeId, good).
seen({_, _, _, _} = IP, Port, <<NodeId/binary>>, NewStatus) ->
    IpPort = {IP, Port},
    {atomic, {OldStatus, NewStatus2}} =
	mnesia:transaction(
	  fun() ->
		  case mnesia:read(node, IpPort) of
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
			  mnesia:write(#node{ip_port = IpPort,
					     node_id = NodeId,
					     last_seen = util:mk_timestamp_ms(),
					     status = NewStatus}),
			  {unsure, NewStatus}
		  end
	  end),
    if
	OldStatus =/= NewStatus2 ->
	    io:format("seen ~p:~p ~p->~p~n", [IP, Port, OldStatus, NewStatus2]);
	true ->
	    io:format("seen ~p:~p stays ~p~n", [IP, Port, NewStatus2]),
	    ok
    end;
seen(_, _, _, _) ->
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
			    Nearest3 =
				lists:sort(fun(#node{node_id = NodeId1},
					       #node{node_id = NodeId2}) ->
						   distance(InfoHash, NodeId1) =< distance(InfoHash, NodeId2)
					   end, Nearest2),
			    cut(?MAX_NEAREST, Nearest3);
		       (_, Nearest1) ->
			    Nearest1
		    end, [], node)
	  end),
    [IpPort
     || #node{ip_port = IpPort} <- Nearest].

distance(<<A:160/big-unsigned>>, <<B:160/big-unsigned>>) ->
    A bxor B;
distance(_, _) ->
    16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF.

cut(N, L) when length(L) =< N ->
    L;
cut(N, L) ->
    {L1, _} = lists:split(N, L),
    L1.
