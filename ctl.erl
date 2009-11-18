-module(ctl).

-export([dir_torrents/1, look_nodes_from_dir/2, care_about/3, reping/2]).


dir_torrents(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    lists:map(fun(File) ->
		      Dir ++ "/" ++ File
	      end,
	      lists:filter(fun(File) ->
				   string:str(File, ".torrent") =/= 0
			   end,
			   Files)
	     ).

look_nodes_from_dir(NodePid, Dir) ->
    InfoHashes = [InfoHash
		  || InfoHash <- [catch benc:info_hash(benc:parse_file(File))
				  || File <- dir_torrents(Dir)],
		     is_binary(InfoHash)],
    lists:foreach(
      fun(InfoHash) ->
	      spawn_link(
		fun() ->
			case catch dht_node:find_node(NodePid, InfoHash) of
			    {'EXIT', Reason} ->
				io:format("Crash for ~p: ~p~n", [InfoHash, Reason]);
			    _ ->
				io:format("Finished for ~p~n", [InfoHash])
			end
		end)
      end, InfoHashes).

care_about(InfoHash, NodeCount, PortBase) ->
    <<InfoHashNum:160/big-unsigned>> = InfoHash,
    Nodes = [Node1 | _] =
	lists:map(
	  fun(N) ->
		  NodeId = <<(InfoHashNum - (NodeCount div 2) + N):160/big-unsigned>>,
		  {ok, Pid} = dht_node:start_link(NodeId, PortBase + N),
		  Pid
	  end,
	  lists:seq(0, NodeCount - 1)),
    dht_node:ping(Node1, "router.bittorrent.com", 6881),
    reping(Nodes, InfoHash),
    Nodes.

reping([Node1 | _] = Nodes, InfoHash) ->			  
    Neighbors = dht_node:find_node(Node1, InfoHash),
    util:pmap(fun({NeighborIP, NeighborPort}) ->
		       util:pmap(fun(Node) ->
					  catch dht_node:ping(Node, NeighborIP, NeighborPort)
				  end, Nodes)
	       end, Neighbors).

			  
