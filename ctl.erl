-module(ctl).

-export([dir_torrents/1, look_nodes_from_dir/2]).


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

    
