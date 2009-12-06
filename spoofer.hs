module Main where

import Control.Concurrent
import Control.Monad

import BEncoding (bdictLookup, BValue(BString))
import qualified Node
import KRPC
import NodeId

main = do node <- Node.new 9999
          forkIO $ Node.run node
          _:a:_ <- Node.getAddrs "router.bittorrent.com" "6881"
          nodeId <- makeRandomNodeId
          findReply <- Node.sendQuery a (FindNode nodeId nodeId) node
          putStrLn $ "received: " ++ show findReply
          case findReply of
               RPacket _ reply ->
                   let me = liftM (decodeNodes . \(BString s) -> s) $
                            reply `bdictLookup` "nodes"
                   in case me of
                        Just nodes -> forM_ nodes $ \(addr, id) ->
                                      forkIO $
                                      do putStrLn $ "Node: " ++ show addr ++ " " ++ show id
                                         Node.sendQuery addr (FindNode nodeId nodeId) node
                                         putStrLn $ "Node " ++ show addr ++ " replied"
                        Nothing -> putStrLn "Got no nodes. :("
          yield
          threadWaitRead $ fromIntegral 0
