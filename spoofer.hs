module Main where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as B8
import Network.Socket (SockAddr)


import BEncoding (bdictLookup, BValue(BString))
import qualified Node
import KRPC
import NodeId
import BEncoding
import Peers

main = do peers <- newPeers
          node <- Node.new 9999
          Node.setQueryHandler (handleQuery peers) node
          forkIO $ Node.run node

          -- Prepare bootstrap
          a:_ <- Node.getAddrs "router.bittorrent.com" "6881"
          nodeId <- makeRandomNodeId
          modifyMVar_ peers $ return . ((Peer a nodeId Nothing):)
          
          -- Go!
          settle nodeId node peers

handleQuery peers addr (Ping nodeId)
    = do updatePeer peers $ Peer { peerAddr = addr,
                                   peerId = nodeId,
                                   peerLastFindNode = Nothing
                                 }
         return $ Right $ BDict [(BString $ B8.pack "id", BString $ nodeIdToBuf $ nodeId `nodeIdPlus` 1)]
handleQuery peers addr (FindNode nodeId _)
    = do updatePeer peers $ Peer { peerAddr = addr,
                                   peerId = nodeId,
                                   peerLastFindNode = Nothing
                                 }
         return $ Right $ BDict [(BString $ B8.pack "id", BString $ nodeIdToBuf $ nodeId `nodeIdPlus` 1),
                              (BString $ B8.pack "nodes", BString B8.empty)]
handleQuery peers addr _
    = return $ Left $ Error 204 $ B8.pack "Method Unknown"

settle :: NodeId -> Node.Node -> Peers -> IO ()
settle nodeId node peers = do mNextNode <- nextPeer peers
                              maybe (return ()) (goFindNode node peers) mNextNode
                              threadDelay $ 500 * 1000 * 1000
                              settle nodeId node peers


goFindNode node peers peer = do now <- getPOSIXTime
                                updatePeer peers  peer { peerLastFindNode = Just now }
                                forkIO_ $
                                  (findNode node (peerAddr peer) (peerId peer `nodeIdPlus` 1) >>=
                                   mapM_ (updatePeer peers)
                                  )
    where forkIO_ f = forkIO f >> return ()


findNode :: Node.Node -> SockAddr -> NodeId -> IO [Peer]
findNode node addr nodeId
    = do now <- getPOSIXTime
         findReply <- Node.sendQuery addr (FindNode nodeId nodeId) node
         case findReply of
               RPacket _ reply ->
                   let mResponder = maybe [] (:[]) $
                                    (\id ->
                                         Peer addr nodeId $ Just now
                                    ) `liftM` (reply `bdictLookup` "id")
                       mNodes = fromMaybe [] $
                                liftM (map (\(addr, nodeId) ->
                                            Peer addr nodeId $ Nothing
                                           ) . decodeNodes . \(BString s) -> s) $
                                reply `bdictLookup` "nodes"
                   in return $ {-mResponder ++-} mNodes
