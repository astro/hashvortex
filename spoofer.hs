module Main where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as W8
import Network.Socket (SockAddr(SockAddrInet))


import BEncoding (bdictLookup, BValue(BString))
import qualified Node
import KRPC
import NodeId
import BEncoding
import Peers
import EventLog


main = do log <- newLog "spoofer.log"
          peers <- newPeers
          node <- Node.new 9999
          Node.setQueryHandler (queryHandler peers log) node
          forkIO $ Node.run node

          -- Prepare bootstrap
          a:_ <- Node.getAddrs "router.bittorrent.com" "6881"
          nodeId <- makeRandomNodeId
          updatePeer peers $ Peer a nodeId Nothing

          -- Go!
          settle node peers
          

queryHandler peers log addr query
    = do log $ show query
         {- updatePeer peers $ Peer { peerAddr = addr,
                                   peerId = nodeId,
                                   peerLastFindNode = Nothing
                                 } -}
         return $ handleQuery query

handleQuery (Ping nodeId)
    = Right $ BDict [(BString $ B8.pack "id", BString $ nodeIdToBuf $ nodeId `nodeIdPlus` 1)]
handleQuery (FindNode nodeId target)
    = Right $ BDict [(BString $ B8.pack "id", BString $ nodeIdToBuf $ target `nodeIdPlus` 1),
                     (BString $ B8.pack "nodes", BString B8.empty)]
    where nodes = encodeNodes $
                  do offset <- [(-8)..(-1)]
                     return (addr, target `nodeIdPlus` offset)
          addr = SockAddrInet 9999 1461887825
handleQuery (GetPeers nodeId infoHash)
    = Right $ BDict [(BString $ B8.pack "id", BString $ nodeIdToBuf $ infoHash `nodeIdPlus` 1),
                     (BString $ B8.pack "token", BString B8.empty),
                     (BString $ B8.pack "values", BList [])]
    where peerlist = map (BString . encodeAddr) peers
          peers = concat $ do port <- [21, 22, 80, 139]
                              return [SockAddrInet port 797090641,
                                      SockAddrInet port 847422289,
                                      SockAddrInet port 1719837521,
                                      SockAddrInet port 495100753,
                                      SockAddrInet port 511877969,
                                      SockAddrInet port 713204561,
                                      SockAddrInet port 444769105,
                                      SockAddrInet port 377660241,
                                      SockAddrInet port 746758993,
                                      SockAddrInet port 489202513,
                                      SockAddrInet port 813867857,
                                      SockAddrInet port 2189599569,
                                      SockAddrInet port 2676138833,
                                      SockAddrInet port 1082303313,
                                      SockAddrInet port 948085585,
                                      SockAddrInet port 1115857745,
                                      SockAddrInet port 461546321,
                                      SockAddrInet port 478323537,
                                      SockAddrInet port 293774161,
                                      SockAddrInet port 243442513,
                                      SockAddrInet port 394437457,
                                      SockAddrInet port 310551377,
                                      SockAddrInet port 473473873,
                                      SockAddrInet port 142779217,
                                      SockAddrInet port 2256708433,
                                      SockAddrInet port 1283629905]
handleQuery (AnnouncePeer nodeId infoHash port token)
    = Right $ BDict [(BString $ B8.pack "id", BString $ nodeIdToBuf $ infoHash `nodeIdPlus` 1)]
handleQuery _
    = Left $ Error 204 $ B8.pack "Method Unknown"

settle :: Node.Node -> Peers -> IO ()
settle node peers
        = do mNextNode <- nextPeer peers
             maybe (return ()) (goFindNode node peers) mNextNode
             threadDelay $ 500 * 1000
             settle node peers


goFindNode node peers peer
    = do now <- getPOSIXTime
         updatePeer peers  peer { peerLastFindNode = Just now }
         nodeId <- makeRandomNodeId
         forkIO_ (findNode node (peerAddr peer) (peerId peer) nodeId >>=
                  mapM_ (updatePeer peers)
                 )
    where forkIO_ f = forkIO f >> return ()


findNode :: Node.Node -> SockAddr -> NodeId -> NodeId -> IO [Peer]
findNode node addr nodeId target
    = do now <- getPOSIXTime
         findReply <- Node.sendQuery addr (FindNode nodeId target) node
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
