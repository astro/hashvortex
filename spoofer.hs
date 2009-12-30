module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as W8
import Network.Socket (SockAddr(SockAddrInet))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>), (><))
import Data.Foldable (toList, foldl)
import Prelude hiding (foldl)


import BEncoding (bdictLookup, BValue(BString))
import qualified Node
import KRPC
import NodeId
import BEncoding
import EventLog


data DigState = DigState { stNode :: Node.Node,
                           stTarget :: NodeId,
                           stFind :: Seq (NodeId, SockAddr),
                           stPing :: Seq (NodeId, SockAddr)
                         }

main = do log <- newLog "spoofer.log"
          node <- Node.new 9999
          nodeId <- makeRandomNodeId
          mDigState <- newMVar $ DigState node nodeId Seq.empty Seq.empty
          Node.setQueryHandler (queryHandler log) node
          Node.setReplyHandler (replyHandler log mDigState) node
          forkIO $ Node.run node

          -- Prepare bootstrap
          entryAddr:_ <- Node.getAddrs "router.bittorrent.com" "6881"

          -- Go!
          let loop = do modifyMVar_ mDigState $ \st ->
                            case (Seq.null $ stFind st, Seq.null $ stPing st) of
                              true ->
                                  do nodeId <- makeRandomNodeId
                                     return $ st { stTarget = nodeId,
                                                   stFind = stFind st |> (nodeId, entryAddr)
                                                 }
                              false -> return st
                        dig mDigState
                        loop
          loop

dig :: MVar DigState -> IO ()
dig mDigState = do modifyMVar_ mDigState $ \st ->
                       do let (stFind', stFind'') = Seq.splitAt 1 $ stFind st
                          case toList stFind' of
                            [(findNodeId, findAddr)] ->
                                do Node.sendQueryNoWait findAddr (FindNode findNodeId (stTarget st)) (stNode st)
                                   return $ st { stFind = stFind'',
                                                 stPing = stPing st |> (findNodeId, findAddr)
                                               }
                            [] ->
                                do let (stPing', stPing'') = Seq.splitAt 1 $ stPing st
                                   case toList stPing' of
                                     [(pingNodeId, pingAddr)] ->
                                         do Node.sendQueryNoWait pingAddr (Ping pingNodeId) (stNode st)
                                            return $ st { stFind = stFind'',
                                                          stPing = stPing'' }
                                     [] -> return st
                   threadDelay $ 1000000 `div` 5

replyHandler log mDigState addr reply
    = do putStrLn $ "reply: " ++ show reply
         let Just (BString nodeId') = reply `bdictLookup` "id"
             nodeId = makeNodeId nodeId'
             Just (BString nodes') = reply `bdictLookup` "nodes"
             nodes = decodeNodes nodes'
             isKnown st what = foldl (\t -> (t ||) . (what ==)) False (stFind st >< stPing st)
         modifyMVar_ mDigState $ \st ->
             foldM (\st what ->
                        case isKnown st what of
                          True -> return st
                          False -> do putStrLn $ "Adding " ++ show what
                                      return $ st { stFind = stFind st |> what }
                   ) st $ (nodeId, addr):nodes

queryHandler log addr query
    = do log $ show query
         return $ handleQuery query

handleQuery (Ping nodeId)
    = Right $ BDict [(BString $ B8.pack "id", BString $ nodeIdToBuf $ nodeId `nodeIdPlus` 1)]
handleQuery (FindNode nodeId target)
    = Right $ BDict [(BString $ B8.pack "id", BString $ nodeIdToBuf $ target `nodeIdPlus` 1),
                     (BString $ B8.pack "nodes", BString B8.empty)]
    where nodes = encodeNodes []
                  {- $
                  do offset <- [(-8)..(-1)]
                     return (addr, target `nodeIdPlus` offset)
          addr = SockAddrInet 9999 1461887825-}
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
