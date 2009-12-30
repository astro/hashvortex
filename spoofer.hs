module Main where

import Control.Concurrent
import Control.Concurrent.STM
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
import Data.Map (Map)
import qualified Data.Map as Map



import BEncoding (bdictLookup, BValue(BString))
import qualified Node
import KRPC
import NodeId
import BEncoding
import EventLog


data DigState = DigState { stNode :: Node.Node,
                           stTarget :: NodeId,
                           stFind :: Seq (NodeId, SockAddr),
                           stSeen :: Map SockAddr NodeId
                         }

main = do log <- newLog "spoofer.log"
          node <- Node.new 9999
          nodeId <- makeRandomNodeId
          tDigState <- atomically $ newTVar $ DigState node nodeId Seq.empty Map.empty
          Node.setQueryHandler (queryHandler log) node
          Node.setReplyHandler (replyHandler tDigState) node
          forkIO $ Node.run node
          forkIO $ statsLoop tDigState

          -- Prepare bootstrap
          entryAddr:_ <- Node.getAddrs "router.bittorrent.com" "6881"
          -- Go!
          let loop = do nodeId <- makeRandomNodeId
                        atomically $ do
                          st <- readTVar tDigState
                          case Seq.null (stFind st) && Map.size (stSeen st) /= 1 of
                            True -> writeTVar tDigState $
                                    st { stTarget = nodeId,
                                         stFind = Seq.singleton (nodeId, entryAddr),
                                         stSeen = Map.empty
                                       }
                            False -> return ()
                        dig tDigState
                        loop
          catch loop $ putStrLn . show

dig :: TVar DigState -> IO ()
dig tDigState = do next <- atomically $
                           do st <- readTVar tDigState
                              case Seq.null (stFind st) of
                                True -> return Nothing
                                _ ->
                                    let (stFind', stFind'') = Seq.splitAt 1 $ stFind st
                                    in case toList stFind' of
                                         [findDest] ->
                                             do writeTVar tDigState $ st { stFind = stFind'' }
                                                return $ Just (stNode st, stTarget st, findDest)
                                         _ -> return Nothing
                   case next of
                     Just (node, target, (findNodeId, findAddr)) ->
                         Node.sendQueryNoWait findAddr (FindNode (findNodeId `nodeIdPlus` 1) target) node
                     Nothing ->
                         return ()
                   threadDelay $ 1000000 `div` 50

statsLoop tDigState = do (findTarget,
                          findCnt,
                          seenCnt) <- atomically $ do
                           st <- readTVar tDigState
                           return (stTarget st,
                                   Seq.length $ stFind st,
                                   Map.size $ stSeen st)
                         putStrLn $ "Find " ++ show findTarget ++
                                  ": " ++ show seenCnt ++
                                  " seen, " ++ show findCnt ++
                                  " to query"
                         threadDelay 1000000
                         statsLoop tDigState

replyHandler tDigState addr reply
    = do let replyFields = do BString nodeId' <- reply `bdictLookup` "id"
                              let nodeId = makeNodeId nodeId'
                              BString nodes' <- reply `bdictLookup` "nodes"
                              let nodes = decodeNodes nodes'
                              return ((nodeId, addr), nodes)
         case replyFields of
           Just (replyer, nodes) ->
               do let isKnown st (_, addr) = Map.member addr $ stSeen st
                  atomically $ do
                            st <- readTVar tDigState
                            st <- foldM (\st node@(nodeId, addr) ->
                                             case isKnown st node of
                                               True -> return st
                                               False -> return $ st { stFind = stFind st |> node,
                                                                      stSeen = Map.insert addr nodeId $ stSeen st
                                                                    }
                                        ) st nodes
                            writeTVar tDigState st
           Nothing -> return ()

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


instance Ord SockAddr where
    compare (SockAddrInet port1 ip1) (SockAddrInet port2 ip2)
        | ip1 == ip2 = port1 `compare` port2
        | otherwise = ip1 `compare` ip2

