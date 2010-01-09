module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
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
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)


import BEncoding (bdictLookup, BValue(BString, BDict, BList))
import qualified Node
import KRPC
import NodeId
import EventLog


data DigState = DigState { stNode :: Node.Node,
                           stTarget :: NodeId,
                           stFind :: Seq (NodeId, SockAddr),
                           stSeen :: Map SockAddr NodeId,
                           stCache :: Seq (NodeId, SockAddr),
                           stLastReset :: POSIXTime
                         }

maxCacheSize = 16
minResetInterval = 2.0
packetRate = 200
idleSleep = 1.0

main = do log <- newLog "spoofer.data"
          node <- Node.new 9999
          nodeId <- makeRandomNodeId
          now <- getPOSIXTime
          entryAddr:_ <- Node.getAddrs "router.bittorrent.com" "6881"
          let entryCache = Seq.singleton (nodeId, entryAddr)
          tDigState <- atomically $ newTVar $ DigState node nodeId Seq.empty Map.empty entryCache now
          Node.setQueryHandler (queryHandler log tDigState) node
          Node.setReplyHandler (replyHandler tDigState) node
          forkIO $ Node.run node
          forkIO $ statsLoop tDigState

          let loop = do now <- getPOSIXTime
                        let shouldReset = do st <- readTVar tDigState
                                             return $
                                                    stLastReset st + minResetInterval <= now &&
                                                    Seq.null (stFind st) &&
                                                    not (Seq.null $ stCache st) &&
                                                    Map.size (stSeen st) /= 1
                        shouldReset' <- atomically shouldReset
                        when shouldReset' $
                             do target <- makeRandomNodeId
                                atomically $
                                           do shouldReset' <- shouldReset
                                              when shouldReset' $
                                                   do st <- readTVar tDigState
                                                      writeTVar tDigState $
                                                                st { stTarget = target,
                                                                     stFind = stCache st,
                                                                     stSeen = Map.empty,
                                                                     stCache = Seq.empty,
                                                                     stLastReset = now
                                                                   }
                        dig tDigState
                        loop
          catch loop print

dig :: TVar DigState -> IO ()
dig tDigState = do next <- atomically $
                           readTVar tDigState >>= \st ->
                           if Seq.null $ stFind st
                           then return Nothing
                           else let (stFind', stFind'') = Seq.splitAt 1 $ stFind st
                                in case toList stFind' of
                                     [findDest] ->
                                         do writeTVar tDigState $ st { stFind = stFind'' }
                                            return $ Just (stNode st, stTarget st, findDest)
                                     _ -> return Nothing
                   case next of
                     Just (node, target, (findNodeId, findAddr)) ->
                         do Node.sendQueryNoWait findAddr (FindNode (findNodeId `nodeIdPlus` 1) target) node
                            threadDelay $ 1000000 `div` packetRate
                     Nothing ->
                         do threadDelay $ truncate $ idleSleep * 1000000
                            return ()

statsLoop tDigState = do (findTarget,
                          findCnt,
                          seenCnt,
                          cacheCnt) <- atomically $ do
                           st <- readTVar tDigState
                           return (stTarget st,
                                   Seq.length $ stFind st,
                                   Map.size $ stSeen st,
                                   Seq.length $ stCache st)
                         putStrLn $ "Find " ++ show findTarget ++
                                  ": " ++ show seenCnt ++
                                  " seen, " ++ show findCnt ++
                                  " to query, " ++ show cacheCnt ++
                                  " cached"
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
                                             if isKnown st node
                                             then return st
                                             else return $ st { stFind = stFind st |> node,
                                                                stSeen = Map.insert addr nodeId $ stSeen st,
                                                                stCache = if Seq.length (stCache st) < maxCacheSize
                                                                          then stCache st |> node
                                                                          else stCache st
                                                              }
                                        ) st nodes
                            writeTVar tDigState st
           Nothing -> return ()

queryHandler log tDigState addr query
    = do log query
         atomically $ do
           st <- readTVar tDigState
           when (Seq.length (stCache st) < maxCacheSize) $
                writeTVar tDigState $ st { stCache = stCache st |> node }
         return $ handleQuery query
    where node = (nodeId, addr)
          nodeId = case query of
                     Ping id -> id
                     FindNode id _ -> id
                     GetPeers id _ -> id
                     AnnouncePeer id _ _ _ -> id

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
          peers = concat $ do port <- [80]
                              return [SockAddrInet port 3414387287]
handleQuery (AnnouncePeer nodeId infoHash port token)
    = Right $ BDict [(BString $ B8.pack "id", BString $ nodeIdToBuf $ infoHash `nodeIdPlus` 1)]
handleQuery _
    = Left $ Error 204 $ B8.pack "Method Unknown"


instance Ord SockAddr where
    compare (SockAddrInet port1 ip1) (SockAddrInet port2 ip2)
        | ip1 == ip2 = port1 `compare` port2
        | otherwise = ip1 `compare` ip2

