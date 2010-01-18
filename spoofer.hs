{-# OPTIONS_GHC -XNoBangPatterns #-}
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

data Peer = Peer {-# UNPACK #-} !NodeId !SockAddr

data DigState = DigState { -- |Network interface
                           stNode :: Node.Node,
                           stFindTarget :: NodeId,
                           -- |To find by distance to stFindTarget
                           stFind :: Map Integer Peer,
                           -- |Limit iteration length
                           stFindCount :: Int,
                           -- |Don't query a node twice
                           stSeen :: Map SockAddr (),
                           -- |Start cache for next iteration
                           stCache :: Seq Peer
                         }

maxFindCount = 256
maxCacheSize = 16
packetRate = 20
idleSleep = 0.1

main = do log <- newLog "spoofer.data"
          node <- Node.new 9999
          nodeId <- makeRandomNodeId
          now <- getPOSIXTime
          entryAddrs <- Node.getAddrs "router.bittorrent.com" "6881"
          let entryCache = Seq.fromList [Peer nodeId entryAddr
                                         | entryAddr <- entryAddrs]
          tDigState <- atomically $ newTVar $
                       DigState { stNode = node,
                                  stFindTarget = nodeId,
                                  stFind = Map.empty,
                                  stFindCount = 0,
                                  stSeen = Map.empty,
                                  stCache = entryCache
                                }
          Node.setQueryHandler (queryHandler log tDigState) node
          Node.setReplyHandler (replyHandler tDigState) node
          forkIO $ Node.run node
          forkIO $ statsLoop tDigState

          let loop = do dig tDigState
                        loop
          catch loop print

resetDig :: TVar DigState -> IO (STM ())
resetDig tDigState
    = do nodeId <- makeRandomNodeId
         return $
                do st <- readTVar tDigState
                   writeTVar tDigState $
                             st { stFindTarget = nodeId,
                                  stFind = Map.empty,
                                  stFindCount = 0,
                                  stSeen = Map.empty,
                                  stCache = Seq.empty
                                }
                   mapM_ (insertCache tDigState) $ toList (stCache st)

dig :: TVar DigState -> IO ()
dig tDigState = do resetter <- resetDig tDigState
                   let getNext =
                           do st <- readTVar tDigState
                              case (Map.null $ stFind st,
                                    Seq.null $ stCache st,
                                    Map.findMin $ stFind st) of
                                (_, False, _) | stFindCount st >= maxFindCount ->
                                      do resetter
                                         getNext
                                (True, False, _) ->
                                      do resetter
                                         getNext
                                (False, _, (_distance, peer)) ->
                                    do writeTVar tDigState $
                                                 st { stFind = Map.deleteMin $ stFind st,
                                                      stFindCount = stFindCount st + 1
                                                    }
                                       return $ Just (stNode st, stFindTarget st, peer)
                                _ ->
                                    return Nothing
                   next <- atomically getNext
                   case next of
                     Just (node, target, Peer findNodeId findAddr) ->
                         do Node.sendQueryNoWait findAddr (FindNode target target) node
                            threadDelay $ 1000000 `div` packetRate
                     Nothing ->
                         do threadDelay $ truncate $ idleSleep * 1000000
                            return ()

statsLoop tDigState = do (findTarget,
                          findLen,
                          findCount,
                          seenLen,
                          cacheLen) <- atomically $ do
                           st <- readTVar tDigState
                           return (stFindTarget st,
                                   Map.size $ stFind st,
                                   stFindCount st,
                                   Map.size $ stSeen st,
                                   Seq.length $ stCache st)
                         putStrLn $ "Find " ++ show findTarget ++
                                  ": " ++ show seenLen ++
                                  " seen, " ++ show findCount ++
                                  " queried, " ++ show findLen ++
                                  " to query, " ++ show cacheLen ++
                                  " cached"
                         threadDelay 1000000
                         statsLoop tDigState

replyHandler tDigState addr reply
    = do let replyFields = do BString nodeId' <- reply `bdictLookup` "id"
                              let nodeId = makeNodeId nodeId'
                              BString nodes' <- reply `bdictLookup` "nodes"
                              let nodes = decodeNodes nodes'
                              return (Peer nodeId addr, nodes)
         case replyFields of
           Just (replyer, nodes) ->
               atomically $
               forM_ nodes $ \(nodeId, addr) ->
                   do let peer = Peer nodeId addr
                      known <- isPeerKnown tDigState peer
                      unless known $
                             do insertCache tDigState peer
                                insertFindPeer tDigState peer
           Nothing -> return ()

isPeerKnown :: TVar DigState -> Peer -> STM Bool
isPeerKnown tDigState peer@(Peer _ addr)
    = do st <- readTVar tDigState
         return $ Map.member addr $ stSeen st

insertFindPeer :: TVar DigState -> Peer -> STM ()
insertFindPeer tDigState peer@(Peer nodeId addr)
    = do st <- readTVar tDigState
         let target = stFindTarget st
             distance = target <-> nodeId
             st' = st { stFind = Map.insert distance peer $ stFind st,
                        stSeen = Map.insert addr () $ stSeen st
                      }
         writeTVar tDigState st'

insertCache tDigState peer
    = do st <- readTVar tDigState
         when (Seq.length (stCache st) < maxCacheSize) $
              writeTVar tDigState $ st { stCache = stCache st |> peer }

queryHandler log tDigState addr query
    = do log query
         atomically $
                    do known <- isPeerKnown tDigState peer
                       unless known $
                              insertCache tDigState peer
         return $ handleQuery query
    where peer = Peer nodeId addr
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

