{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment
import Network.Socket (SockAddr(SockAddrInet))
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.IORef
import qualified Network.Libev as Ev
import qualified Data.ByteString.Lazy.Char8 as B8
import Foreign (nullFunPtr)
import Data.List (foldl')

import BEncoding (bdictLookup, BValue(BString, BDict, BList))
import qualified Node
import KRPC
import NodeId
import EventLog
import qualified MagnetGrep

type MyNode = ()
data Peer = Peer { peerAddr :: SockAddr,
                   peerLastSeen :: POSIXTime,
                   peerLastResponse :: Maybe POSIXTime,
                   peerLastQuery :: Maybe POSIXTime
                 }

data AppState = AppState { stMyNodes :: Map NodeId MyNode,
                           stPeers :: Map NodeId Peer
                         }

data AppContext = AppContext { ctxState :: IORef AppState,
                               ctxNode :: IORef Node.Node,
                               ctxEvLoop :: Ev.EvLoopPtr
                             }
type App a = ReaderT AppContext IO a

getState :: App AppState
getState = ctxState <$> ask >>=
           liftIO . readIORef

putState :: AppState -> App ()
putState app = do appRef <- ctxState <$> ask
                  liftIO $ writeIORef appRef app

setTimer :: Ev.EvTimestamp -> Ev.EvTimestamp -> App () -> App ()
setTimer delay repeat handler
    = do ctx <- ask
         let evLoop = ctxEvLoop ctx
             handler' = runReaderT handler ctx
         liftIO $ do evTimer <- Ev.mkEvTimer
                     evCbRef <- newIORef nullFunPtr
                     evCb <- Ev.mkTimerCallback $ \evLoop evTimer evType ->
                             do when (repeat < 1) $
                                     do -- stop first?
                                       Ev.freeEvTimer evTimer
                                       evCb <- readIORef evCbRef
                                       Ev.freeTimerCallback evCb
                                handler'
                     writeIORef evCbRef evCb 
                     Ev.evTimerInit evTimer evCb delay repeat
                     Ev.evTimerStart evLoop evTimer

setTimeout delay
    = setTimer delay 0
setInterval interval
    = setTimer interval interval

-- Model

nearestMyNode :: NodeId -> App (NodeId, MyNode)
nearestMyNode nodeId
    = do (less, mbEqual, greater) <- Map.splitLookup nodeId <$>
                                     stMyNodes <$>
                                     getState
         case mbEqual of
           Just myNode -> return (nodeId, myNode)
           Nothing ->
               do let less' = Map.toDescList less
                      greater' = Map.toAscList greater
                      ((myNodeId, myNode):_) = mergeByDistance nodeId less' greater'
                  return (myNodeId, myNode)

nearestPeers :: NodeId -> App [(NodeId, Peer)]
nearestPeers nodeId
    = do (less, mbEqual, greater) <- Map.splitLookup nodeId <$>
                                     stPeers <$>
                                     getState
         let r1 = case mbEqual of
                    Just peer -> [(nodeId, peer)]
                    Nothing -> []
             less' = Map.toDescList less
             greater' = Map.toAscList greater
             r2 = mergeByDistance nodeId less' greater'
         return $ r1 ++ r2

mergeByDistance :: NodeId
                -> [(NodeId, a)]
                -> [(NodeId, a)]
                -> [(NodeId, a)]
mergeByDistance _ [] ys = ys
mergeByDistance _ xs [] = xs
mergeByDistance nodeId (x@(xNodeId, _):xs) (y@(yNodeId, _):ys)
    | (xNodeId <-> nodeId) < (yNodeId <-> nodeId)
        = x : mergeByDistance nodeId xs (y:ys)
    | otherwise
        = y : mergeByDistance nodeId (x:xs) ys

alterPeer :: NodeId -> (Maybe Peer -> Maybe Peer) -> App ()
alterPeer nodeId alterFun
    = do app <- getState
         when (not $ Map.member nodeId $ stMyNodes app) $
              do let peers = stPeers app
                     peers' = Map.alter alterFun nodeId peers
                 peers' `seq`
                        putState $ app { stPeers = peers' }

seenPeer :: NodeId -> SockAddr -> App ()
seenPeer nodeId addr
    = do now <- liftIO getPOSIXTime
         let f Nothing = Just $
                         Peer { peerAddr = addr,
                                peerLastSeen = now,
                                peerLastResponse = Nothing,
                                peerLastQuery = Nothing
                              }
             f (Just peer) = Just $
                             peer { peerAddr = addr,
                                    peerLastSeen = now
                                  }
         alterPeer nodeId f

updatePeerResponse :: NodeId -> App ()
updatePeerResponse nodeId
    = do now <- liftIO $ getPOSIXTime
         let f Nothing = Nothing
             f (Just peer) = Just $
                             peer { peerLastResponse = Just now
                                  }
         alterPeer nodeId f

peerQueried nodeId
    = do now <- liftIO $ getPOSIXTime
         let f Nothing = Nothing
             f (Just peer) = Just $
                             peer { peerLastQuery = Just now
                                  }
         alterPeer nodeId f

-- Query handling

token = B8.pack "a"

onQuery addr (Ping nodeId)
    = do (myNodeId, _) <- nearestMyNode nodeId
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf $ myNodeId)]
onQuery addr (FindNode nodeId target)
    = do (myNodeId, _) <- nearestMyNode target
         nearest <- take 8 <$> nearestPeers target
         let nodes = encodeNodes $
                     map (\(nodeId1, peer1) ->
                              (nodeId1, peerAddr peer1)
                         ) nearest
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf myNodeId),
                       (BString $ B8.pack "nodes",
                        BString nodes)]
onQuery addr (GetPeers nodeId infoHash)
    = do (myNodeId, _) <- nearestMyNode infoHash
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf myNodeId),
                       (BString $ B8.pack "token",
                        BString token),
                       (BString $ B8.pack "values",
                        BList peerlist)]
    where peerlist = map (BString . encodeAddr) peers
          peers = concat $ do port <- [85, 87]
                              return [SockAddrInet port 3414387287]
onQuery addr (AnnouncePeer nodeId infoHash port token)
    = do (myNodeId, _) <- nearestMyNode infoHash
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf myNodeId)]
onQuery addr _
    = return $ Left $
      Error 204 $ B8.pack "Method Unknown"

-- Reply handling

onReply addr reply
    = do case reply `bdictLookup` "id" of
           Just (BString nodeIdBuf) ->
               do let nodeId = makeNodeId nodeIdBuf
                  updatePeerResponse nodeId
           _ ->
               return ()

         case reply `bdictLookup` "nodes" of
           Just (BString nodesBuf) ->
               do let nodes = decodeNodes nodesBuf
                  forM nodes $ \(nodeId, addr) ->
                      seenPeer nodeId addr
                  return ()
           _ ->
               return ()

-- Settling

settleTo :: NodeId -> App Int
settleTo target
    = do peers <- take 2 <$> nearestPeers target
         now <- liftIO getPOSIXTime
         let peerFilter (_, peer) =
                 case peerLastQuery peer of
                   Nothing -> True
                   Just lastQuery ->
                       lastQuery < now - 10
             peers' = filter peerFilter peers
             q = FindNode target target
         node <- ctxNode <$> ask >>=
                 liftIO . readIORef

         case null peers of
           -- We know some peers in the neighbourhood, let's try them
           -- filtered by lastQuery
           False ->
               do forM peers' $ \(nodeId, peer) ->
                      do peerQueried nodeId
                         liftIO $ Node.sendQueryNoWait (peerAddr peer) q node
                
                  return $ length peers'
           -- Need to bootstrap
           True ->
               liftIO $
               do addr : _ <- Node.getAddrs "router.bittorrent.com" "6881"
                  Node.sendQueryNoWait addr q node
                  return 1

settle :: NodeId -> App ()
settle nodeId =
    do t1 <- liftIO $ getPOSIXTime
       sent <- settleTo nodeId
       t2 <- liftIO $ getPOSIXTime
       when (sent > 0) $
            liftIO $ putStrLn $
                       "Sent " ++ show sent ++
                       " requests for " ++ show nodeId ++
                       " in " ++ show (t2 - t1)

settler :: [NodeId] -> App ()
settler []
    = do app <- getState
         settler $ Map.keys $ stMyNodes app
settler (nodeId:nodeIds)
    = do settle nodeId
         setTimeout 0.1 $ settler nodeIds

-- Compaction

purge :: App ()
purge = do now <- liftIO $ getPOSIXTime
           app <- getState
           let peers = stPeers app
               keep peer
                   = (peerLastSeen peer > now - 300) ||
                     (case peerLastResponse peer of
                        Just lastResponse -> lastResponse > now - 900
                        Nothing -> False) ||
                     (case peerLastQuery peer of
                        Just lastQuery -> lastQuery > now - 90
                        Nothing -> False)
               peers' = Map.filter keep peers
           peers' `seq`
                  putState $ app { stPeers = peers' }

purger = do t1 <- liftIO $ getPOSIXTime
            purge
            t2 <- liftIO $ getPOSIXTime
            liftIO $ putStrLn $ "Purged peers in " ++ show (t2 - t1)

            setTimeout 10 purger

-- Stats

stats :: App ()
stats = do app <- getState
           liftIO $ putStrLn $
                  "MyNodes: " ++ show (Map.size $ stMyNodes app) ++
                  "\tPeers: " ++ show (Map.size $ stPeers app)

statsLoop = setInterval 1 stats

-- Main

runSpoofer port myNodeIds
    = do log <- newLog 1.0 "spoofer.data"
         evFlags <- Ev.evRecommendedBackends
         evLoop <- Ev.evDefaultLoop Ev.evflag_auto
         node <- Node.new evLoop 10000

         let app = AppState { stMyNodes = Map.fromList $
                                          map (, ()) myNodeIds,
                              stPeers = Map.empty
                            }
         appRef <- newIORef app
         nodeRef <- newIORef node
         let ctx = AppContext { ctxState = appRef,
                                ctxNode = nodeRef,
                                ctxEvLoop = evLoop
                              }
             appCall :: App a -> IO a
             appCall f = runReaderT f ctx
             appCallback f a q = appCall $ f a q

         Node.setQueryHandler (appCallback onQuery) node
         Node.setReplyHandler (appCallback onReply) node

         appCall $ settler []
         appCall purger
         appCall statsLoop

         Ev.evLoop evLoop 0

main = do args <- getArgs
          myNodeIds <- concat <$>
                       forM args MagnetGrep.grep 
          runSpoofer 10000 myNodeIds
