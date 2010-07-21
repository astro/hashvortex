{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment
import Network.Socket (SockAddr(SockAddrInet))
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, ViewL((:<), EmptyL))
import qualified Data.Sequence as Seq
import Data.Time.Clock.POSIX (getPOSIXTime)
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
type Time = Ev.EvTimestamp
data Peer = Peer { peerAddr :: SockAddr,
                   peerLastSeen :: Time,
                   peerLastResponse :: Maybe Time,
                   peerLastQuery :: Maybe Time
                 }

data AppState = AppState { stMyNodes :: Map NodeId MyNode,
                           stPeers :: Map NodeId Peer,
                           stSettleQueue :: Seq NodeId
                         }

data AppContext = AppContext { ctxState :: IORef AppState,
                               ctxNode :: IORef Node.Node,
                               ctxTargets :: BValue,
                               ctxEvLoop :: Ev.EvLoopPtr,
                               ctxLogger :: Logger
                             }
type App a = ReaderT AppContext IO a

getState :: App AppState
getState = ctxState <$> ask >>=
           liftIO . readIORef

putState :: AppState -> App ()
putState app = do appRef <- ctxState <$> ask
                  liftIO $ writeIORef appRef app

now :: App Ev.EvTimestamp
now = ctxEvLoop <$> ask >>=
      (liftIO . Ev.evNow)

setTimer :: Ev.EvTimestamp -> Ev.EvTimestamp -> App () -> App ()
setTimer delay repeat handler
    = do ctx <- ask
         let evLoop = ctxEvLoop ctx
             handler' = runReaderT handler ctx
         liftIO $ do evTimer <- Ev.mkEvTimer
                     evCbRef <- newIORef nullFunPtr
                     evCb <- Ev.mkTimerCallback $ \evLoop evTimer evType ->
                             do when (repeat <= 0) $
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
    = do now <- now
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
    = do now <- now
         let f Nothing = Nothing
             f (Just peer) = Just $
                             peer { peerLastResponse = Just now
                                  }
         alterPeer nodeId f

peerQueried nodeId
    = do now <- now
         let f Nothing = Nothing
             f (Just peer) = Just $
                             peer { peerLastQuery = Just now
                                  }
         alterPeer nodeId f

-- Query handling

token = B8.pack "a"

onQuery' (Ping nodeId)
    = do (myNodeId, _) <- nearestMyNode nodeId
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf $ myNodeId)]
onQuery' (FindNode nodeId target)
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
onQuery' (GetPeers nodeId infoHash)
    = do (myNodeId, _) <- nearestMyNode infoHash
         targets <- ctxTargets <$> ask
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf myNodeId),
                       (BString $ B8.pack "token",
                        BString token),
                       (BString $ B8.pack "values",
                        targets)]
onQuery' (AnnouncePeer nodeId infoHash port token)
    = do (myNodeId, _) <- nearestMyNode infoHash
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf myNodeId)]
onQuery' _
    = return $ Left $
      Error 204 $ B8.pack "Method Unknown"

onQuery addr q
    = do logger <- ctxLogger <$> ask
         liftIO $ logger q
         onQuery' q

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
    = do peers <- take 8 <$> nearestPeers target
         now <- now
         let peerFilter (_, peer) =
                 case peerLastQuery peer of
                   Nothing -> True
                   Just lastQuery ->
                       lastQuery < now - 90
             peers' = take 2 $ filter peerFilter peers
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

settler :: App ()
settler
    = popSettleQueue >>= settle

popSettleQueue :: App NodeId
popSettleQueue
    = do app <- getState
         case Seq.viewl $ stSettleQueue app of
           nodeId :< settleQueue ->
               do putState $ app { stSettleQueue = settleQueue }
                  return nodeId
           _ ->
               do let nodeId : nodeIds = Map.keys $ stMyNodes app
                  putState $ app { stSettleQueue = Seq.fromList nodeIds }
                  return nodeId

-- Compaction

purge :: App ()
purge = do now <- now
           app <- getState
           let peers = stPeers app
               keep peer
                   = (peerLastSeen peer > now - 300) ||
                     (case peerLastResponse peer of
                        Just lastResponse -> lastResponse > now - 600
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

-- Stats

stats :: App ()
stats = do app <- getState
           liftIO $ putStrLn $
                  "MyNodes: " ++ show (Map.size $ stMyNodes app) ++
                  "\tPeers: " ++ show (Map.size $ stPeers app)

-- Main

makeTargets :: [(String, String)] -> IO BValue
makeTargets hostsPorts = BList <$> map (BString . encodeAddr) <$>
                         (forM hostsPorts $ \(host, port) ->
                              head <$> Node.getAddrs host port)

runSpoofer port myNodeIds
    = do log <- newLog 1.0 "trackerspoofer.data"
         evLoop <- Ev.evRecommendedBackends >>=
                   Ev.evDefaultLoop
         node <- Node.new evLoop port

         let app = AppState { stMyNodes = Map.fromList $
                                          map (, ()) myNodeIds,
                              stPeers = Map.empty,
                              stSettleQueue = Seq.empty
                            }
         appRef <- newIORef app
         nodeRef <- newIORef node
         targets <- makeTargets []
         let ctx = AppContext { ctxState = appRef,
                                ctxNode = nodeRef,
                                ctxTargets = targets,
                                ctxEvLoop = evLoop,
                                ctxLogger = log
                              }
             appCall :: App a -> IO a
             appCall f = runReaderT f ctx
             appCallback f a q = appCall $ f a q

         Node.setQueryHandler (appCallback onQuery) node
         Node.setReplyHandler (appCallback onReply) node

         appCall $ do
           setInterval 0.2 $ settler
           setInterval 10.0 $ purger
           setInterval 1.0 $ stats

         Ev.evLoop evLoop 0

main = do args <- getArgs
          myNodeIds <- concat <$>
                       forM args MagnetGrep.grep 
          case length myNodeIds of
            0 ->
                putStrLn "Cannot start without any magnet links"
            _ ->
                runSpoofer 10000 myNodeIds
