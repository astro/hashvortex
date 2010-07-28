{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment
import Network.Socket (SockAddr(SockAddrInet), PortNumber(PortNum))
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.Sequence (Seq, ViewL((:<), EmptyL), (|>))
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

type Time = Ev.EvTimestamp
data Peer = Peer { peerNodeId :: NodeId,
                   peerAddr :: SockAddr }

queryQueueMax = 256
data AppState = AppState { stQueryQueue :: Seq Peer }

data AppContext = AppContext { ctxState :: IORef AppState,
                               ctxNode :: IORef Node.Node,
                               ctxTargets :: [SockAddr],
                               ctxEvLoop :: Ev.EvLoopPtr,
                               ctxLogger :: Logger,
                               ctxPort :: PortNumber
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

appendPeer :: Peer -> App ()
appendPeer peer
    = do app <- getState
         myPort <- ctxPort <$> ask
         targets <- ctxTargets <$> ask
         let queue = stQueryQueue app
             portAllowed
                 = case peerAddr peer of
                     SockAddrInet peerPort _ -> peerPort /= myPort
                     _ -> False
             isTarget = peerAddr peer `elem` targets
         when (Seq.length queue < queryQueueMax &&
               portAllowed &&
               not isTarget) $
              let queue' = queue |> peer
              in queue' `seq`
                 putState $ app { stQueryQueue = queue' }

popPeer :: App (Maybe Peer)
popPeer
    = do app <- getState
         case Seq.viewl $ stQueryQueue app of
           peer :< queue ->
               do putState $ app { stQueryQueue = queue }
                  return $ Just peer
           _ ->
               return Nothing

-- Query handling

token = B8.pack "a"

onQuery' addr (Ping nodeId)
    = do appendPeer $ Peer nodeId addr
         nodeId' <- liftIO $ makeRandomNeighbor nodeId
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf nodeId')]
onQuery' addr (FindNode nodeId target)
    = do nodeId' <- liftIO $ makeRandomNeighbor nodeId
         targets <- ctxTargets <$> ask
         nodes <- encodeNodes <$>
                  mapM (\addr ->
                            do target' <- liftIO $ makeRandomNeighbor target
                               return (target', addr)
                       ) (take 8 targets)
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf nodeId'),
                       (BString $ B8.pack "nodes",
                        BString nodes)]
onQuery' addr (GetPeers nodeId infoHash)
    = do nodeId' <- liftIO $ makeRandomNeighbor nodeId
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf nodeId'),
                       (BString $ B8.pack "token",
                        BString token),
                       (BString $ B8.pack "values",
                        BList [])]
onQuery' addr (AnnouncePeer nodeId infoHash port token)
    = do nodeId' <- liftIO $ makeRandomNeighbor nodeId
         return $ Right $
                BDict [(BString $ B8.pack "id",
                        BString $ nodeIdToBuf nodeId')]
onQuery' addr _
    = return $ Left $
      Error 204 $ B8.pack "Method Unknown"

onQuery addr bvalue q
    = do logger <- ctxLogger <$> ask
         liftIO $ logger q
         onQuery' addr q

-- Reply handling

onReply addr bvalue reply
    = case reply `bdictLookup` "nodes" of
        Just (BString nodesBuf) ->
            do let nodes = decodeNodes nodesBuf
               forM nodes $ \(nodeId, addr) ->
                   appendPeer $ Peer nodeId addr
               return ()
        _ ->
            return ()

-- Querying

query :: App ()
query = do mbPeer <- popPeer
           node <- ctxNode <$> ask >>=
                   liftIO . readIORef
           liftIO $
                  case mbPeer of
                    Nothing ->
                        do addr : _ <- Node.getAddrs "router.bittorrent.com" "6881"
                           target <- makeRandomNodeId
                           target' <- makeRandomNeighbor target
                           let q = FindNode target target'
                           Node.sendQueryNoWait addr q node
                    Just peer ->
                        do target <- makeRandomNeighbor $ peerNodeId peer
                           target' <- makeRandomNodeId
                           let q = FindNode target target'
                           Node.sendQueryNoWait (peerAddr peer) q node

-- Main

makeTargets :: [(String, String)] -> IO [SockAddr]
makeTargets hostsPorts
    = forM hostsPorts $ \(host, port) ->
      head <$> Node.getAddrs host port

runSpoofer port
    = do evLoop <- Ev.evRecommendedBackends >>=
                   Ev.evDefaultLoop
         log <- newLog evLoop "nodespoofer.data"
         node <- Node.new evLoop port

         let app = AppState { stQueryQueue = Seq.empty }
         appRef <- newIORef app
         nodeRef <- newIORef node
         targets <- makeTargets []
         let ctx = AppContext { ctxState = appRef,
                                ctxNode = nodeRef,
                                ctxTargets = targets,
                                ctxEvLoop = evLoop,
                                ctxLogger = log,
                                ctxPort = PortNum $ fromIntegral port
                              }
             appCall :: App a -> IO a
             appCall f = runReaderT f ctx
             appCallback f a b q = appCall $ f a b q

         Node.setQueryHandler (appCallback onQuery) node
         Node.setReplyHandler (appCallback onReply) node

         appCall $ do
           setInterval 0.05 $ query

         Ev.evLoop evLoop 0

main = runSpoofer 10000
