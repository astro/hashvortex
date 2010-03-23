module Main where
 
import qualified Data.ByteString.Lazy.Char8 as C
import qualified System.Event as Ev
import Network.Socket (SockAddr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array.IArray
import Data.IORef
import Control.Monad.State.Lazy
import System.Environment
import Data.Maybe (fromMaybe)
import Control.Monad.Maybe
import Data.Time.Clock.POSIX
import Data.List (intercalate)

import EventLog
import KRPC
import qualified Node as Node
import NodeId
import qualified BEncoding as B

data Buckets = Buckets NodeId (Array Int Bucket)
             deriving (Show)
type Bucket = Map NodeId Peer
data PeerState = Good | Questionable | Bad
               deriving (Eq, Show)
data Peer = Peer { peerState :: PeerState,
                   peerAddress :: SockAddr,
                   peerLastReply :: Time,
                   peerLastSend :: Time
                 }
            deriving (Show)
type Time = POSIXTime
type ServerAction a = StateT Buckets IO a

param_n = 160

-- Startup

main = do args <- getArgs
          case args of
            [portS, logPath] ->
                do nodeId <- makeRandomNodeId
                   putStrLn $ "Random nodeId: " ++ show nodeId
                   run (read portS) logPath nodeId
            [portS, logPath, torrentFile] ->
                do infoHash <- (fromMaybe $ error "No infohash") `liftM`
                               runMaybeT (MaybeT (B.parseFile torrentFile) >>=
                                          MaybeT . B.infoHash)
                   let nodeId = makeNodeId infoHash
                   putStrLn $ "Extracted infoHash: " ++ show nodeId
                   run (read portS) logPath nodeId
            _ ->
                do progName <- getProgName
                   putStrLn $ "Usage: " ++ progName ++ " <port> <log-path> [node-id-source.torrent]"

run port logPath nodeId
    = do log <- newLog 1.0 "spoofer.data"
         mgr <- Ev.new
         node <- Node.new mgr port

         refBuckets <- newIORef $
                       Buckets nodeId $
                       array (0, param_n) $
                       zip [0..param_n]  $ repeat Map.empty
         let inContext = withBuckets refBuckets
         Node.setQueryHandler (\addr query ->
                                   withBuckets refBuckets $
                                   handleQuery log addr query
                              ) node
         Node.setReplyHandler (\addr reply ->
                                   withBuckets refBuckets $
                                   handleReply addr reply
                              ) node
         withBuckets refBuckets $
                     bootstrap node "router.bittorrent.com" 6881

         inContext $ get >>= liftIO . print

         let tick = do inContext $ schedule node
                       -- inContext listBuckets
                       Ev.registerTimeout mgr 100 tick
                       return ()
         tick
         Ev.loop mgr

withBuckets :: IORef Buckets -> ServerAction a -> IO a
withBuckets refBuckets f
    = do buckets <- readIORef refBuckets
         (a, buckets') <- runStateT f buckets
         writeIORef refBuckets buckets'
         return a

getNodeId :: ServerAction NodeId
getNodeId = get >>= \(Buckets nodeId _) -> return nodeId

bootstrap :: Node.Node -> String -> Int -> ServerAction ()
bootstrap node host port
    = do entryAddrs <- liftIO $ 
                       Node.getAddrs host (show port)
         myNodeId <- getNodeId
         forM_ entryAddrs $
                  \addr ->
                      sendRequest node myNodeId addr

handleQuery :: Logger -> SockAddr -> Query
            -> ServerAction (Either Error Reply)
handleQuery logger addr query
    = do liftIO $ putStrLn $ "Query from " ++ show addr ++ ": " ++ show query
         let nodeId = case query of
                        Ping nodeId -> nodeId
                        FindNode nodeId _ -> nodeId
                        GetPeers nodeId _ -> nodeId
                        AnnouncePeer nodeId _ _ _ -> nodeId
         seen nodeId addr

         myNodeId <- getNodeId
         let myNodeId' = nodeIdToBuf myNodeId

         case query of
           Ping nodeId ->
               return $
                      Right $
                      B.bdict [("id", B.BString myNodeId')]
           FindNode nodeId target ->
               return $
                      Right $
                      B.bdict [("id", B.BString myNodeId'),
                               ("nodes", B.BString C.empty)  -- TODO
                              ]
           _ ->
               return $
                      Left $
                      Error 201 $ C.pack "Not implemented"

handleReply :: SockAddr -> Reply -> ServerAction ()
handleReply addr reply
    = do case reply ?< "id" of
           Just (B.BString nodeId') ->
               let nodeId = makeNodeId nodeId'
               in receivedReply nodeId addr
           Nothing ->
               return ()

         case reply ?< "nodes" of
           Just (B.BString nodes') ->
               let nodes = decodeNodes nodes'
               in forM_ nodes $
                      \(nodeId, addr) ->
                          seen nodeId addr

class BucketIndex a where
    getBucket :: a -> ServerAction Bucket
    putBucket :: a -> Bucket -> ServerAction ()
instance BucketIndex Int where
    getBucket n
        = do Buckets _ buckets <- get
             return $ buckets ! n
    putBucket n bucket
        = do Buckets nodeId buckets <- get
             put $ Buckets nodeId $
                 buckets // [(n, bucket)]
instance BucketIndex NodeId where
    getBucket nodeId
        = do myNodeId <- getNodeId
             getBucket $ nodeId `distanceOrder` myNodeId
    putBucket nodeId bucket
        = do myNodeId <- getNodeId
             putBucket (nodeId `distanceOrder` myNodeId) bucket

isFull :: Bucket -> Bool
isFull
    = (>= 8) .
      Map.size .
      Map.filter ((== Good) . peerState)

seen :: NodeId -> SockAddr -> ServerAction ()
seen nodeId addr
    = do bucket <- getBucket nodeId
         when (not $ isFull bucket) $
              putBucket nodeId $
              Map.alter update nodeId bucket
    where update Nothing
              = Just $ Peer { peerState = Questionable,
                              peerAddress = addr,
                              peerLastReply = 0,
                              peerLastSend = 0
                            }
          update (Just peer)
              = Just $ case (peerState peer,
                             addr == peerAddress peer) of
                         -- Never allow address update of good peer
                         (Good, _) -> peer
                         -- Clear Bad ones with updated address
                         (Bad, False) -> Peer { peerAddress = addr,
                                                peerState = Questionable,
                                                peerLastReply = 0,
                                                peerLastSend = 0
                                              }
                         -- But others will be updated
                         (_, False) -> peer { peerAddress = addr,
                                              peerState = Questionable
                                            }
                         -- No change
                         (_, True) -> peer

receivedReply :: NodeId -> SockAddr -> ServerAction ()
receivedReply nodeId addr
    = do now <- liftIO getPOSIXTime
         bucket <- getBucket nodeId
         case Map.lookup nodeId bucket of
           -- Reply from a known peer
           Just peer ->
               do liftIO $ putStrLn $ show addr ++ " replied"
                  putBucket nodeId $
                            Map.insert nodeId (peer { peerState = Good,
                                                      peerLastReply = now
                                                    }) bucket
           -- Who is this? Do we need this one?
           Nothing ->
               do liftIO $ putStrLn $ show addr ++ " replied unexpectedly"
                  seen nodeId addr

queryInterval = 5 * 60
timeout = 60

schedule :: Node.Node -> ServerAction ()
schedule node
    = liftIO getPOSIXTime >>= \now ->
      getNodeId >>= \myNodeId ->
      forM_ [0..param_n] $ \n ->
      do bucket <- getBucket n
         let (io, bucket') =
                 case isFull bucket of
                   True ->
                       Map.mapAccum (\io peer ->
                                         if peerLastSend peer + queryInterval <= now
                                         then (io >> sendRequest node myNodeId (peerAddress peer),
                                               peer { peerLastSend = now})
                                         else
                                             if peerLastReply peer + timeout <= now
                                             then (io, peer { peerState = Bad})
                                             else (io, peer)
                                    ) (return ()) $
                                    Map.filter ((== Good) . peerState) $ bucket
                   False ->
                       Map.mapAccum (\io peer ->
                                         if peerLastSend peer + queryInterval <= now
                                         then (io >> sendRequest node myNodeId (peerAddress peer),
                                               peer { peerLastSend = now})
                                         else
                                             if peerLastReply peer + timeout <= now
                                             then (io, peer { peerState = Bad})
                                             else (io, peer)
                                    ) (return ()) $
                                    Map.filter (\peer ->
                                                    not (peerState peer == Bad &&
                                                         peerLastSend peer + queryInterval <= now)
                                               ) $ bucket
         putBucket n bucket'
         io

sendRequest :: Node.Node -> NodeId -> SockAddr -> ServerAction ()
sendRequest node target dest
    = do myNodeId <- getNodeId
         let query = FindNode myNodeId target
         liftIO $ putStrLn $ "Sending request to " ++ show dest ++ " for " ++ show target
         liftIO $ Node.sendQueryNoWait dest query node

listBuckets :: ServerAction ()
listBuckets
    = do liftIO $ putStrLn "Buckets:"
         forM_ [0..param_n] $ \n ->
             do bucket <- getBucket n
                when (Map.size bucket > 0) $
                     liftIO $ putStrLn $
                            show n ++ ": " ++ intercalate ", "
                                     (map showPeer $ Map.toList bucket)
    where showPeer (nodeId, peer)
              = show (peerAddress peer) ++
                "[" ++ show (peerState peer) ++ "]"

(?<) = B.bdictLookup
