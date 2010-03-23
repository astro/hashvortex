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
import Data.List (intercalate, sortBy, groupBy, nub)
import Data.Ix (inRange)
import Text.Tabular
import qualified Text.Tabular.AsciiArt as AA

import qualified ControlSocket
import EventLog
import KRPC
import qualified Node as Node
import NodeId
import qualified BEncoding as B
import InState

data ServerState = Server NodeId (Array Int Bucket) TrackerAddrs TrackerStats
             deriving (Show)
type Bucket = Map NodeId Peer
data PeerState = Good | Questionable | Bad
               deriving (Eq, Ord, Show)
data Peer = Peer { peerState :: PeerState,
                   peerAddress :: SockAddr,
                   peerLastReply :: Time,
                   peerLastSend :: Time
                 }
            deriving (Show, Eq)
type TrackerStats = Map NodeId (Integer, Integer)
type TrackerAddrs = [SockAddr]
type Time = POSIXTime
type ServerAction a = StateT ServerState IO a

param_n = 160
param_k = 8
param_k_nonfull = param_k * 3

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
    = do log <- newLog 1.0 logPath
         mgr <- Ev.new
         node <- Node.new mgr port

         refServer <- newIORef $
                       Server nodeId (array (0, param_n) $
                                      zip [0..param_n]  $ repeat Map.empty) [] Map.empty
         let inContext = withServer refServer
         Node.setQueryHandler (\addr query ->
                                   withServer refServer $
                                   handleQuery log addr query
                              ) node
         Node.setReplyHandler (\addr reply ->
                                   withServer refServer $
                                   handleReply addr reply
                              ) node
         withServer refServer $
                     bootstrap node "router.bittorrent.com" 6881

         ControlSocket.listenSocket mgr "server.sock" $
                          \command ->

                              withServer refServer $
                              case command of
                                ["buckets"] ->
                                    listBuckets
                                ["nodeid"] ->
                                    (++ "\n") `liftM` show `liftM` getNodeId
                                ["peers"] ->
                                    do now <- liftIO $ getPOSIXTime
                                       myNodeId <- getNodeId
                                       Server _ buckets _ _ <- get
                                       let peers = concatMap Map.toList $ elems buckets
                                       return $ AA.render id id id $
                                              Table (Group SingleLine $
                                                           map (Group NoLine) $
                                                           map (map $ Header . show . peerAddress . snd) $
                                                           groupBy (\(nodeId1, _) (nodeId2, _) ->
                                                                        (myNodeId `distanceOrder` nodeId1)
                                                                        ==
                                                                        (myNodeId `distanceOrder` nodeId2)
                                                                   ) $
                                                           peers)
                                                        (Group SingleLine [Header "State",
                                                                           Header "Dist",
                                                                           Header "NodeId",
                                                                           Header "Last send",
                                                                           Header "Last reply"])
                                                        [[show $ peerState peer,
                                                          show $ myNodeId `distanceOrder` nodeId,
                                                          show $ nodeId,
                                                          showTimeDiff now (peerLastSend peer),
                                                          showTimeDiff now (peerLastReply peer)]
                                                         | (nodeId, peer) <- peers]
                                ["tracker"] ->
                                    do Server _ _ _ stats <- get
                                       let stats' = Map.toList stats
                                       return $ AA.render id id id $
                                              Table (Group NoLine $
                                                           map (Header . show . fst) stats')
                                                        (Group SingleLine [Header "Reads",
                                                                           Header "Writes"])
                                                        [[show r, show w]
                                                         | (_, (r, w)) <- stats']
                                ["pclear"] ->
                                    do Server myNodeId buckets _ stats <- get
                                       put $ Server myNodeId buckets [] stats
                                       return "Cleared\n"
                                ["plist"] ->
                                    do Server _ _ addrs _ <- get
                                       return $ (intercalate " " $ map show addrs) ++ "\n"
                                "padd" : addrS ->
                                    do Server myNodeId buckets addrs stats <- get
                                       addrs' <- liftIO $
                                                 forM addrS $ \s ->
                                                 let (host, ':' : port) = break (== ':') s
                                                 in nub `liftM`
                                                    Node.getAddrs host port
                                       let addrs'' = addrs ++ concat addrs'
                                       put $ Server myNodeId buckets addrs'' stats
                                       return "Added\n"
                                [] -> return ""
                                cmd:_ -> return $ "Unknown command " ++ show cmd ++ "\n"

         let tick = do inContext $ schedule node
                       Ev.registerTimeout mgr 100 tick
                       return ()
         tick
         Ev.loop mgr
    where showTimeDiff _ 0 = "Never"
          showTimeDiff now t = show $ now - t

withServer :: IORef ServerState -> ServerAction a -> IO a
withServer = refInStateT

getNodeId :: ServerAction NodeId
getNodeId = get >>= \(Server nodeId _ _ _) -> return nodeId

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
handleQuery log addr query
    = do liftIO $ putStrLn $ "Query from " ++ show addr ++ ": " ++ show query
         liftIO $ log query

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
               do nodes <- take 8 `liftM` selectNodesFor target
                  liftIO $ putStrLn $ "Selected for " ++ show target ++ ":\n" ++ show nodes
                  return $
                         Right $
                         B.bdict [("id", B.BString myNodeId'),
                                  ("nodes", B.BString $ encodeNodes nodes)
                                 ]
           GetPeers nodeId infoHash ->
               do let token = C.pack "foo"

                  nodes <- take 8 `liftM`
                           filter (\(nodeId, addr) ->
                                       (nodeId <-> infoHash) < (myNodeId <-> infoHash)
                                  ) `liftM` selectNodesFor infoHash
                  case nodes of
                    -- No nearer nodes to infoHash, we are responsible
                    [] ->
                        do addrs <- trackGet infoHash
                           return $
                                  Right $
                                  B.bdict [("id", B.BString myNodeId'),
                                           ("token", B.BString token),
                                           ("values", B.BList $ map (B.BString . encodeAddr) addrs)]
                    -- Nearer nodes available, act like FindNode with
                    -- token. No idea why token would be needed in this
                    -- case?
                    _ ->
                        do liftIO $ putStrLn $ "Redirecting GetPeers for " ++ show infoHash
                           return $
                                  Right $
                                  B.bdict [("id", B.BString myNodeId'),
                                           ("token", B.BString token),
                                           ("nodes", B.BString $ encodeNodes nodes)]
           AnnouncePeer nodeId infoHash port token ->
               do let valid = True
                  case valid of
                    True ->
                        do trackPeer infoHash addr port
                           return $
                                  Right $
                                  B.bdict [("id", B.BString myNodeId')]
                    False ->
                        return $
                               Left $
                               Error 203 $ C.pack "Bad token"
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
        = do Server _ buckets _ _ <- get
             return $ buckets ! n
    putBucket n bucket
        = do Server nodeId buckets addrs stats <- get
             put $ Server nodeId (buckets // [(n, bucket)]) addrs stats
instance BucketIndex NodeId where
    getBucket nodeId
        = do myNodeId <- getNodeId
             getBucket $ nodeId `distanceOrder` myNodeId
    putBucket nodeId bucket
        = do myNodeId <- getNodeId
             putBucket (nodeId `distanceOrder` myNodeId) bucket

selectNodesFor :: NodeId -> ServerAction [(NodeId, SockAddr)]
selectNodesFor target
    = do d <- distanceOrder target `liftM` getNodeId
         concat `liftM`
                mapM (\n ->
                          map (\(nodeId, peer) ->
                                    (nodeId, peerAddress peer)
                              ) `liftM`
                          sortBy (\(nodeId1, peer1) (nodeId2, peer2) ->
                                      (target <-> nodeId1) `compare`
                                                               (target <-> nodeId2)
                                 ) `liftM`
                          filter (\(nodeId, peer) ->
                                      peerState peer == Good
                                 ) `liftM`
                          Map.toList `liftM`
                          getBucket n
                     ) (filter (inRange (0, param_n)) $
                        reverse [0..d] ++ [(d + 1)..160])

isFull :: Bucket -> Bool
isFull
    = (>= 8) .
      Map.size .
      Map.filter ((== Good) . peerState)

seen :: NodeId -> SockAddr -> ServerAction ()
seen nodeId addr
    = do bucket <- getBucket nodeId
         isMe <- (nodeId ==) `liftM` getNodeId
         when (not $ isMe || isFull bucket) $
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
                                             if peerLastSend peer > peerLastReply peer &&
                                                peerLastReply peer + timeout <= now
                                             then (io, peer { peerState = Bad})
                                             else (io, peer)
                                    ) (return ()) $
                       cutBucket param_k bucket
                   False ->
                       Map.mapAccum (\io peer ->
                                         if peerLastSend peer + queryInterval <= now
                                         then (io >> sendRequest node myNodeId (peerAddress peer),
                                               peer { peerLastSend = now})
                                         else
                                             if peerLastSend peer > peerLastReply peer &&
                                                peerLastReply peer + timeout <= now
                                             then (io, peer { peerState = Bad})
                                             else (io, peer)
                                    ) (return ()) $
                       cutBucket param_k_nonfull bucket
         putBucket n bucket'
         io
    where cutBucket n bucket
              | Map.size bucket <= n = bucket
              | otherwise = Map.fromList $
                            take n $
                            sortPeers $
                            Map.toList bucket
          sortPeers = sortBy (\(nodeId1, peer1) (nodeId2, peer2) ->
                                  (peerState peer1 `compare` peerState peer2)
                                  `compareFurther`
                                  (peerLastReply peer1 `compare` peerLastReply peer2)
                             )
          compareFurther EQ = id
          compareFurther ord = const ord

sendRequest :: Node.Node -> NodeId -> SockAddr -> ServerAction ()
sendRequest node target dest
    = do myNodeId <- getNodeId
         let query = FindNode myNodeId target
         liftIO $ putStrLn $ "Sending request to " ++ show dest ++ " for " ++ show target
         liftIO $ Node.sendQueryNoWait dest query node

listBuckets :: ServerAction String
listBuckets
    = concat `liftM`
      forM [0..param_n] (\n ->
                             getBucket n >>= \bucket ->
                             if Map.size bucket > 0
                             then return $
                                  show n ++ (if isFull bucket
                                             then "*"
                                             else "") ++ ": " ++
                                       intercalate " " (map showPeer $ Map.toList bucket) ++
                                       "\n"
                             else return ""
                        )
    where showPeer (nodeId, peer)
              = showAddress (peerAddress peer) ++
                "[" ++ take 1 (show $ peerState peer) ++ "]"
          showAddress = fst . break (== ':') . show

trackPeer :: NodeId -> SockAddr -> Integer -> ServerAction ()
trackPeer infoHash _addr _port
    = do Server myNodeId buckets addrs stats <- get
         let stats' = Map.alter (\old -> Just $
                                         case old of
                                           Nothing -> (0, 1)
                                           Just (r, w) -> (r, w + 1)
                                 ) infoHash stats
         stats' `seq`
                put $ Server myNodeId buckets addrs stats'

trackGet :: NodeId -> ServerAction TrackerAddrs
trackGet infoHash
    = do Server myNodeId buckets addrs stats <- get
         let stats' = Map.alter (\old -> Just $
                                         case old of
                                           Nothing -> (1, 0)
                                           Just (r, w) -> (r + 1, w)
                                 ) infoHash stats
         stats' `seq`
                put $ Server myNodeId buckets addrs stats'
         return addrs



(?<) = B.bdictLookup
