module Main where

import System.Environment
import Control.Monad.Maybe
import Data.Maybe
import Control.Applicative
import Data.Array.IArray

import qualified Node as Node
import qualified BEncoding as B
import qualified System.Event as Ev
import NodeId

-- Startup

main = do args <- getArgs
          case args of
            [portS, logPath] ->
                do nodeId <- makeRandomNodeId
                   putStrLn $ "Random nodeId: " ++ show nodeId
                   run (read portS) logPath nodeId
            [portS, logPath, torrentFile] ->
                do infoHash <- (fromMaybe $ error "No infohash") <$>
                               runMaybeT (MaybeT (B.parseFile torrentFile) >>=
                                          MaybeT . B.infoHash)
                   let nodeId = makeNodeId infoHash
                   putStrLn $ "Extracted infoHash: " ++ show nodeId
                   run (read portS) logPath nodeId
            _ ->
                do progName <- getProgName
                   putStrLn $ "Usage: " ++ progName ++ " <port> <log-path> [node-id-source.torrent]"

run :: Int -> FilePath -> NodeId -> IO ()
run port logPath nodeId
    = do mgr <- Ev.new
         node <- Node.new mgr port
         Ev.loop mgr

-- Protocol parameters

param_n = 160
param_k = 8
param_alpha = 3

-- Routing data structures

data Buckets = Buckets NodeId (Array Int Bucket)
type Bucket = Map NodeId Peer

newBuckets nodeId
    = Buckets nodeId $
      array (0, param_n) $
      zip [0..param_n] $ repeat []

data Peer = Peer { peerId :: NodeId,
                   peerAddress :: SockAddr,
                   peerState :: PeerState,
                   peerLastRequest :: Time,
                   peerLastResponse :: Time
                 }
data PeerState = Good
               | Questionable
               | Bad
                 deriving (Show, Eq, Ord)

-- |Modify a bucket
mapBucket :: Buckets -> Int -> (Bucket -> (a, Bucket)) -> (a, Buckets)
mapBucket (Buckets nodeId buckets) order f
    = let bucket = buckets ! order
          (a, bucket') = f bucket
          bucket'' = Map.filter ((/= Bad) . peerState) bucket'
          buckets' = buckets // [(order, bucket'')]
      in (a, Buckets nodeId buckets')

isBucketFull :: Buckets -> Int -> Bool
isBucketFull buckets order
    = fst $ mapBucket buckets order $
      (>= 8) .
      Map.length .
      Map.filter ((== Good) . peerState)


{-
-- |Modify a peer
withPeer :: Buckets -> NodeId -> (Peer -> Peer) -> Buckets
withPeer (Buckets nodeId buckets) peerNodeId f
    = let order = nodeId `distanceOrder` peerNodeId
          bucket = buckets !! order
          peer = 
-}