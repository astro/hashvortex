module Peers where

import Control.Concurrent.MVar
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Network.Socket (SockAddr)
import Data.List (partition)
import NodeId


data Peer = Peer { peerAddr :: SockAddr,
                   peerId :: NodeId,
                   peerLastFindNode :: Maybe POSIXTime
                 }
          deriving (Show)
type Peers = MVar [Peer]


newPeers = newMVar []

findNodeInterval = fromInteger 3600

nextPeer :: Peers -> IO (Maybe Peer)
nextPeer peers
    = withMVar peers $ \peers ->
      do now <- getPOSIXTime
         let notQueriedPeers = filter ((Nothing ==) . peerLastFindNode) peers
             peersToRequery = filter (\peer ->
                                          case peerLastFindNode peer of
                                            Just mlfn
                                                | now - mlfn >= findNodeInterval ->
                                                    True
                                            _ -> False
                                     ) peers
         return $ notQueriedPeers >+ peersToRequery
    where (>+) :: [a] -> [a] -> Maybe a
          (>+) (a:_) _ = Just a
          (>+) _ (a:_) = Just a
          (>+) [] [] = Nothing

updatePeer :: Peers -> Peer -> IO ()
updatePeer peers peer = modifyMVar_ peers $ return . flip updatePeers peer
    where updatePeers :: [Peer] -> Peer -> [Peer]
          updatePeers peers peer = let (olds, peers') = partition ((peerAddr peer ==) . peerAddr) peers
                                       new (old:_) = old { peerId = peerId peer,
                                                           peerLastFindNode = peerLastFindNode peer `maxLast` peerLastFindNode old
                                                         }
                                       new _ = peer
                                   in (new olds):peers'
          maxLast :: Maybe POSIXTime -> Maybe POSIXTime -> Maybe POSIXTime
          maxLast Nothing b = b
          maxLast a Nothing = a
          maxLast (Just a) (Just b) | a > b = Just a
                                    | otherwise = Just b
