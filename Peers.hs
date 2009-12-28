{-# LANGUAGE TypeSynonymInstances, BangPatterns #-}
module Peers where

import Control.Concurrent.MVar
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Network.Socket (SockAddr(SockAddrInet))
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map

import NodeId


data Peer = Peer { peerAddr :: SockAddr,
                   peerId :: NodeId,
                   peerLastFindNode :: Maybe POSIXTime
                 }
          deriving (Show)
type Peers = MVar (Map SockAddr Peer)

instance Ord SockAddr where
    compare (SockAddrInet port1 ip1) (SockAddrInet port2 ip2)
        = case ip1 `compare` ip2 of
            EQ -> port1 `compare` port2
            r -> r


newPeers = newMVar Map.empty

findNodeInterval = fromInteger 3600

nextPeer :: Peers -> IO (Maybe Peer)
nextPeer peers
    = withMVar peers $ \peers ->
      do now <- getPOSIXTime
         let peerList = map snd $ Map.toList peers
             notQueriedPeers = filter ((Nothing ==) . peerLastFindNode) $ peerList
             peersToRequery = filter (\peer ->
                                          case peerLastFindNode peer of
                                            Just mlfn
                                                | now - mlfn >= findNodeInterval ->
                                                    True
                                            _ -> False
                                     ) peerList
         return $ notQueriedPeers >+ peersToRequery
    where (>+) :: [a] -> [a] -> Maybe a
          (>+) (a:_) _ = Just a
          (>+) _ (a:_) = Just a
          (>+) [] [] = Nothing

updatePeer :: Peers -> Peer -> IO ()
updatePeer peers !peer = modifyMVar_ peers $
                         return . Map.alter (-><- Just peer) addr
    where addr = peerAddr peer

class Mergeable a where
    (-><-) :: a -> a -> a
instance Mergeable Peer where
    a -><- b = Peer { peerAddr = m peerAddr,
                      peerId = m peerId,
                      peerLastFindNode = m peerLastFindNode }
        where m f = f a -><- f b
instance Mergeable SockAddr where
    _ -><- a = a
instance Mergeable NodeId where
    _ -><- a = a
instance Mergeable a => Mergeable (Maybe a) where
    Nothing -><- Nothing = Nothing
    a@(Just _) -><- Nothing = a
    Nothing -><- b@(Just _) = b
    (Just a) -><- (Just b) = Just $ a -><- b
instance Mergeable POSIXTime where
    a -><- b | a < b = a
             | otherwise = b
