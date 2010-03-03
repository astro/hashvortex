{-# OPTIONS_GHC -XNoBangPatterns #-}
module Main where

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
import Data.IORef
import qualified System.Event as Ev

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
          mgr <- Ev.new
          node <- Node.new mgr 10000
          nodeId <- makeRandomNodeId
          now <- getPOSIXTime
          entryAddrs <- Node.getAddrs "router.bittorrent.com" "6881"
          let entryCache = Seq.fromList [Peer nodeId entryAddr
                                         | entryAddr <- entryAddrs]
          rDigState <- newIORef $
                       DigState { stNode = node,
                                  stFindTarget = nodeId,
                                  stFind = Map.empty,
                                  stFindCount = 0,
                                  stSeen = Map.empty,
                                  stCache = entryCache
                                }
          Node.setQueryHandler (queryHandler log rDigState) node
          Node.setReplyHandler (replyHandler rDigState) node

          let digTime = do delay <- dig rDigState
                           Ev.registerTimeout mgr delay digTime
                           return ()
              statsTime = do printStats rDigState
                             Ev.registerTimeout mgr 1000 statsTime
                             return ()
          digTime
          statsTime
          Ev.loop mgr

resetDig :: IORef DigState -> IO ()
resetDig rDigState
    = do nodeId <- makeRandomNodeId
         modifyIORef rDigState $ \st ->
             st { stFindTarget = nodeId,
                  stFind = Map.empty,
                  stFindCount = 0,
                  stSeen = Map.empty,
                  stCache = Seq.empty
                }
         peers <- (toList . stCache) `liftM` readIORef rDigState
         mapM_ (insertFindPeer rDigState) peers

dig :: IORef DigState -> IO Int
dig rDigState = do let reset = resetDig rDigState
                       getNext =
                           do st <- readIORef rDigState
                              case (Map.null $ stFind st,
                                    Seq.null $ stCache st,
                                    Map.findMin $ stFind st) of
                                (_, False, _) | stFindCount st >= maxFindCount ->
                                      do reset
                                         getNext
                                (True, False, _) ->
                                      do reset
                                         getNext
                                (False, _, (_distance, peer)) ->
                                    do writeIORef rDigState $
                                                  st { stFind = Map.deleteMin $ stFind st,
                                                       stFindCount = stFindCount st + 1
                                                     }
                                       return $ Just (stNode st, stFindTarget st, peer)
                                _ ->
                                    return Nothing
                   next <- getNext
                   case next of
                     Just (node, target, Peer findNodeId findAddr) ->
                         do Node.sendQueryNoWait findAddr (FindNode target target) node
                            return $ 1000 `div` packetRate
                     Nothing ->
                         do return $ truncate $ idleSleep * 1000

printStats rDigState = do st <- readIORef rDigState
                          putStrLn $ "Find " ++ show (stFindTarget st) ++
                                       ": " ++ show (Map.size $ stSeen st) ++
                                       " seen, " ++ show (stFindCount st) ++
                                       " queried, " ++ show (Map.size $ stFind st) ++
                                       " to query, " ++ show (Seq.length $ stCache st) ++
                                       " cached"

replyHandler rDigState addr reply
    = do let replyFields = do BString nodeId' <- reply `bdictLookup` "id"
                              let nodeId = makeNodeId nodeId'
                              BString nodes' <- reply `bdictLookup` "nodes"
                              let nodes = decodeNodes nodes'
                              return (Peer nodeId addr, nodes)
         case replyFields of
           Just (replyer, nodes) ->
               forM_ nodes $ \(nodeId, addr) ->
                   do let peer = Peer nodeId addr
                      known <- isPeerKnown rDigState peer
                      unless known $
                             do insertCache rDigState peer
                                insertFindPeer rDigState peer
           Nothing -> return ()

isPeerKnown :: IORef DigState -> Peer -> IO Bool
isPeerKnown rDigState peer@(Peer _ addr)
    = do st <- readIORef rDigState
         return $ Map.member addr $ stSeen st

insertFindPeer :: IORef DigState -> Peer -> IO ()
insertFindPeer rDigState peer@(Peer nodeId addr)
    = modifyIORef rDigState $ \st ->
      let target = stFindTarget st
          distance = target <-> nodeId
      in st { stFind = Map.insert distance peer $ stFind st,
              stSeen = Map.insert addr () $ stSeen st
            }

insertCache rDigState peer
    = do st <- readIORef rDigState
         when (Seq.length (stCache st) < maxCacheSize) $
              writeIORef rDigState $ st { stCache = stCache st |> peer }

queryHandler log rDigState addr query
    = do log query
         known <- isPeerKnown rDigState peer
         unless known $
                insertCache rDigState peer
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

