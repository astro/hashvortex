module KRPC where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as W8
import qualified Data.ByteString as SW8
import Data.LargeWord (Word160)
import Data.Binary.Get
import Network.Socket (SockAddr(SockAddrInet))
import Data.Maybe (fromMaybe)
import Control.Monad

import BEncoding
import NodeId
import IntBuf


newtype T = T B8.ByteString
    deriving (Eq, Show, Ord)

tSucc :: T -> T
tSucc = mapT $ integerToBuf . (+ 1) . bufToInteger
    where mapT f (T b) = T $ f b


data Packet = QPacket T Query
            | RPacket T Reply
            | EPacket T Error
           deriving (Show, Eq)
data Query = Ping NodeId
           | FindNode NodeId NodeId
           | GetPeers NodeId NodeId
           | AnnouncePeer NodeId NodeId Integer B8.ByteString
           | OtherQuery String BValue
           deriving (Show, Eq)
type Reply = BValue
{-data Reply = PingReply NodeId
           | FindNodeReply NodeId [(NodeId, SockAddr)]
           deriving (Show, Eq)-}
data Error = Error Integer B8.ByteString
           deriving (Show, Eq)

decodePacket :: B8.ByteString -> Packet
decodePacket buf
    = let pkt@(BDict _) = decode buf
          get k d = fromMaybe (error $ "No such key: " ++ k) $
                    d `bdictLookup` k
          getS k d = let Just (BString v) = d `bdictLookup` k
                     in B8.unpack v
          BString t = get "t" pkt
          y = getS "y" pkt
          q = getS "q" pkt
          a@(BDict _) = get "a" pkt
          r@(BDict _) = get "r" pkt
          BList [BInteger eN, BString eS] = get "e" pkt
      in case y of
           "q" -> QPacket (T t) $ decodeQuery q a
           "r" -> RPacket (T t) r
           "e" -> EPacket (T t) $ Error eN eS

decodeQuery q a
    = let getId k = let Just (BString s) = a `bdictLookup` k
                    in makeNodeId s
          aId = getId "id"
          aTarget = getId "target"
          aInfoHash = getId "info_hash"
          Just (BInteger aPort) = a `bdictLookup` "port"
          Just (BString aToken) = a `bdictLookup` "token"
      in case q of
           "ping" -> Ping aId
           "find_node" -> FindNode aId aTarget
           "get_peers" -> GetPeers aId aInfoHash
           "announce_peer" -> AnnouncePeer aId aInfoHash aPort aToken
           _ -> OtherQuery q a

encodePacket :: Packet -> B8.ByteString
encodePacket (QPacket (T t) qry)
    = let (q, a) = case qry of
                     Ping id ->
                         ("ping", BDict [(BString $ B8.pack "id",
                                          BString $ nodeIdToBuf id)])
                     FindNode id target ->
                         ("find_node", BDict [(BString $ B8.pack "id",
                                               BString $ nodeIdToBuf id),
                                              (BString $ B8.pack "target",
                                               BString $ nodeIdToBuf target)])
      in encode $
         BDict [(BString $ B8.singleton 't', BString t),
                (BString $ B8.singleton 'y', BString $ B8.singleton 'q'),
                (BString $ B8.singleton 'q', BString $ B8.pack q),
                (BString $ B8.singleton 'a', a)]
encodePacket (RPacket (T t) reply)
    = encode $
      BDict [(BString $ B8.singleton 't', BString t),
             (BString $ B8.singleton 'y', BString $ B8.singleton 'r'),
             (BString $ B8.singleton 'r', reply)]
encodePacket (EPacket (T t) (Error code msg))
    = encode $
      BDict [(BString $ B8.singleton 't', BString t),
             (BString $ B8.singleton 'y', BString $ B8.singleton 'e'),
             (BString $ B8.singleton 'e', BList [BInteger code,
                                                 BString msg]
             )]


decodeNodes :: W8.ByteString -> [(SockAddr, NodeId)]
decodeNodes = let step = remaining >>= \remaining ->
                         if remaining < 26
                         then return []
                         else
                             do id <- NodeId `liftM` getBytes 20
                                ip <- getWord32host
                                port <- fromIntegral `liftM` getWord16be
                                let addr = SockAddrInet port ip
                                ((addr, id):) `liftM` step
              in runGet step

