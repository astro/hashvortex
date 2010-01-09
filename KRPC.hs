module KRPC where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as W8
import qualified Data.ByteString as SW8
import Data.LargeWord (Word160)
import Data.Binary.Get
import Data.Binary.Put
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

decodePacket :: SW8.ByteString -> Either String Packet
decodePacket buf
    = case decode buf of
        Right pkt@(BDict _) ->
            fromMaybe (Left "Malformed packet") $
            do BString t <- pkt `bdictLookup` "t"
               BString y <- pkt `bdictLookup` "y"
               case B8.unpack y of
                 "q" ->
                     do BString q <- pkt `bdictLookup` "q"
                        let q' = B8.unpack q
                        a@(BDict _) <- pkt `bdictLookup` "a"
                        return $ Right $ QPacket (T t) $ decodeQuery q' a
                 "r" ->
                     do r@(BDict _) <- pkt `bdictLookup` "r"
                        return $ Right $ RPacket (T t) r
                 "e" ->
                     do BList [BInteger eN, BString eS] <- pkt `bdictLookup` "e"
                        return $ Right $ EPacket (T t) $ Error eN eS
                 _ -> fail $ "Invalid packet type: " ++ show y
        Left e -> Left $ "decode: " ++ e

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


decodeNodes :: W8.ByteString -> [(NodeId, SockAddr)]
decodeNodes = let step = remaining >>= \remaining ->
                         if remaining < 26
                         then return []
                         else
                             do id <- NodeId `liftM` getBytes 20
                                ip <- getWord32host
                                port <- fromIntegral `liftM` getWord16be
                                let addr = SockAddrInet port ip
                                ((id, addr):) `liftM` step
              in runGet step

encodeNodes :: [(SockAddr, NodeId)] -> W8.ByteString
encodeNodes = runPut . mapM_
              (\(SockAddrInet port ip, nodeId) ->
                   do putLazyByteString $ nodeIdToBuf nodeId
                      putWord32host ip
                      putWord16be $ fromIntegral port
              )

encodeAddr :: SockAddr -> W8.ByteString
encodeAddr (SockAddrInet port ip) = runPut $ do putWord32host ip
                                                putWord16be $ fromIntegral port