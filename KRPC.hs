module KRPC where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as W8
import qualified Data.ByteString as SW8
import Data.Binary.Get
import Data.Binary.Put
import Network.Socket (SockAddr(SockAddrInet))
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.DeepSeq
import Control.Applicative

import BEncoding
import NodeId
import IntBuf


newtype T = T B8.ByteString
    deriving (Eq, Show, Ord)
instance NFData T where
    rnf (T bs) = rnf $ B8.unpack bs

tSucc :: T -> T
tSucc = mapT $ integerToBuf . (+ 1) . bufToInteger
    where mapT f (T b) = T $ f b


data Packet = QPacket T Query
            | RPacket T Reply
            | EPacket T Error
           deriving (Show, Eq)
instance NFData Packet where
    rnf (QPacket t query) = rnf t `seq`
                            rnf query
    rnf (RPacket t reply) = rnf t `seq`
                            rnf reply
    rnf (EPacket t error) = rnf t `seq`
                            rnf error
data Query = Ping NodeId
           | FindNode NodeId NodeId
           | GetPeers NodeId NodeId
           | AnnouncePeer NodeId NodeId Integer B8.ByteString
           | OtherQuery String BValue
           deriving (Show, Eq)
instance NFData Query where
    rnf (Ping nodeId) = rnf nodeId
    rnf (FindNode nodeId nodeId') = rnf nodeId `seq`
                                    rnf nodeId'
    rnf (GetPeers nodeId nodeId') = rnf nodeId `seq`
                                    rnf nodeId'
    rnf (AnnouncePeer nodeId nodeId' i bs) = rnf nodeId `seq`
                                             rnf nodeId' `seq`
                                             rnf i `seq`
                                             rnf (B8.unpack bs)
type Reply = BValue
{-data Reply = PingReply NodeId
           | FindNodeReply NodeId [(NodeId, SockAddr)]
           deriving (Show, Eq)-}
data Error = Error Integer B8.ByteString
           deriving (Show, Eq)
instance NFData Error where
    rnf (Error i bs) = rnf i `seq`
                       rnf (B8.unpack bs)

decodePacket :: SW8.ByteString -> Either String (BValue, Packet)
decodePacket buf
    = case decode buf of
        Right pkt@(BDict _) ->
            maybe (Left "Malformed packet")
                  (\result ->
                       case result of
                         Left e -> Left e
                         Right packet -> Right (pkt, packet)
                  ) $
            do BString t <- pkt `bdictLookup` "t"
               BString y <- pkt `bdictLookup` "y"
               case B8.unpack y of
                 "q" ->
                     do BString q <- pkt `bdictLookup` "q"
                        let q' = B8.unpack q
                        a@(BDict _) <- pkt `bdictLookup` "a"
                        query <- decodeQuery q' a
                        return $ Right $ QPacket (T t) query
                 "r" ->
                     do r@(BDict _) <- pkt `bdictLookup` "r"
                        return $ Right $ RPacket (T t) r
                 "e" ->
                     do BList [BInteger eN, BString eS] <- pkt `bdictLookup` "e"
                        return $ Right $ EPacket (T t) $ Error eN eS
                 _ -> fail $ "Invalid packet type: " ++ show y
        Right _ -> Left "Malformed packet"
        Left e -> Left $ "decode: " ++ e

decodeQuery q a
    = let getId k = do BString s <- a `bdictLookup` k
                       -- TODO: check s len
                       return $ makeNodeId s
      in case q of
           "ping" -> Ping <$>
                     getId "id"
           "find_node" -> FindNode <$>
                          getId "id" <*>
                                getId "target"
           "get_peers" -> GetPeers <$>
                          getId "id" <*>
                          getId "info_hash"
           "announce_peer" -> AnnouncePeer <$>
                              getId "id" <*>
                              getId "info_hash" <*>
                             (do BInteger port <- a `bdictLookup` "port"
                                 return port) <*>
                             (do BString token <- a `bdictLookup` "token"
                                 return token)

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

encodeNodes :: [(NodeId, SockAddr)] -> W8.ByteString
encodeNodes = runPut . mapM_
              (\(nodeId, SockAddrInet port ip) ->
                   do putLazyByteString $ nodeIdToBuf nodeId
                      putWord32host ip
                      putWord16be $ fromIntegral port
              )

encodeAddr :: SockAddr -> W8.ByteString
encodeAddr (SockAddrInet port ip) = runPut $ do putWord32host ip
                                                putWord16be $ fromIntegral port