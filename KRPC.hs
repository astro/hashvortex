module KRPC where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as W8
import Data.LargeWord (Word160)
import Data.Binary.Put
import Data.Bits
import Network.Socket (SockAddr)
import Data.Maybe (fromMaybe)
import Control.Monad

import BEncoding


newtype T = T B8.ByteString
    deriving (Eq, Show, Ord)

data Packet = QPacket T Query
            | RPacket T Reply
            | EPacket T Error
           deriving (Show, Eq)
data Query = Ping NodeId
           | FindNode NodeId NodeId
           deriving (Show, Eq)
type Reply = BValue
data Error = Error Integer B8.ByteString
           deriving (Show, Eq)

newtype NodeId = NodeId Word160
    deriving (Show, Eq)

decodePacket :: B8.ByteString -> Packet
decodePacket buf
    = let BDict pkt = decode buf
          get k d = fromMaybe (error $ "No such key: " ++ k) $
                    (BString $ B8.pack k) `lookup` d
          getS k d = let BString v = get k d
                     in B8.unpack v
          BString t = get "t" pkt
          y = getS "y" pkt
          q = getS "q" pkt
          BDict a = get "a" pkt
          BString aId' = get "id" a
          aId = parseNodeId aId'
          BString aTarget' = get "target" a
          aTarget = parseNodeId aTarget'
          r@(BDict _) = get "r" pkt
          BList [BInteger eN, BString eS] = get "e" pkt
      in case y of
           "q" -> QPacket (T t) $
                  case q of
                    "ping" -> Ping aId
                    "find_node" -> FindNode aId aTarget
           "r" -> RPacket (T t) r
           "e" -> EPacket (T t) $ Error eN eS

encodePacket :: Packet -> B8.ByteString
encodePacket (QPacket (T t) qry)
    = let (q, a) = case qry of
                     Ping nodeId ->
                         ("ping", BDict [(BString $ B8.pack "id",
                                          BString $ serializeNodeId nodeId)])
                     FindNode nodeId target ->
                         ("find_node", BDict [(BString $ B8.pack "id",
                                               BString $ serializeNodeId nodeId),
                                              (BString $ B8.pack "target",
                                               BString $ serializeNodeId target)])
      in encode $
         BDict [(BString $ B8.singleton 't', BString t),
                (BString $ B8.singleton 'y', BString $ B8.singleton 'q'),
                (BString $ B8.singleton 'q', BString $ B8.pack q),
                (BString $ B8.singleton 'a', a)]

serializeNodeId :: NodeId -> B8.ByteString
serializeNodeId (NodeId word)
    = runPut $
      forM_ (reverse [0..19]) $ \i ->
          putWord8 $ fromIntegral $ word `shiftR` (i * 8) .&. 0xFF

parseNodeId :: B8.ByteString -> NodeId
parseNodeId bs
    | B8.length bs == 20
    = NodeId $
      fromIntegral $
      W8.foldl (\r w8 ->
                    (r `shiftL` 8) .|. w8
               ) 0 bs
                    
tToInteger :: T -> Integer
tToInteger (T t) = W8.foldl (\r w8 ->
                                 (r `shiftL` 8) .|. fromIntegral w8
                            ) 0 t

tFromInteger :: Integer -> T
tFromInteger 0 = T W8.empty
tFromInteger n = let T pre = tFromInteger (n `shiftR` 8)
                 in T $
                    pre `W8.append`
                    W8.singleton (fromInteger n .&. 0xFF)

tPlus :: T -> Integer -> T
tPlus = flip $ \offset -> tFromInteger . (+ offset) . tToInteger
