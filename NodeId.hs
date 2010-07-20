module NodeId where

import Control.Monad
import qualified Data.ByteString as W8
import qualified Data.ByteString.Lazy as LW8
import Numeric (showHex, readHex)
import Data.Bits
import System.Random
import Control.DeepSeq

import IntBuf


newtype NodeId = NodeId W8.ByteString
    deriving (Eq, Ord)

instance Show NodeId where
    show (NodeId b) = pad 40 $ showHex (bufToInteger b) ""
        where pad len s | length s < len = pad len $ '0':s
                        | otherwise = s

instance NFData NodeId where
    rnf (NodeId bs) = bs `seq` ()

class NodeIdSource a where
    makeNodeId :: a -> NodeId
instance NodeIdSource W8.ByteString where
    makeNodeId = NodeId
instance NodeIdSource LW8.ByteString where
    makeNodeId = NodeId . W8.concat . LW8.toChunks

makeRandomNodeId :: IO NodeId
makeRandomNodeId = (NodeId .
                    W8.pack .
                    take 20 .
                    map fromInteger .
                    randomRs (0, 255)) `liftM` newStdGen

nodeIdToBuf :: NodeId -> LW8.ByteString
nodeIdToBuf (NodeId bs) = LW8.fromChunks [bs]

distance :: NodeId -> NodeId -> Integer
distance (NodeId a) (NodeId b)
    = bufToInteger $ W8.pack $ W8.zipWith xor a b

(<->) :: NodeId -> NodeId -> Integer
(<->) = distance

distanceOrder :: NodeId -> NodeId -> Int
distanceOrder a b = let r 0 = 0
                        r n = 1 + r (n `shiftR` 1)
                    in r $ distance a b

nodeIdPlus :: NodeId -> Integer -> NodeId
nodeIdPlus (NodeId buf) off = NodeId $ integerToBuf $ bufToInteger buf + off


hexToNodeId :: String -> Maybe NodeId
hexToNodeId s
    | length s == 40 = Just $ NodeId $
                       W8.pack $
                       map hexToByte $
                       chunkify 2 s
    | otherwise = Nothing
    where hexToByte s' = let [(i, "")] = readHex s'
                         in i
          chunkify size s'
              | length s' < size = []
              | otherwise = let (x, xs) = splitAt size s'
                            in x : chunkify size xs
