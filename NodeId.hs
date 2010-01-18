module NodeId where

import Control.Monad
import qualified Data.ByteString as W8
import qualified Data.ByteString.Lazy as LW8
import Numeric (showHex)
import Data.Bits
import System.Random

import IntBuf


newtype NodeId = NodeId W8.ByteString
    deriving (Eq)

instance Show NodeId where
    show (NodeId b) = pad 40 $ showHex (bufToInteger b) ""
        where pad len s | length s < len = pad len $ '0':s
                        | otherwise = s

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

(<->) = distance

distanceOrder :: NodeId -> NodeId -> Int
distanceOrder a b = let r 0 = 0
                        r n = 1 + r (n `shiftR` 1)
                    in r $ distance a b

nodeIdPlus :: NodeId -> Integer -> NodeId
nodeIdPlus (NodeId buf) off = NodeId $ integerToBuf $ bufToInteger buf + off

