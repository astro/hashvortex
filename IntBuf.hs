module IntBuf (bufToInteger, integerToBuf) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Bits
import Test.QuickCheck hiding ((.&.))

class IntBuffer bs where
    bufToInteger :: bs -> Integer
    integerToBuf :: Integer -> bs

instance IntBuffer S.ByteString where
    bufToInteger = S.foldl (\r -> (r `shiftL` 8 .|.) . fromIntegral) 0
    integerToBuf 0 = S.empty
    integerToBuf n = integerToBuf (n `shiftR` 8)
                     `S.append`
                     S.singleton (fromInteger n .&. 0xFF)

instance IntBuffer L.ByteString where
    bufToInteger = L.foldl (\r -> (r `shiftL` 8 .|.) . fromIntegral) 0
    integerToBuf 0 = L.empty
    integerToBuf n = integerToBuf (n `shiftR` 8)
                     `L.append`
                     L.singleton (fromInteger n .&. 0xFF)


prop_reflexive1 bs = bs == integerToBuf (bufToInteger bs)
prop_reflexive2 n = n == bufToInteger (integerToBuf n :: L.ByteString)
