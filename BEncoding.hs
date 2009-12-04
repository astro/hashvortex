module BEncoding (BValue(..), encode, decode, parseFile) where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as W8
import Data.Attoparsec.Char8 as P
import Data.Char (isDigit, chr)
import Control.Monad
import qualified Data.Digest.SHA1 as SHA1
import Test.QuickCheck


data BValue = BInteger Integer
            | BString B8.ByteString
            | BList [BValue]
            | BDict [(BValue, BValue)]
            deriving (Show, Eq, Ord)

encode :: BValue -> B8.ByteString
encode (BInteger i) = B8.singleton 'i' `B8.append`
                      (B8.pack $ show i) `B8.append`
                      B8.singleton 'e'
encode (BString s) = (B8.pack $ show $ B8.length s) `B8.append`
                     B8.singleton ':' `B8.append`
                     s
encode (BList xs) = B8.singleton 'l' `B8.append`
                    B8.concat (map encode xs) `B8.append`
                    B8.singleton 'e'
encode (BDict xs) = B8.singleton 'd' `B8.append`
                    B8.concat (map (\(k, v) ->
                                        encode k `B8.append` encode v
                                   ) xs) `B8.append`
                    B8.singleton 'e'


decode :: B8.ByteString -> BValue
decode bs = case P.parse decoder bs of
              (rest, Right a) | rest == B8.empty -> a
              (rest, Right _) -> error $ "Rest: " ++ B8.unpack rest
              (_, Left e) -> error $ "Parse: " ++ e

decoder :: Parser BValue
decoder = do c1 <- P.anyChar
             case c1 of
               'i' ->
                     do iS <- P.takeWhile (\c -> c == '-' || isDigit c)
                        P.char 'e'
                        let Just (i, _) = B8.readInteger iS
                        return $ BInteger i
               d | isDigit d ->
                     do lS <- (B8.cons d) `liftM` P.takeWhile isDigit
                        P.char ':'
                        let Just (l, _) = B8.readInteger lS
                        s <- (B8.take $ fromIntegral l) `liftM` getInput
                        forM_ [1..l] $ \_ -> P.anyChar
                        return $ BString s
               'l' ->
                     do BList `liftM` (P.manyTill decoder $ char 'e')
               'd' ->
                     do BDict `liftM` (P.manyTill (do k <- decoder
                                                      v <- decoder
                                                      return (k, v)
                                                  ) $ char 'e')
               _ -> fail $ "unexpected type: " ++ show c1


parseFile :: FilePath -> IO BValue
parseFile f = decode `liftM` B8.readFile f


infoHash :: BValue -> SHA1.Word160
infoHash (BDict dict) = let Just infoVal = lookup (BString $ B8.pack "info") dict
                        in sha1 $ encode infoVal
    where sha1 = SHA1.hash . W8.unpack

{-
instance Arbitrary Char where
  arbitrary = chr `fmap` oneof [choose (0,127), choose (0,255)]-}
instance Arbitrary B8.ByteString where
    arbitrary = B8.pack `liftM` arbitrary
instance Arbitrary BValue where
    arbitrary = frequency [(10, BInteger `liftM` arbitrary),
                           (5, resize 150 $ BString `liftM` arbitrary),
                           (2, resize 5 $ BList `liftM` arbitrary),
                           (2, resize 5 $ BDict `liftM` arbitrary)]

propEncodeDecode val = decode (encode val) == val
