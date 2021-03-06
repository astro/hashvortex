module BEncoding (BValue(..), encode, decode, parseFile, bdict, bdictLookup, infoHash) where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as W8
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString as SW8
import Data.Binary.Strict.Get
import Data.Char (isDigit, chr)
import Control.Monad
import Test.QuickCheck
import Data.List (intercalate)
import Control.DeepSeq
import OpenSSL.Digest (MessageDigest(SHA1))
import OpenSSL.Digest.ByteString.Lazy (digest)
import Prelude hiding (getChar, takeWhile)


data BValue = BInteger Integer
            | BString B8.ByteString
            | BList [BValue]
            | BDict [(BValue, BValue)]
            deriving (Eq, Ord)
instance NFData BValue where
    rnf (BInteger i) = rnf i
    rnf (BString bs) = rnf $ B8.unpack bs
    rnf (BList []) = ()
    rnf (BList (x:xs)) = rnf x `seq`
                       rnf (BList xs)
    rnf (BDict []) = ()
    rnf (BDict ((x, x'):xs)) = rnf x `seq`
                             rnf x' `seq`
                             rnf (BDict xs)

encode :: BValue -> B8.ByteString
encode (BInteger i) = B8.singleton 'i' `B8.append`
                      B8.pack (show i) `B8.append`
                      B8.singleton 'e'
encode (BString s) = B8.pack (show $ B8.length s) `B8.append`
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

bdict :: [(String, BValue)] -> BValue
bdict = BDict . map (\(s, v) ->
                         (BString $ B8.pack s, v)
                    )

instance Show BValue where
    show (BInteger i) = show i
    show (BString s) = show $ B8.unpack s
    show (BList l) = show l
    show (BDict d) = "{ " ++ (intercalate ", " $
                              map (\(k, v) -> show k ++ ": " ++ show v) d
                             ) ++
                     " }"


decode :: SB8.ByteString -> Either String BValue
decode bs = case runGet decoder bs of
              (Right a, _) -> Right a
              (Left e, _) -> Left $ "Parse: " ++ e

decoder :: Get BValue
decoder = do c1 <- getChar
             case c1 of
               'i' ->
                     do iS <- takeWhile (\c -> c == '-' || isDigit c)
                        char 'e'
                        let Just (i, _) = SB8.readInteger iS
                        return $ BInteger i
               d | isDigit d ->
                     do lS <- SB8.cons d `liftM` takeWhile isDigit
                        char ':'
                        let Just (l, _) = SB8.readInteger lS
                        s <- getByteString $ fromIntegral l
                        return $ BString $ B8.fromChunks [s]
               'l' ->
                     BList `liftM` manyTill decoder 'e'
               'd' ->
                     BDict `liftM` manyTill (do k <- decoder
                                                v <- decoder
                                                return (k, v)
                                            ) 'e'
               _ -> fail $ "unexpected type: " ++ show c1
    where getChar :: Get Char
          getChar = (chr . fromIntegral) `liftM` getWord8
          char :: Char -> Get ()
          char c = getChar >>= \c' ->
                   if c == c'
                   then return ()
                   else fail $ "expected " ++ show c
          takeWhile :: (Char -> Bool) -> Get SB8.ByteString
          takeWhile p = do buf <- lookAhead $ remaining >>= getByteString
                           let bufLen = SB8.length buf
                               len = run 0
                               run i | i >= bufLen = bufLen
                                     | p (SB8.index buf i) = run $ i + 1
                                     | otherwise = i
                           getByteString len
          manyTill :: Get a -> Char -> Get [a]
          manyTill e c = lookAhead getChar >>= \c' ->
                         if c == c'
                         then getChar >> return []
                         else do el <- e
                                 (el:) `liftM` manyTill e c

parseFile :: FilePath -> IO (Maybe BValue)
parseFile = return . result . decode <=< SB8.readFile
    where result (Left _) = Nothing
          result (Right a) = Just a

bdictLookup :: BValue -> String -> Maybe BValue
bdictLookup (BDict dict) key = lookup (BString $ B8.pack key) dict
bdictLookup _ _ = Nothing

infoHash :: BValue -> IO (Maybe SW8.ByteString)
infoHash metaInfo
    = maybe (return Nothing) (return . Just <=< sha1) $
      encode `liftM` (metaInfo `bdictLookup` "info")
    where sha1 :: B8.ByteString -> IO SW8.ByteString
          sha1 bs = SW8.pack `liftM` digest SHA1 bs

{-
instance Arbitrary Char where
  arbitrary = chr `fmap` oneof [choose (0,127), choose (0,255)]-}
instance Arbitrary B8.ByteString where
    arbitrary = B8.pack `fmap`  arbitrary
instance Arbitrary SB8.ByteString where
    arbitrary = SB8.pack `fmap`  arbitrary
instance Arbitrary BValue where
    arbitrary = frequency [(10, BInteger `liftM` arbitrary),
                           (5, resize 150 $ BString `liftM` arbitrary),
                           (2, resize 5 $ BList `liftM` arbitrary),
                           (2, resize 5 $ BDict `liftM` arbitrary)]

propEncodeDecode val = decode (SB8.concat $ B8.toChunks $ encode val) == Right val
