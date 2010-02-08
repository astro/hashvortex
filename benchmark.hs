module Main where

import Control.Monad.State
import System.Random
import Data.Char (chr)
import Control.DeepSeq
import System.CPUTime (getCPUTime)
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString as SW8
import qualified BEncoding as B
import KRPC
import NodeId (nodeIdToBuf)

instance NFData SW8.ByteString where
    rnf a = {-# SCC "forceStrictByteString" #-} a `seq` ()

type Randomized a = State [Char] a


randomBytes :: Int -> Randomized B8.ByteString
randomBytes n = do rnds <- get
                   put $ drop n rnds
                   return $ B8.pack $ take n rnds

mkRequest :: Randomized SW8.ByteString
mkRequest = do t <- randomBytes 4
               id <- randomBytes 20
               target <- randomBytes 20
               return $ {-# SCC "concat" #-} SW8.concat $
                      B8.toChunks $
                      {-# SCC "encode" #-} B.encode $
                      B.bdict [("t", B.BString t),
                               ("y", B.BString $ B8.pack "q"),
                               ("q", B.BString $ B8.pack "find_node"),
                               ("a", B.bdict [("id", B.BString id),
                                              ("target", B.BString target)])]

runRandomT :: Randomized a -> IO a
runRandomT f = getStdGen >>= return . evalState f . randomRs ('\0', '\xFF')


main = do t1 <- getCPUTime
          let n = 100000
          pkts <- runRandomT (mapM (const mkRequest) [1..n])
          t2 <- {-# SCC "pkts" #-} pkts `deepseq` getCPUTime
          putStrLn $ show (length pkts) ++ " pkts generated, rate: " ++
                       (show $ fromInteger n * 1000000000000 / fromInteger (t2 - t1)) ++ " Hz"
          t3 <- getCPUTime
          let allOk = all (\pkt ->
                               case {-# SCC "decodePacket" #-} decodePacket pkt of
                                 Right (QPacket (T t) (FindNode id target)) ->
                                     B8.length t == 4 &&
                                     B8.length (nodeIdToBuf id) == 20 &&
                                     B8.length (nodeIdToBuf target) == 20
                                 _ -> False
                          ) pkts
          t4 <- {-# SCC "allOk" #-} allOk `seq` getCPUTime
          putStrLn $ "allOk=" ++ show allOk ++ ", rate: " ++
                       (show $ fromInteger n * 1000000000000 / fromInteger (t4 - t3)) ++ " Hz"
