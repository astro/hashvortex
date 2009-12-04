module Main where

import Control.Concurrent

import qualified Node
import KRPC

main = do node <- Node.new 9999
          forkIO $ Node.run node
          a:_ <- Node.getAddrs "router.bittorrent.com" "6881"
          r <- Node.sendQuery a (Ping $ NodeId $ fromIntegral 0) node
          putStrLn $ "received: " ++ show r
