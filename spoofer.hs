module Main where

import qualified Node

main = do node <- Node.new 9999
          forkIO $ Node.run node
