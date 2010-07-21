module Main where

import qualified Data.ByteString.Char8 as SC8
import Data.Object
import qualified Data.Object.Json as JSON
import Control.Applicative
import qualified Data.Set as Set
import Data.List (intercalate)
import Control.Monad (forM)


findAllKeys :: [Object String String] -> IO [String]
findAllKeys objs = ("time":) <$>
                   filter (/= "time") <$>
                   Set.toAscList <$>
                   Set.fromAscList <$>
                   map fst <$> 
                   concat <$> 
                   mapM fromMapping objs
                   
writeData :: String -> [String] -> [Object String String] -> IO ()
writeData path keys objs
  = unlines <$>
    mapM (\obj ->
           fromMapping obj >>= \mapping ->
           return $
           intercalate " " $
           map (\key ->
                 case lookupScalar key mapping of
                   Nothing -> "0"
                   Just val -> val
               ) keys
         ) objs >>=
    writeFile path

main = do lines <- SC8.lines <$> SC8.getContents
          objs <- mapM JSON.decode lines
          keys <- findAllKeys objs
          
          writeData "data" keys objs
          
          putStrLn "set xdata time"
          putStrLn "set timefmt \"%s\""
          putStrLn "set xlabel \"Time\""
          putStrLn "set ylabel \"Pkt/s\""
          putStrLn "set samples 10000"
          putStrLn $
            "plot " ++ (intercalate ", " $
                        map (\(key, i) ->
                              "'data' using 1:" ++ show i ++
                              " title '" ++ key ++
                              "' with lines"
                            ) $
                        zip (tail keys) [2..])
