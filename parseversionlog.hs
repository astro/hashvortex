{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Char8 as SC8
import Data.Object
import qualified Data.Object.Json as JSON
import Control.Applicative
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Monad (mapM)
import qualified Control.Exception as Exception


objToMap :: Object String String -> IO (Map String String)
objToMap obj = do objMapping <- fromMapping obj
                  Map.fromList <$>
                         mapM (\(k, v) ->
                                (k, ) <$> fromScalar v
                              ) objMapping

findAllKeys :: [Map String String] -> [String]
findAllKeys = ("time":) .
              filter (/= "time") .
              Set.toAscList .
              Set.fromList .
              map fst .
              concatMap Map.toList
                   
writeData :: String -> [String] -> [Map String String] -> IO ()
writeData path keys
  = writeFile path .
    unlines .
    map (\map' ->
          intercalate " " $
          map (\key ->
                case Map.lookup key map' of
                  Nothing -> "0"
                  Just val -> val
              ) keys
        )

main = do lines <- SC8.lines <$> SC8.getContents
          maps <- mapM (\line ->
                            Exception.catch (JSON.decode line >>= \m ->
                                             m `seq` objToMap m)
                                         (\(e :: Exception.SomeException) ->
                                              return Map.empty)
                       ) lines
          let keys = findAllKeys maps
          putStrLn $ "# Keys: " ++ show keys
          
          writeData "data" keys maps
          
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
