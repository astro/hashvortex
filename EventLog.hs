module EventLog (Logger, newLog) where

import Data.IORef
import System.IO
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Control.Monad.State hiding (State)
import Data.List (intercalate)
import Data.Array.Unboxed

import KRPC


type Logger = Query -> IO ()
type Events = UArray Int Int
type Time = POSIXTime
data LoggerState = State { stInterval :: Time,
                           stNow :: Time,
                           stEvents :: Events
                         }
type LoggerAction a = StateT LoggerState IO a



newLog :: Double -> FilePath -> IO Logger
newLog interval logPath
    = do f <- openFile logPath AppendMode
         hSetBuffering f LineBuffering

         start <- getPOSIXTime
         stRef <- newIORef $ State (realToFrac interval) start newEvents
         return $ writer f stRef

writer :: Handle -> IORef LoggerState -> Logger
writer f stRef query
    =  getPOSIXTime >>= \now ->

       readIORef stRef >>=
       execStateT (writeUntilNow f now
                   >>
                   incEvent query) >>=
       writeIORef stRef

writeUntilNow :: Handle -> Time -> LoggerAction ()
writeUntilNow f now
    = get >>= \st ->
      let nextInterval = stNow st + stInterval st
      in if nextInterval <= now
         then -- One interval step
              writeEvents f >>
              get >>= \st ->
              put (st { stNow = nextInterval }) >>
              -- Loop
              writeUntilNow f now
      else return ()

writeEvents :: Handle -> LoggerAction ()
writeEvents f
    = do st <- get
         liftIO $ hPutStrLn f $ intercalate " " $ init (show $ stNow st) : map show (elems $ stEvents st)
         put $ st { stEvents = newEvents }

newEvents :: Events
newEvents = array (1, 4) [(n, 0) | n <- [1..4]]

incEvent :: Query -> LoggerAction ()
incEvent query
    = do st <- get
         let events = stEvents st
             events' = events // [(i, (events ! i) + 1)]
         events' `seq`
                 put $ st { stEvents = events' }
    where i = case query of
                Ping _ -> 1
                FindNode _ _ -> 2
                GetPeers _ _ -> 3
                AnnouncePeer _ _ _ _ -> 4
