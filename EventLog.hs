module EventLog (Logger, newLog) where

import Data.IORef
import System.IO
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Control.Monad.State hiding (State)
import Data.List (intercalate)
import Data.Array.Unboxed
import qualified Network.Libev as Ev

import KRPC

type Events = UArray Int Int
type Logger = Query -> IO ()


type Time = POSIXTime
data LoggerState = State { stNow :: Integer,
                           stEvents :: Events
                         }
type LoggerAction a = StateT LoggerState IO a



newLog :: Ev.EvLoopPtr -> FilePath -> IO Logger
newLog evLoop logPath
    = do f <- openFile logPath AppendMode
         hSetBuffering f LineBuffering

         stRef <- newIORef $ State 0 newEvents
         setInterval evLoop 1 $ writer f stRef
         return $ logger stRef

logger :: IORef LoggerState -> Logger
logger stRef query
    = do st <- readIORef stRef
         let events = stEvents st
             events' = incEvent query events
         events' `seq`
                 writeIORef stRef $ st { stEvents = events' }

writer :: Handle -> IORef LoggerState -> IO ()
writer f stRef
    = do st <- readIORef stRef
         hPutStrLn f $ intercalate " "
                       (show (stNow st) :
                        map show (elems $ stEvents st))
         writeIORef stRef $ st { stNow = stNow st + 1,
                                 stEvents = newEvents }

newEvents :: Events
newEvents = array (1, 4) [(n, 0) | n <- [1..4]]

incEvent :: Query -> Events -> Events
incEvent query events
    = events // [(i, (events ! i) + 1)]
    where i = case query of
                Ping _ -> 1
                FindNode _ _ -> 2
                GetPeers _ _ -> 3
                AnnouncePeer _ _ _ _ -> 4

setInterval :: Ev.EvLoopPtr -> Ev.EvTimestamp
            -> IO () -> IO ()
setInterval evLoop interval handler
    = do evTimer <- Ev.mkEvTimer
         evCb <- Ev.mkTimerCallback $ \evLoop evTimer evType ->
                 handler
         Ev.evTimerInit evTimer evCb interval interval
         Ev.evTimerStart evLoop evTimer
