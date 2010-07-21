module VersionLog (Logger, newLog) where

import Data.IORef
import System.IO
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Control.Monad.State hiding (State)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Libev as Ev
import GHC.Word (Word8)

import KRPC
import Versions
import BEncoding

type Events = Map Version Int
type Logger = BValue -> IO ()


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
logger stRef pkt
    = do st <- readIORef stRef
         let events = stEvents st
             events' = incEvent pkt events
         events' `seq`
                 writeIORef stRef $ st { stEvents = events' }

writer :: Handle -> IORef LoggerState -> IO ()
writer f stRef
    = do st <- readIORef stRef
         let encl = ("{" ++) . (++ "}")
         hPutStrLn f $ encl $ intercalate ", " $
           ("\"time\": " ++ show (stNow st)) :
           map (\(version, amount) ->
                 "\"" ++ show version ++ "\": " ++ show amount
               ) (Map.toList $ stEvents st)
         writeIORef stRef $ st { stNow = stNow st + 1,
                                 stEvents = newEvents
                               }

newEvents :: Events
newEvents = Map.empty

resetEvents :: Events -> Events
resetEvents = Map.map $ const 0

incEvent :: BValue -> Events -> Events
incEvent pkt
    = let version = parseKRPCVersion pkt
          force r = r `seq` r
      in force $
         Map.alter (Just . maybe 1 (+ 1)) version

setInterval :: Ev.EvLoopPtr -> Ev.EvTimestamp
            -> IO () -> IO ()
setInterval evLoop interval handler
    = do evTimer <- Ev.mkEvTimer
         evCb <- Ev.mkTimerCallback $ \evLoop evTimer evType ->
                 handler
         Ev.evTimerInit evTimer evCb interval interval
         Ev.evTimerStart evLoop evTimer
