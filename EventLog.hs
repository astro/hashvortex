module EventLog (Logger, newLog) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.IO
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Control.Monad (when)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State hiding (State)
import Text.Printf (printf)
import Data.List (intercalate)

import KRPC


type Logger = Query -> IO ()
data Event = Event { evTime :: POSIXTime,
                     evQuery :: String
                   }
             deriving (Eq, Ord)
data LoggerState = State { stNow :: POSIXTime,
                           stEvents :: Set Event
                         }

type Data = [Int]
data TimeData = TimeData POSIXTime Data
instance Show TimeData where
    show (TimeData time datas)
        = intercalate " " $
          printf "%.2f " (realToFrac time :: Double):map show datas


newLog :: FilePath -> IO Logger
newLog logPath =
    do chan <- newChan
       forkIO $ writer logPath chan
       start <- getPOSIXTime
       return $ \query ->
           do now <- getPOSIXTime
              let q = case query of
                        Ping _ -> "Ping"
                        FindNode _ _ -> "FindNode"
                        GetPeers _ _ -> "GetPeers"
                        AnnouncePeer _ _ _ _ -> "AnnouncePeer"
              writeChan chan $ Event (now - start) q

writer :: FilePath -> Chan Event -> IO ()
writer logPath chan = withFile logPath WriteMode $ \f ->
                      let idleFlush = do empty <- isEmptyChan chan
                                         when empty $
                                              hFlush f
                          loop = do event <- liftIO $ readChan chan
                                    datas <- updateEvents event
                                    liftIO $
                                           forM_ datas $
                                           hPutStrLn f . show
                                    liftIO idleFlush
                                    loop
                      in evalStateT loop $ State 0.0 Set.empty

interval = 0.1

updateEvents :: Event -> StateT LoggerState IO [TimeData]
updateEvents event@(Event now _)
    = do st <- get
         put $ st { stEvents = event `Set.insert` stEvents st }

         let datas = get >>= \st ->
                     if stNow st + interval < now - 0.5
                     then do dropUntilNow
                             data' <- TimeData now `liftM` countEvents
                                    
                             st <- get
                             put $ st { stNow = stNow st + interval }
                                  
                             liftM (data':) datas
                     else return []
         datas

dropUntilNow :: StateT LoggerState IO ()
dropUntilNow = do st <- get
                  let now = stNow st
                      events = snd $
                               Set.split (Event (now - 0.5) "") $
                               stEvents st
                  put $ st { stEvents = events }

countEvents :: StateT LoggerState IO Data
countEvents = do st <- get
                 let now = stNow st
                     events = fst $
                              Set.split (Event (now + 0.5) "") $
                              stEvents st
                     count s = Set.size $
                               Set.filter (\event ->
                                               evQuery event == s
                                          ) events
                     data' = [count "Ping",
                              count "FindNode",
                              count "GetPeers",
                              count "AnnouncePeer"]
                 return data'
