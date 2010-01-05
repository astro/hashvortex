module EventLog (Logger, newLog) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.IO
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Control.Monad (when)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import Control.Monad.State hiding (State)
import Text.Printf (printf)
import Data.List (intercalate)

import KRPC


type Logger = Query -> IO ()
data Event = Event { evTime :: POSIXTime,
                     evQuery :: String
                   }
data LoggerState = State { stStart :: POSIXTime,
                           stNow :: POSIXTime,
                           stEvents :: Seq Event
                         }

type Data = [Int]
data TimeData = TimeData POSIXTime Data
instance Show TimeData where
    show (TimeData time datas)
        = intercalate " " $
          (printf "%.2f " (realToFrac time :: Double)):(map show datas)


newLog :: FilePath -> IO Logger
newLog logPath =
    do chan <- newChan
       forkIO $ writer logPath chan
       let sender query =
               do now <- getPOSIXTime
                  let q = case query of
                            Ping _ -> "Ping"
                            FindNode _ _ -> "FindNode"
                            GetPeers _ _ -> "GetPeers"
                            AnnouncePeer _ _ _ _ -> "AnnouncePeer"
                  writeChan chan $ Event now q
       return sender

writer :: FilePath -> Chan Event -> IO ()
writer logPath chan = withFile logPath AppendMode $ \f ->
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
                      in getPOSIXTime >>= \now ->
                          evalStateT loop $ State now now Seq.empty

interval = 0.1

updateEvents :: Event -> StateT LoggerState IO [TimeData]
updateEvents event@(Event now _)
    = do st <- get
         put $ st { stEvents = stEvents st |> event }

         let datas = do st <- get
                        case stNow st + interval < now - 0.5 of
                          True ->
                              do dropUntilNow
                                 data' <- (TimeData now) `liftM` countEvents
                                 liftM (data':) $
                                       do put $ st { stNow = stNow st + interval }
                                          datas
                          False ->
                              return []
         datas

dropUntilNow :: StateT LoggerState IO ()
dropUntilNow = do st <- get
                  let now = stNow st
                      events = Seq.dropWhileL (\(Event time _) ->
                                                   time <= now - 0.5
                                              ) $ stEvents st
                  put $ st { stEvents = events }

countEvents :: StateT LoggerState IO Data
countEvents = do st <- get
                 let now = stNow st
                     events = Seq.dropWhileL (\(Event time _) ->
                                                  time <= now + 0.5
                                             ) $ stEvents st
                     count s = Seq.length $
                               Seq.filter (\event ->
                                               evQuery event == s
                                          ) events
                     data' = [count "Ping",
                              count "FindNode",
                              count "GetPeers",
                              count "AnnouncePeer"]
                 return data'
