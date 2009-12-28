module EventLog (Logger, newLog) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.IO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (when)


type Logger = String -> IO ()

newLog :: FilePath -> IO Logger
newLog logPath = do chan <- newChan
                    forkIO $ writer logPath chan
                    let sender s = do now <- getPOSIXTime
                                      writeChan chan (show now ++ " " ++ s)
                    return sender

writer :: FilePath -> Chan String -> IO ()
writer logPath chan = withFile logPath AppendMode $ \f ->
                      let idleFlush = do empty <- isEmptyChan chan
                                         when empty $
                                              hFlush f
                          loop = do s <- readChan chan
                                    hPutStrLn f s

                                    idleFlush
                                    loop
                      in loop
