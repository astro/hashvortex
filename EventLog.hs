module EventLog (Logger, newLog) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.IO


type Logger = String -> IO ()

newLog :: FilePath -> IO Logger
newLog logPath = do chan <- newChan
                    forkIO $ writer logPath chan
                    let sender = writeChan chan
                    return sender

writer :: FilePath -> Chan String -> IO ()
writer logPath chan = withFile logPath AppendMode $ \f ->
                      let loop = do s <- readChan chan
                                    hPutStrLn f s
                                    loop
                      in loop
