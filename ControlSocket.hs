module ControlSocket where

import Data.IORef
import Control.Monad.State.Lazy
import qualified System.Event as Ev
import Network.Socket

import InState


type ControlHandler = [String] -> IO String

listenSocket :: Ev.EventManager -> FilePath -> ControlHandler -> IO ()
listenSocket mgr path handler
    = do serv <- socket AF_UNIX Stream defaultProtocol
         bindSocket serv (SockAddrUnix path)
         listen serv 0
         Ev.registerFd mgr (acceptClient mgr serv handler) (fromIntegral $ fdSocket serv) Ev.evtRead
         return ()

data Client = Context ControlHandler Socket String

acceptClient mgr serv handler _key _ev
    = do (sock, _) <- accept serv
         refCtx <- newIORef $ Context handler sock ""
         let f _key _ev = refInStateT refCtx $ readClient
         Ev.registerFd mgr f (fromIntegral $ fdSocket serv) Ev.evtRead
         return ()

readClient :: StateT Client IO ()
readClient
    = do Context handler sock buf <- get
         buf' <- liftIO $ recv sock 1
         let buf'' = buf ++ buf'
             breaks = (`elem` "\r\n")
         case break breaks buf'' of
           (line, c:rest)
             | breaks c ->
                 do liftIO $ handler $ words line
                    put $ Context handler sock rest
           _ ->
               put $ Context handler sock buf''