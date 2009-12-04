{-# LANGUAGE FlexibleInstances #-}
module Node where

import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Char8 as SB8
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent.Chan

import KRPC


data NodeState = State { stSock :: Socket,
                         stQueryHandler :: QueryHandler,
                         stLastT :: T
                       }
type Node = MVar NodeState
type QueryHandler = Query -> IO (Either Error Reply)

class InState a where
    inState :: a -> Node -> IO ()

instance InState (NodeState -> IO NodeState) where
    inState f node = modifyMVar_ node f

instance InState (NodeState -> IO ()) where
    inState f node = withMVar node f

instance InState (NodeState -> NodeState) where
    inState f node = modifyMVar_ node $ return . f

new :: Int -> IO Node
new port = do sock <- socket AF_INET Datagram defaultProtocol
              bindSocket sock (SockAddrInet (fromIntegral port) 0)
              newMVar $ State { stSock = sock,
                                stQueryHandler = nullQueryHandler
                              }

nullQueryHandler _ = return $ Left $ Error 201 $ B8.pack "Not implemented"

setQueryHandler :: QueryHandler -> Node -> IO ()
setQueryHandler cb = inState $ \st -> st { stQueryHandler = cb }

run :: Node -> IO ()
run node = runOnce node >>
           run node

runOnce :: Node -> IO ()
runOnce = inState $ \st ->
          do (buf, addr) <- recvFrom (stSock st) 65535
             catch (handlePacket st (B8.fromChunks [buf]) addr) $ \e ->
                 do putStrLn $ "Error handling packet: " ++ show e
                    return st


handlePacket :: NodeState -> B8.ByteString -> SockAddr -> IO NodeState
handlePacket st buf addr
    = do putStrLn $ "received " ++ (show $ B8.length buf) ++ " from " ++ show addr
         let pkt = decodePacket buf
         putStrLn $ "pkt = " ++ show pkt
         return st

sendQuery :: SockAddr -> Query -> Node -> IO Reply
sendQuery addr qry
    = do inState $ \st ->
             do let t' = stLastT st `tPlus` 1
                    buf = SB8.concat $ B8.toChunks $ encodePacket $ QPacket t' qry
                sendTo (stSock st) buf addr
                return st { stLastT = t' }
