import Node (getAddrs)
import System.Environment (getArgs)
import Network.Socket (SockAddr(SockAddrInet))
import Data.List (nub)
import Control.Monad (liftM)

main = getArgs >>=
       mapM_ (\arg ->
                  liftM nub (getAddrs arg "21") >>=
                        mapM_ (\(SockAddrInet port ip) ->
                                   putStrLn $ "SockAddrInet " ++ show port ++ " " ++ show ip
                              )
             )

