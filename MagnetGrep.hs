module MagnetGrep where

import qualified Network.HTTP as HTTP
import Network.URI (parseURI)
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)


grep url
    = do let uri = fromMaybe undefined $
                   parseURI url
             req = HTTP.mkRequest HTTP.GET uri
             req' = req { HTTP.rqHeaders = filter (\header ->
                                                       not $
                                                           HTTP.hdrName header `elem` [HTTP.HdrUserAgent,
                                                                                       HTTP.HdrContentLength]
                                                  ) $ HTTP.rqHeaders req
                        }
         Right rsp <- HTTP.simpleHTTP req'
         return $ grepInfoHashes $ HTTP.rspBody rsp

grepInfoHashes :: String -> [String]
grepInfoHashes ""
    = []
grepInfoHashes s
    = case stripPrefix "magnet:?xt=urn:btih:" s of
        Just s' ->
            let (infoHash, s'') = break (== '&') s'
            in infoHash : grepInfoHashes s''
        Nothing ->
            grepInfoHashes $ tail s

