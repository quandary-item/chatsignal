{-# LANGUAGE OverloadedStrings #-}

import Application (createInitialState, application, MutableServerState)

import Network.HTTP.Types (status400)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (defaultTlsSettings, tlsSettings, runTLS)
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import System.Environment


websocketsOr' :: WS.ConnectionOptions
              -> MutableServerState
              -> Wai.Application
              -> Wai.Application
websocketsOr' opts state backup req sendResponse =
  case websocketsApp opts (application (Wai.remoteHost req) state) req of
    Nothing  -> backup req sendResponse
    Just res -> sendResponse res


backupApplication :: Wai.Application
backupApplication _ respond = respond $ Wai.responseLBS status400 [] "Not a WebSocket request"


main :: IO ()
main = do
    args <- getArgs

    state <- createInitialState
    let certFile = args !! 0
    let keyFile = args !! 1
    let localTlsSettings = tlsSettings certFile keyFile
    let localSettings = setPort 9160 $ defaultSettings
    runTLS localTlsSettings localSettings $ websocketsOr' WS.defaultConnectionOptions state backupApplication
