{-# LANGUAGE OverloadedStrings #-}

import Application (createInitialState, application, MutableServerState)

import Network.HTTP.Types (status400)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, setPort, Settings)
import Network.Wai.Handler.WarpTLS (TLSSettings, tlsSettings, runTLS)
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS


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
    state <- createInitialState
    let keyPath = "something"
    let certPath = "something"
    let defaultTLS = tlsSettings keyPath certPath
    let localSettings = setPort 9160 $ defaultSettings
    runTLS defaultTLS localSettings $ websocketsOr' WS.defaultConnectionOptions state backupApplication
