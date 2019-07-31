{-# LANGUAGE OverloadedStrings #-}

import Application (createInitialState, application, MutableServerState)

import Network.HTTP.Types (status400)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (defaultTlsSettings, runTLS)
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
    let localSettings = setPort 9160 $ defaultSettings
    runTLS defaultTlsSettings localSettings $ websocketsOr' WS.defaultConnectionOptions state backupApplication
