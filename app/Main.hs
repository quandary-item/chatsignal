import Application (createInitialState, application)

import Network.WebSockets.Snap
import Snap.Http.Server

main :: IO ()
main = do
    state <- createInitialState

    quickHttpServe $ runWebSocketsSnap $ application state
