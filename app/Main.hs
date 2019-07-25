{-# LANGUAGE OverloadedStrings #-}

import Application (createInitialState, application, MutableServerState)

import Network.WebSockets.Snap
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

site :: MutableServerState -> Snap ()
site state = route
  [ ("/api", serveDirectory "static")
  , ("/static", serveApi state)
  ]

serveApi :: MutableServerState -> Snap ()
serveApi state = runWebSocketsSnap $ application state

main :: IO ()
main = do
    state <- createInitialState

    quickHttpServe $ site state
