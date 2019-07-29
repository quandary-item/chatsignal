{-# LANGUAGE OverloadedStrings #-}

import Application (createInitialState, application)

import qualified Data.ByteString.Lazy.Char8 as BL
import Network.WebSockets.Snap
import Snap.Core
import Snap.Http.Server

main :: IO ()
main = do
    state <- createInitialState

    quickHttpServe $ do
      request <- getRequest
      let addr = BL.fromStrict $ rqClientAddr request
      runWebSocketsSnap $ application addr state
