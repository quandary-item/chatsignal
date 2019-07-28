{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application (createInitialState, application, MutableServerState) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Concurrent (MVar, newMVar, readMVar)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import ServerState (ServerState, Client(..), connection, newServerState)

import Responses(sendSingle, ServerMessage(..))
import Requests(Ping, Say, OfferSDPRequest, SendICECandidate, StartCall, AcceptCall, RejectCall, ConnectRequestData, perform, ingestData)

type MutableServerState = MVar ServerState

data SelectedAction = Action T.Text
instance FromJSON SelectedAction where
  parseJSON = withObject "request" $ \o -> do
    Action <$> o .: "action"


serveConnection :: Client -> MVar ServerState -> IO ()
serveConnection client state = do
    msg <- WS.receiveData $ (connection client)

    clients <- readMVar $ state

    onFail (flip logError $ connection client) $ do
      (Action action) <- getAction msg

      case action of
        "ping" -> do
          command <- ingestData msg clients
          pure $ perform (client, clients) (command :: Ping) state
        "say" -> do
          command <- ingestData msg clients
          pure $ perform (client, clients) (command :: Say) state
        "offer" -> do
          command <- ingestData msg clients
          pure $ perform (client, clients) (command :: OfferSDPRequest) state
        "ice" -> do
          command <- ingestData msg clients
          pure $ perform (client, clients) (command :: SendICECandidate) state
        "startcall" -> do
          command <- ingestData msg clients
          pure $ perform (client, clients) (command :: StartCall) state
        "acceptcall" -> do
          command <- ingestData msg clients
          pure $ perform (client, clients) (command :: AcceptCall) state
        "rejectcall" -> do
          command <- ingestData msg clients
          pure $ perform (client, clients) (command :: RejectCall) state
        _ -> Left $ unknownActionErrorMsg ++ (T.unpack action)


getAction :: BL.ByteString -> Either String SelectedAction
getAction = eitherDecode


logError :: String -> WS.Connection -> IO ()
logError errorMsg = sendSingle (ServerMessage $ T.pack errorMsg)


onFail :: (Monad m) => (String -> m ()) -> Either String (m ()) -> m ()
onFail logMessage f = case f of
  Left errorMsg -> logMessage errorMsg
  Right monad -> monad

unknownActionErrorMsg :: String
unknownActionErrorMsg = "Unrecognised action: "

createInitialState :: IO (MutableServerState)
createInitialState = newMVar newServerState

application :: BL.ByteString -> MVar ServerState -> WS.ServerApp
application addr state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    msg <- WS.receiveData conn

    clients <- readMVar state

    onFail (flip logError $ conn) $ do
      (Action action) <- getAction msg
      case action of
        "connect" -> do
          command <- ingestData msg clients :: Either String ConnectRequestData
          pure $ perform (conn, serveConnection) command state
        _ -> Left $ unknownActionErrorMsg ++ (T.unpack action)
