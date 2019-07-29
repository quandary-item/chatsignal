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
import Control.Error.Safe (assertErr)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import BanList (getBanList, addrIsBanned)
import OneHourClub (isOpen)
import ServerState (ServerState, Client(..), connection, newServerState)
import Responses(sendSingle, ServerMessage(..), BannedResponse(..))
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

serveApplication :: BL.ByteString -> MVar ServerState -> WS.Connection -> IO ()
serveApplication addr state conn = do
    msg <- WS.receiveData conn

    clients <- readMVar state

    onFail (flip logError $ conn) $ do
      (Action action) <- getAction msg
      case action of
        "connect" -> do
          command <- ingestData msg clients :: Either String ConnectRequestData
          pure $ perform (conn, serveConnection) command state
        _ -> Left $ unknownActionErrorMsg ++ (T.unpack action)

banListName :: String
banListName = "ban_list.txt"

data ConnectionError = OneHourClubIsClosed | IPIsBanned

isOpenM :: IO (Either ConnectionError ())
isOpenM = do
  isOpen' <- isOpen
  pure $ assertErr OneHourClubIsClosed isOpen'

isBannedM :: BL.ByteString -> IO (Either ConnectionError ())
isBannedM addr = do
  banList <- getBanList banListName
  let isNotBanned = addrIsBanned addr banList
  pure $ assertErr IPIsBanned isNotBanned

application :: BL.ByteString -> MVar ServerState -> WS.ServerApp
application addr state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    connectionStatus <- (isBannedM addr >> isOpenM)
    
    case connectionStatus of
      Left IPIsBanned -> do
        putStrLn $ (BL.unpack addr) ++ " tried to log in but is marked as banned"
        sendSingle (BannedResponse) conn
      Left OneHourClubIsClosed -> do
        putStrLn $ (BL.unpack addr) ++ " tried to log in but is marked as banned"
        -- sendSingle () conn
      Right () -> serveApplication addr state conn
