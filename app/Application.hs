{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application (createInitialState, application, MutableServerState) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Clock
import Control.Concurrent (MVar, newMVar, readMVar)
import qualified Data.Text as T
import Network.Socket (SockAddr)
import qualified Network.WebSockets as WS

import BanList (getBanList, addrIsBanned)
import OneHourClub (isClosed)
import NetUtils (getHostAddress, hostAddressToString)
import ServerState (ServerState, Client(..), connection, newServerState)
import Responses(sendSingle, ServerMessage(..), BannedResponse(..), OneHourClubClosedResponse(..))
import RequestLang(runDoThing, doPing, doSay, doOfferSdpRequest, doSendIceCandidate, doStartCall, doAcceptCall, doRejectCall)
import Requests(Ping(..), Say(..), OfferSDPRequest(..), SendICECandidate(..), StartCall(..), AcceptCall(..), RejectCall(..), ConnectRequestData(..), perform, ingestData)

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
          Ping { target = pingTargetId } <- ingestData msg clients
          let action' = doPing pingTargetId
          pure $ runDoThing (client, clients) action'
        "say" -> do
          Say { message = sayMessage } <- ingestData msg clients
          let action' = doSay sayMessage
          pure $ runDoThing (client, clients) action'
        "offer" -> do
          OfferSDPRequest { to = actionTo, sdp = actionSdp } <- ingestData msg clients
          let action' = doOfferSdpRequest actionTo actionSdp
          pure $ runDoThing (client, clients) action'
        "ice" -> do
          SendICECandidate { to = actionTo, ice = actionIce } <- ingestData msg clients
          let action' = doSendIceCandidate actionTo actionIce
          pure $ runDoThing (client, clients) action'
        "startcall" -> do
          StartCall { to = startCallTo } <- ingestData msg clients
          let action' = doStartCall startCallTo
          pure $ runDoThing (client, clients) action'
        "acceptcall" -> do
          AcceptCall { to = acceptCallTo } <- ingestData msg clients
          let action' = doAcceptCall acceptCallTo
          pure $ runDoThing (client, clients) action'
        "rejectcall" -> do
          RejectCall { to = rejectCallTo } <- ingestData msg clients
          let action' = doRejectCall rejectCallTo
          pure $ runDoThing (client, clients) action'
        _ -> Left $ unknownActionErrorMsg ++ (T.unpack action)


getAction :: BL.ByteString -> Either String SelectedAction
getAction = eitherDecode


logError :: String -> WS.Connection -> IO ()
logError errorMsg conn = do
  now <- getCurrentTime
  sendSingle (ServerMessage (T.pack errorMsg) now ) conn


onFail :: (Monad m) => (String -> m ()) -> Either String (m ()) -> m ()
onFail logMessage f = case f of
  Left errorMsg -> logMessage errorMsg
  Right monad -> monad

unknownActionErrorMsg :: String
unknownActionErrorMsg = "Unrecognised action: "

createInitialState :: IO (MutableServerState)
createInitialState = newMVar newServerState

serveApplication :: String -> MVar ServerState -> WS.Connection -> IO ()
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


application :: SockAddr -> MVar ServerState -> WS.ServerApp
application sockAddr state pending = do
  case getHostAddress sockAddr of
    Just ipv4Address -> application' (hostAddressToString ipv4Address) state pending
    Nothing          -> rejectNoIPv4 pending


rejectNoIPv4 :: WS.ServerApp
rejectNoIPv4 pending = WS.rejectRequest pending "Cannot accept requests from clients that do not have an IPv4 address"


application' :: String -> MVar ServerState -> WS.ServerApp
application' addr state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    putStrLn $ addr

    banList <- getBanList banListName
    isClosed' <- isClosed

    case (addrIsBanned addr banList) of
      True -> do
        putStrLn $ addr ++ " tried to log in but is marked as banned"
        sendSingle (BannedResponse) conn
      False -> do
        case isClosed' of
          True -> do
            putStrLn $ addr ++ " tried to log in, but one hour club is closed"
            sendSingle (OneHourClubClosedResponse) conn
          False -> serveApplication addr state conn
