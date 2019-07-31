{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Control.Monad.Catch (finally)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isPunctuation, isSpace)
import qualified Data.Text as T
import GHC.Generics
import qualified Network.WebSockets as WS

import Responses (ConnectionNotify(..), ServerStateResponse(..), ServerMessage(..), OfferSDPResponse(..), SendICEResponse(..), StartCallResponse(..), AcceptCallResponse(..), RejectCallResponse(..), sendSingle, sendBroadcast)
import ServerState (ServerState, Client(..), lookupClientById, clientExistsWithUsername, removeClient, addClient, clientList)
import UserID (UserID, makeRandomUserID)
import Util (assertM, dupe)
import WebRTC (SDPData, ICECandidate)


class Request a where
  validate :: a -> ServerState -> Either String ()

class Performable a b | a -> b where
  perform :: b -> a -> MVar ServerState -> IO ()


ingestData :: (FromJSON r, Request r) => BL.ByteString -> ServerState -> Either String r
ingestData msg clients = do
  -- decode the request
  command <- eitherDecode $ msg
  let unwrappedCommand = command
  -- validate the request data
  validate unwrappedCommand clients
  -- if successful, return the command
  pure unwrappedCommand


disconnect :: UserID -> MVar ServerState -> IO ()
disconnect userId' state = do
  putStrLn "disconnect"
  currentState <- readMVar state
  putStrLn $ show currentState

  case lookupClientById userId' currentState of
    Nothing -> return ()
    Just (Client { username = clientUsername }) -> do
      s <- modifyMVar state $ pure . dupe . (removeClient userId')
      sendBroadcast (ServerMessage $ clientUsername `mappend` " disconnected") s

      
connectClient :: Client -> ServerState -> IO ServerState
connectClient (client@Client { userId = clientUserId, connection = clientConnection, username = clientUsername }) clients = do
  -- Send a welcome / motd
  sendSingle (ServerMessage "Welcome to One Hour Chat!") clientConnection
  -- Tell the client what their user id is
  sendSingle (ConnectionNotify clientUserId) clientConnection

  -- Add the new client to the state
  let newClients = addClient client clients

  -- Notify everyone that the party has officially started
  sendBroadcast (ServerMessage $ clientUsername `mappend` " joined") clients
  sendBroadcast (ServerStateResponse $ clientList newClients) newClients

  -- return the updated list of clients
  pure newClients

    
data ConnectRequestData = Connect { username :: T.Text } deriving (FromJSON, Generic)

isValidUsername :: T.Text -> Bool
isValidUsername providedUsername = not $ or (map ($ providedUsername) [T.null, T.any isPunctuation, T.any isSpace])

invalidUsernameErrorMessage :: String
invalidUsernameErrorMessage = "Username cannot contain punctuation or whitespace, and cannot be empty"

usernameIsTakenErrorMessage :: String
usernameIsTakenErrorMessage = "Username is already taken by an existing user"


instance Request ConnectRequestData where
  validate (Connect providedUsername) clients = do
    assertM invalidUsernameErrorMessage $ isValidUsername providedUsername
    assertM usernameIsTakenErrorMessage $ not $ clientExistsWithUsername providedUsername clients

instance Performable ConnectRequestData (WS.Connection, Client -> MVar ServerState -> IO ()) where
  perform (conn, handler) (Connect providedUsername) state = do
    putStrLn $ T.unpack providedUsername
    -- Create a user id
    newUserId <- makeRandomUserID
    -- Create the actual client
    let client = Client { username = providedUsername, userId = newUserId, connection = conn }

    flip finally (disconnect (newUserId) state) $ do
      -- Connect the client by updating the state
      modifyMVar_ state $ connectClient client
      forever $ (handler client state)


data Ping = Ping { target :: UserID } deriving (FromJSON, Generic)
data Say = Say { message :: T.Text } deriving (FromJSON, Generic)
data OfferSDPRequest = OfferSDPRequest { to :: UserID, sdp :: SDPData } deriving (FromJSON, Generic)
data SendICECandidate = SendICECandidate { to :: UserID, ice ::  ICECandidate } deriving (FromJSON, Generic)
data StartCall = StartCall { to :: UserID } deriving (FromJSON, Generic)
data AcceptCall = AcceptCall { to :: UserID } deriving (FromJSON, Generic)
data RejectCall = RejectCall { to :: UserID } deriving (FromJSON, Generic)


instance Request Ping where
  validate _ _ = pure ()

instance Performable Ping (Client, ServerState) where
  perform ((Client { username = clientUsername }), clients) (Ping targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just (Client { username = targetUsername }) -> sendBroadcast (ServerMessage $ clientUsername `mappend` " pinged " `mappend` targetUsername) clients


instance Request Say where
  validate _ _ = pure ()

instance Performable Say (Client, ServerState) where
  perform ((Client { username = clientUsername }), clients) (Say message) _ = do
    sendBroadcast (ServerMessage $ clientUsername `mappend` ": " `mappend` message) clients


instance Request OfferSDPRequest where
  validate _ _ = pure ()

instance Performable OfferSDPRequest (Client, ServerState) where
  perform (client, clients) (OfferSDPRequest targetId sdp) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (OfferSDPResponse (userId client) sdp) (connection targetUser)

instance Request SendICECandidate where
  validate _ _ = pure ()

instance Performable SendICECandidate (Client, ServerState) where
  perform (client, clients) (SendICECandidate targetId ice) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (SendICEResponse (userId client) ice) (connection targetUser)

instance Request StartCall where
  validate _ _ = pure ()

instance Performable StartCall (Client, ServerState) where
  perform (client, clients) (StartCall targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (StartCallResponse (userId client)) (connection targetUser)

instance Request AcceptCall where
  validate _ _ = pure ()

instance Performable AcceptCall (Client, ServerState) where
  perform (client, clients) (AcceptCall targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (AcceptCallResponse (userId client)) (connection targetUser)

instance Request RejectCall where
  validate _ _ = pure ()

instance Performable RejectCall (Client, ServerState) where
  perform (client, clients) (RejectCall targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (RejectCallResponse (userId client)) (connection targetUser)
