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
import Data.Time.Clock
import GHC.Generics
import qualified Network.WebSockets as WS

import Responses (ConnectionNotify(..), ServerStateResponse(..), ServerMessage(..), sendSingle, sendBroadcast, Response(..))
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
      now <- getCurrentTime
      let messageContent = clientUsername `mappend` " disconnected"
      sendBroadcast (ServerMessage messageContent now) s

      
connectClient :: Client -> ServerState -> IO ServerState
connectClient (client@Client { userId = clientUserId, connection = clientConnection, username = clientUsername }) clients = do
  -- Send a welcome / motd
  now <- getCurrentTime
  sendSingle (ServerMessage "Welcome to One Hour Chat!" now) clientConnection
  -- Tell the client what their user id is
  sendSingle (ConnectionNotify clientUserId) clientConnection

  -- Add the new client to the state
  let newClients = addClient client clients

  -- Notify everyone that the party has officially started
  let joinMessageContent = clientUsername `mappend` " joined"
  sendBroadcast (ServerMessage joinMessageContent now) clients
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

instance Request Say where
  validate _ _ = pure ()

instance Request OfferSDPRequest where
  validate _ _ = pure ()

instance Request SendICECandidate where
  validate _ _ = pure ()

instance Request StartCall where
  validate _ _ = pure ()

instance Request AcceptCall where
  validate _ _ = pure ()

instance Request RejectCall where
  validate _ _ = pure ()
