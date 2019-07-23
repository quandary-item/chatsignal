{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Control.Monad.Catch (finally)
import Control.Monad.Reader (liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isPunctuation, isSpace)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Responses (ConnectionNotify(..), ServerStateResponse(..), ServerMessage(..), OfferSDPResponse(..), SendICEResponse(..), StartCallResponse(..), AcceptCallResponse(..), RejectCallResponse(..), sendSingle, sendBroadcast)
import ServerState (ServerState, Client(..), lookupClientById, clientExistsWithUsername, removeClient, addClient)
import UserID (UserID, makeRandomUserID)
import Util (assertM, dupe)
import WebRTC (SDPData, ICECandidate)


class Request a where
  parseObject :: Object -> (Parser a)
  validate :: a -> ServerState -> Either String ()

newtype WrappedFromJSON a = WrapFromJSON { unwrapFromJSON :: a }

instance (Request a) => FromJSON (WrappedFromJSON a) where
  parseJSON = withObject "request" $ \o -> do
    fmap WrapFromJSON $ parseObject o

class Performable a b | a -> b where
  perform :: b -> a -> MVar ServerState -> IO ()


ingestData :: (Request r) => BL.ByteString -> ServerState -> Either String r
ingestData msg clients = do
  -- decode the request
  command <- eitherDecode $ msg
  let unwrappedCommand = unwrapFromJSON command
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
    Just client -> do
      s <- modifyMVar state $ pure . dupe . (removeClient userId')
      sendBroadcast (ServerMessage $ username client `mappend` " disconnected") s


connectClient :: Client -> ServerState -> IO ServerState
connectClient client clients = do
  -- Send a welcome / motd
  sendSingle (ServerMessage "Welcome to One Hour Chat!") (connection client)
  -- Tell the client what their user id is
  sendSingle (ConnectionNotify (userId client)) (connection client)

  -- Add the new client to the state
  let newClients = addClient client clients

  -- Notify everyone that the party has officially started
  sendBroadcast (ServerMessage $ username client `mappend` " joined") clients
  sendBroadcast (ServerStateResponse newClients) newClients

  -- return the updated list of clients
  pure newClients

    
data ConnectRequestData = Connect T.Text deriving Show

isValidUsername :: T.Text -> Bool
isValidUsername providedUsername = not $ or (map ($ providedUsername) [T.null, T.any isPunctuation, T.any isSpace])

invalidUsernameErrorMessage :: String
invalidUsernameErrorMessage = "Username cannot contain punctuation or whitespace, and cannot be empty"

usernameIsTakenErrorMessage :: String
usernameIsTakenErrorMessage = "Username is already taken by an existing user"


instance Request ConnectRequestData where
  parseObject o = Connect <$> o .: "username"
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


data Ping = Ping UserID
data Say = Say T.Text
data OfferSDPRequest = OfferSDPRequest UserID SDPData
data SendICECandidate = SendICECandidate UserID ICECandidate
data StartCall = StartCall UserID
data AcceptCall = AcceptCall UserID
data RejectCall = RejectCall UserID


instance Request Ping where
  parseObject o = Ping <$> o .: "target"
  validate _ _ = pure ()

instance Performable Ping (Client, ServerState) where
  perform (client, clients) (Ping targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendBroadcast (ServerMessage $ username client `mappend` " pinged " `mappend` username targetUser) clients


instance Request Say where
  parseObject o = Say <$> o .: "message"
  validate _ _ = pure ()

instance Performable Say (Client, ServerState) where
  perform (client, clients) (Say message) _ = do
    sendBroadcast (ServerMessage $ username client `mappend` ": " `mappend` message) clients


instance Request OfferSDPRequest where
  parseObject o = OfferSDPRequest <$> o .: "to" <*> o .: "sdp"
  validate _ _ = pure ()

instance Performable OfferSDPRequest (Client, ServerState) where
  perform (client, clients) (OfferSDPRequest targetId sdp) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (OfferSDPResponse (userId client) sdp) (connection targetUser)

instance Request SendICECandidate where
  parseObject o = SendICECandidate <$> o .: "to" <*> o .: "ice"
  validate _ _ = pure ()

instance Performable SendICECandidate (Client, ServerState) where
  perform (client, clients) (SendICECandidate targetId ice) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (SendICEResponse (userId client) ice) (connection targetUser)

instance Request StartCall where
  parseObject o = StartCall <$> o .: "to"
  validate _ _ = pure ()

instance Performable StartCall (Client, ServerState) where
  perform (client, clients) (StartCall targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (StartCallResponse (userId client)) (connection targetUser)

instance Request AcceptCall where
  parseObject o = AcceptCall <$> o .: "to"
  validate _ _ = pure ()

instance Performable AcceptCall (Client, ServerState) where
  perform (client, clients) (AcceptCall targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (AcceptCallResponse (userId client)) (connection targetUser)

instance Request RejectCall where
  parseObject o = RejectCall <$> o .: "to"
  validate _ _ = pure ()

instance Performable RejectCall (Client, ServerState) where
  perform (client, clients) (RejectCall targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> sendSingle (RejectCallResponse (userId client)) (connection targetUser)
