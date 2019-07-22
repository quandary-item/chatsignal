{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Aeson
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad (forM_, forever)
import Control.Monad.Catch (finally)
import Control.Monad.Except (liftEither)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import qualified Data.Text as T
import Control.Monad.Reader
import qualified Network.WebSockets as WS

import ServerState (ServerState, Client(..), userId, connection, username, addClient, removeClient, newServerState, clientExistsWithUsername, clientIdList, clientList, lookupClientById)
import UserID (UserID, makeRandomUserID)
import Util (assertM, dupe)

type Validation = ReaderT ServerState (Either String)


class Validatable r where
  validate :: r -> Validation ()

class Performable r a | r -> a where
  perform :: r -> MVar ServerState -> ReaderT a IO ()


sendSingle :: (JSONResponse a, Show a) => a -> WS.Connection -> IO ()
sendSingle responseData conn = do
  putStrLn $ "Response: " ++ show responseData
  WS.sendTextData conn message
    where message = encode $ WrapToJSON responseData

sendBroadcast :: (JSONResponse a, Show a) => a -> ServerState -> IO ()
sendBroadcast responseData clients = do
  putStrLn $ "Broadcast: " ++ show responseData
  forM_ clients $ \client -> WS.sendTextData (connection client) message
    where message = encode $ WrapToJSON responseData


data ConnectRequestData = Connect T.Text deriving Show

instance Validatable ConnectRequestData where
  validate (Connect providedUsername) = do
    clients <- ask

    assertM invalidUsernameErrorMessage $ isValidUsername providedUsername
    assertM usernameIsTakenErrorMessage $ not $ clientExistsWithUsername providedUsername clients

instance Performable ConnectRequestData WS.Connection where
  perform (Connect providedUsername) state = do
    conn <- ask
    -- Create a user id
    newUserId <- liftIO $ makeRandomUserID
    -- Create the actual client
    let client = Client { username = providedUsername, userId = newUserId, connection = conn }

    (liftIO $ serveConnection client state) `finally` (liftIO $ disconnect (newUserId) state)

instance FromJSON ConnectRequestData where
  parseJSON = withObject "connect" $ \o -> do
    action <- o .: "action"
    case action of
      "connect"    -> Connect <$> o .: "username"
      _            -> fail ("unknown action " ++ action)

type SDPData = T.Text
type ICECandidate = T.Text
data RequestData = Ping UserID
                 | Say T.Text
                 | OfferSDPRequest UserID SDPData
                 | SendICECandidate UserID ICECandidate
                 | StartCall UserID
                 | AcceptCall UserID
                 | RejectCall UserID deriving Show

instance FromJSON RequestData where
  parseJSON = withObject "ping or say or offersdprequest or sendicecandidate or startcall or acceptcall or rejectcall" $ \o -> do
    action <- o .: "action"
    case action of
      "ping"  -> Ping            <$> o .: "target"
      "say"   -> Say             <$> o .: "message"
      "offer" -> OfferSDPRequest <$> o .: "to" <*> o .: "sdp"
      "ice"   -> SendICECandidate <$> o .: "to" <*> o .: "ice"
      "startcall" -> StartCall <$> o .: "to"
      "acceptcall" -> AcceptCall <$> o .: "to"
      "rejectcall" -> RejectCall <$> o .: "to"
      _       -> fail ("unknown action " ++ action)

instance Validatable RequestData where
  validate _ = pure ()


instance Performable RequestData (Client, ServerState) where
  perform (Ping targetId) _ = do
    (client, clients) <- ask
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendBroadcast (ServerMessage $ username client `mappend` " pinged " `mappend` username targetUser) clients

  perform (Say message) _ = do
    (client, clients) <- ask
    liftIO $ sendBroadcast (ServerMessage $ username client `mappend` ": " `mappend` message) clients

  perform (OfferSDPRequest targetId sdp) _ = do
    (client, clients) <- ask
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendSingle (OfferSDPResponse (userId client) sdp) (connection targetUser)

  perform (SendICECandidate targetId ice) _ = do
    (client, clients) <- ask
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendSingle (SendICEResponse (userId client) ice) (connection targetUser)

  perform (StartCall targetId) _ = do
    (client, clients) <- ask
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendSingle (StartCallResponse (userId client)) (connection targetUser)

  perform (AcceptCall targetId) _ = do
    (client, clients) <- ask
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendSingle (AcceptCallResponse (userId client)) (connection targetUser)

  perform (RejectCall targetId) _ = do
    (client, clients) <- ask
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendSingle (RejectCallResponse (userId client)) (connection targetUser)


data ServerStateResponse = ServerStateResponse ServerState
data ServerMessage = ServerMessage T.Text
data ConnectionNotify = ConnectionNotify UserID
data OfferSDPResponse = OfferSDPResponse UserID SDPData
data SendICEResponse = SendICEResponse UserID ICECandidate
data StartCallResponse = StartCallResponse UserID
data AcceptCallResponse = AcceptCallResponse UserID
data RejectCallResponse = RejectCallResponse UserID

instance Show ServerStateResponse where
  show (ServerStateResponse clients) = "Clients: " ++ (T.unpack $ T.intercalate ", " $ map (T.pack . show) $ clientIdList clients)

instance Show ServerMessage where
  show (ServerMessage text)          = "Message: " ++ T.unpack text

instance Show ConnectionNotify where
  show (ConnectionNotify userId')    = "Notify: "  ++ show userId'

instance Show OfferSDPResponse where
  show (OfferSDPResponse fromId _)   = "Offer SDP Response from " ++ show fromId

instance Show SendICEResponse where
  show (SendICEResponse  fromId iceData)   = "Send ICE Candidate '" ++ (show iceData) ++ "' from " ++ show fromId

instance Show StartCallResponse where
  show (StartCallResponse fromId)    = "Start Call from " ++ show fromId

instance Show AcceptCallResponse where
  show (AcceptCallResponse fromId)   = "Accept Call from " ++ show fromId

instance Show RejectCallResponse where
  show (RejectCallResponse fromId)   = "Reject Call from " ++ show fromId


class JSONResponse a where
  kind :: a -> T.Text
  toValue :: KeyValue b => a -> [b]

instance JSONResponse ServerStateResponse where
  kind _ = "clients"
  toValue (ServerStateResponse clients)     = [ "clients" .= clientList clients ]
instance JSONResponse ServerMessage where
  kind _ = "message"
  toValue (ServerMessage text)              = [ "data" .= text ]

instance JSONResponse ConnectionNotify where
  kind _ = "notify"
  toValue (ConnectionNotify userId')        = [ "user_id" .= show userId' ]

instance JSONResponse OfferSDPResponse where
  kind _ = "offer"
  toValue (OfferSDPResponse fromId sdpData) = [ "from" .= show fromId, "sdp"  .= sdpData ]

instance JSONResponse SendICEResponse where
  kind _ = "ice"
  toValue (SendICEResponse fromId iceData)  = [ "from" .= show fromId, "ice"  .= iceData ]

instance JSONResponse StartCallResponse where
  kind _ = "startcall"
  toValue (StartCallResponse fromId)        = [ "from" .= show fromId ]

instance JSONResponse AcceptCallResponse where
  kind _ = "acceptcall"
  toValue (AcceptCallResponse fromId)        = [ "from" .= show fromId ]

instance JSONResponse RejectCallResponse where
  kind _ = "rejectcall"
  toValue (RejectCallResponse fromId)        = [ "from" .= show fromId ]

newtype WrappedToJSON a = WrapToJSON { unwrapToJSON :: a }

instance (JSONResponse a) => ToJSON (WrappedToJSON a) where
  toJSON (WrapToJSON responseData) = object $ [ "kind" .= kind responseData ] ++ toValue responseData


isValidUsername :: T.Text -> Bool
isValidUsername providedUsername = not $ or (map ($ providedUsername) [T.null, T.any isPunctuation, T.any isSpace])

invalidUsernameErrorMessage :: String
invalidUsernameErrorMessage = "Username cannot contain punctuation or whitespace, and cannot be empty"

usernameIsTakenErrorMessage :: String
usernameIsTakenErrorMessage = "Username is already taken by an existing user"


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


serveConnection :: Client -> MVar ServerState -> IO ()
serveConnection client state = do
  -- Connect the client by updating the state
  modifyMVar_ state $ connectClient client

  -- Serve subsequent requests for this client
  forever $ do
    msg <- WS.receiveData $ (connection client)

    clients <- readMVar $ state

    case runReaderT (ingestData msg) clients of
      Left errorMsg -> sendSingle (ServerMessage $ T.pack errorMsg) (connection client)
      Right command -> runReaderT (perform (command :: RequestData) state) (client, clients)


ingestData :: (Validatable r, FromJSON r) => BL.ByteString -> Validation r
ingestData msg = do
  -- decode the request
  command <- liftEither . eitherDecode $ msg
  -- validate the request data
  validate command
  -- if successful, return the command
  pure command


application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    msg <- WS.receiveData conn

    clients <- readMVar state

    case runReaderT (ingestData msg) clients of
      Left errorMsg -> sendSingle (ServerMessage $ T.pack errorMsg) conn
      Right command -> runReaderT (perform (command :: ConnectRequestData) state) conn


main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state
