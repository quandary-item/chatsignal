{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad (forM_, forever)
import Control.Monad.Catch (finally)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import qualified Data.Text as T
import Control.Monad.Reader
import qualified Network.WebSockets as WS

import ServerState (ServerState, Client(..), userId, connection, username, addClient, removeClient, newServerState, clientExistsWithUsername, clientIdList, clientList, lookupClientById)
import UserID (UserID, makeRandomUserID)
import Util (assertM, dupe)

data SelectedAction = Action T.Text
instance FromJSON SelectedAction where
  parseJSON = withObject "request" $ \o -> do
    Action <$> o .: "action"

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

instance Request ConnectRequestData where
  parseObject o = Connect <$> o .: "username"
  validate (Connect providedUsername) clients = do
    assertM invalidUsernameErrorMessage $ isValidUsername providedUsername
    assertM usernameIsTakenErrorMessage $ not $ clientExistsWithUsername providedUsername clients

instance Performable ConnectRequestData WS.Connection where
  perform conn (Connect providedUsername) state = do
    -- Create a user id
    newUserId <- liftIO $ makeRandomUserID
    -- Create the actual client
    let client = Client { username = providedUsername, userId = newUserId, connection = conn }
    (liftIO $ serveConnection client state) `finally` (liftIO $ disconnect (newUserId) state)


type SDPData = T.Text
type ICECandidate = T.Text

data Ping = Ping UserID
data Say = Say T.Text
data OfferSDPRequest = OfferSDPRequest UserID SDPData
data SendICECandidate = SendICECandidate UserID ICECandidate
data StartCall = StartCall UserID
data AcceptCall = AcceptCall UserID
data RejectCall = RejectCall UserID

newtype WrappedFromJSON a = WrapFromJSON { unwrapFromJSON :: a }

class Request a where
  parseObject :: Object -> (Parser a)
  validate :: a -> ServerState -> Either String ()

instance (Request a) => FromJSON (WrappedFromJSON a) where
  parseJSON = withObject "request" $ \o -> do
    fmap WrapFromJSON $ parseObject o

instance Request Ping where
  parseObject o = Ping <$> o .: "target"
  validate _ _ = pure ()

doPing :: (Client, ServerState) -> Ping -> MVar ServerState -> IO ()
doPing (client, clients) (Ping targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendBroadcast (ServerMessage $ username client `mappend` " pinged " `mappend` username targetUser) clients


instance Request Say where
  parseObject o = Say <$> o .: "message"
  validate _ _ = pure ()

instance Performable Say (Client, ServerState) where
  perform (client, clients) (Say message) _ = do
    liftIO $ sendBroadcast (ServerMessage $ username client `mappend` ": " `mappend` message) clients


instance Request OfferSDPRequest where
  parseObject o = OfferSDPRequest <$> o .: "to" <*> o .: "sdp"
  validate _ _ = pure ()

instance Performable OfferSDPRequest (Client, ServerState) where
  perform (client, clients) (OfferSDPRequest targetId sdp) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendSingle (OfferSDPResponse (userId client) sdp) (connection targetUser)

instance Request SendICECandidate where
  parseObject o = SendICECandidate <$> o .: "to" <*> o .: "ice"
  validate _ _ = pure ()

instance Performable SendICECandidate (Client, ServerState) where
  perform (client, clients) (SendICECandidate targetId ice) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendSingle (SendICEResponse (userId client) ice) (connection targetUser)

instance Request StartCall where
  parseObject o = StartCall <$> o .: "to"
  validate _ _ = pure ()

instance Performable StartCall (Client, ServerState) where
  perform (client, clients) (StartCall targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendSingle (StartCallResponse (userId client)) (connection targetUser)

instance Request AcceptCall where
  parseObject o = AcceptCall <$> o .: "to"
  validate _ _ = pure ()

instance Performable AcceptCall (Client, ServerState) where
  perform (client, clients) (AcceptCall targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendSingle (AcceptCallResponse (userId client)) (connection targetUser)

instance Request RejectCall where
  parseObject o = RejectCall <$> o .: "to"
  validate _ _ = pure ()

instance Performable RejectCall (Client, ServerState) where
  perform (client, clients) (RejectCall targetId) _ = do
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

class Performable a b | a -> b where
  perform :: b -> a -> MVar ServerState -> IO ()

instance Performable Ping (Client, ServerState) where
  perform (client, clients) (Ping targetId) _ = do
    case lookupClientById targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendBroadcast (ServerMessage $ username client `mappend` " pinged " `mappend` username targetUser) clients

serveConnection :: Client -> MVar ServerState -> IO ()
serveConnection client state = do
  -- Connect the client by updating the state
  modifyMVar_ state $ connectClient client

  -- Serve subsequent requests for this client
  forever $ do
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


ingestData :: (Request r) => BL.ByteString -> ServerState -> Either String r
ingestData msg clients = do
  -- decode the request
  command <- eitherDecode $ msg
  let unwrappedCommand = unwrapFromJSON command
  -- validate the request data
  validate unwrappedCommand clients
  -- if successful, return the command
  pure unwrappedCommand


getAction :: BL.ByteString -> Either String SelectedAction
getAction = eitherDecode


logError :: String -> WS.Connection -> IO ()
logError errorMsg = sendSingle (ServerMessage $ T.pack errorMsg)


onFail :: (Monad m) => (String -> m ()) -> Either String (m ()) -> m ()
onFail logMessage f = case f of
  Left errorMsg -> logMessage errorMsg
  Right _ -> pure ()

unknownActionErrorMsg :: String
unknownActionErrorMsg = "Unrecognised action: "

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    msg <- WS.receiveData conn

    clients <- readMVar state

    onFail (flip logError $ conn) $ do
      (Action action) <- getAction msg
      case action of
        "connect" -> do
          command <- ingestData msg clients :: Either String ConnectRequestData
          pure $ perform conn command state
        _ -> Left $ unknownActionErrorMsg ++ (T.unpack action)

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state
