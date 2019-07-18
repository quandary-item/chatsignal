{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Aeson
import Data.Char (isPunctuation, isSpace)
import qualified Data.Map.Strict as Map
import Data.Monoid (mappend)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad (forM_, forever)
import Control.Monad.Catch (finally)
import Control.Monad.Except (liftEither)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import qualified Data.Text as T
import Control.Monad.Reader
import qualified Network.WebSockets as WS
import Data.ByteString.UTF8 (toString)

import UserID (UserID, makeRandomUserID)
import Util (assertM, dupe)

type Validation = ReaderT ServerState (Either String)


class Validatable r where
  validate :: r -> Validation ()

class Performable r a | r -> a where
  perform :: r -> MVar ServerState -> ReaderT a IO ()


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


data RequestData = Ping UserID | Say T.Text deriving Show

instance FromJSON RequestData where
  parseJSON = withObject "ping or say" $ \o -> do
    action <- o .: "action"
    case action of
      "ping"       -> Ping <$> o .: "target"
      "say"        -> Say  <$> o .: "message"
      _            -> fail ("unknown action " ++ action)

instance Validatable RequestData where
  validate _ = pure ()

instance Performable RequestData Client where  
  perform (Ping targetId) state = do
    client <- ask
    clients <- liftIO $ readMVar state
    case Map.lookup targetId clients of
      Nothing         -> return ()
      Just targetUser -> liftIO $ sendResponse $ Broadcast (ServerMessage $ username client `mappend` " pinged " `mappend` username targetUser) clients
  perform (Say message) state = do
    client <- ask
    clients <- liftIO $ readMVar state
    liftIO $ sendResponse $ Broadcast (ServerMessage $ username client `mappend` ": " `mappend` message) clients


data Response = Response ResponseData WS.Connection | Broadcast ResponseData ServerState

instance Show Response where
  show (Response responseData _) = "Response: " ++ show responseData
  show (Broadcast responseData _) = "Broadcast: " ++ show responseData

sendResponse :: Response -> IO ()
sendResponse (Response  r conn   ) = sendSingleResponse conn r
sendResponse (Broadcast r clients) = sendBroadcastResponse clients r

sendSingleResponse :: WS.Connection -> ResponseData -> IO ()
sendSingleResponse conn responseData = do
  putStrLn $ "response: " ++ (toString $ BL.toStrict message)
  WS.sendTextData conn message
  where message = encode responseData

sendBroadcastResponse :: ServerState -> ResponseData -> IO ()
sendBroadcastResponse clients responseData = do
  putStrLn $ "broadcast: " ++ (toString $ BL.toStrict message)
  forM_ clients $ \client -> WS.sendTextData (connection client) message
  where message = encode responseData


data ResponseData = ServerStateResponse ServerState | ServerMessage T.Text | ConnectionNotify UserID
instance Show ResponseData where
  show (ServerStateResponse clients) = "Clients: " ++ (T.unpack $ T.intercalate ", " $ map (T.pack . show . fst) $ Map.toList clients)
  show (ServerMessage text)          = "Message: " ++ T.unpack text
  show (ConnectionNotify userId')    = "Notify: "  ++ show userId'
instance ToJSON ResponseData where
  toJSON (ServerStateResponse clients) = object [ "kind"    .= ("clients" :: T.Text)
                                                , "clients" .= map snd (Map.toList clients)
                                                ]
  toJSON (ServerMessage text)          = object [ "kind" .= ("message" :: T.Text)
                                                , "data" .= text
                                                ]
  toJSON (ConnectionNotify userId')    = object [ "kind"    .= ("notify" :: T.Text)
                                                , "user_id" .= show userId'
                                                ]

data Client = Client { username :: T.Text, userId :: UserID, connection :: WS.Connection }

instance Show Client where
  show (Client { username = username', userId = userId' }) = "Client: " ++ show (username', userId')

instance ToJSON Client where
  toJSON client = object [ "username" .= username client
                         , "id"       .= show (userId client)
                         ]

type ServerState = Map.Map UserID Client

newServerState :: ServerState
newServerState = Map.empty

numClients :: ServerState -> Int
numClients = Map.size

clientExistsWithUsername :: T.Text -> ServerState -> Bool
clientExistsWithUsername username' serverState = (Map.size matchingClients) > 0
  where matchingClients = Map.filter ((== username') . username) serverState

addClient :: Client -> ServerState -> ServerState
addClient client = Map.insert (userId client) client

removeClient :: UserID -> ServerState -> ServerState
removeClient = Map.delete


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

  case Map.lookup userId' currentState of
    Nothing -> return ()
    Just client -> do
      s <- modifyMVar state $ pure . dupe . (removeClient userId')
      sendResponse $ Broadcast (ServerMessage $ username client `mappend` " disconnected") s


connectClient :: Client -> ServerState -> IO ServerState
connectClient client clients = do
  -- Send a welcome / motd
  sendResponse $ Response (ServerMessage "Welcome to One Hour Chat!") (connection client)
  -- Tell the client what their user id is
  sendResponse $ Response (ConnectionNotify (userId client)) (connection client)

  -- Add the new client to the state
  let newClients = addClient client clients

  -- Notify everyone that the party has officially started
  sendResponse $ Broadcast (ServerMessage $ username client `mappend` " joined") clients
  sendResponse $ Broadcast (ServerStateResponse newClients) newClients

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
      Left errorMsg -> sendResponse $ Response (ServerMessage $ T.pack errorMsg) (connection client)
      Right command -> runReaderT (perform (command :: RequestData) state) client


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
      Left errorMsg -> sendResponse $ Response (ServerMessage $ T.pack errorMsg) conn
      Right command -> runReaderT (perform (command :: ConnectRequestData) state) conn


main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state
