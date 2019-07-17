{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Aeson
import Data.Char (isPunctuation, isSpace)
import qualified Data.Map.Strict as Map
import Data.Monoid (mappend)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad (forM_, forever)
import Control.Monad.Catch (finally)
import Control.Monad.Except (liftEither, MonadError)
import Control.Monad.Identity (Identity)
import Control.Monad.Writer.Strict
import Control.Concurrent (MVar, newMVar, modifyMVar, readMVar)
import qualified Data.Text as T
import Control.Monad.Reader

import qualified Network.WebSockets as WS

import UserID (UserID, makeRandomUserID)

type Validation = ReaderT ServerState (Either String)


assert :: String -> Bool -> Either String ()
assert _       True  = Right ()
assert message False = Left message

assertM :: (MonadError String m) => String -> Bool -> m ()
assertM message = liftEither . (assert message)


class Action r where
  validate :: r -> Validation ()


data ConnectRequestData = Connect T.Text deriving Show

instance Action ConnectRequestData where
  validate (Connect providedUsername) = do
    clients <- ask

    assertM invalidUsernameErrorMessage $ isValidUsername providedUsername
    assertM usernameIsTakenErrorMessage $ not $ clientExistsWithUsername providedUsername clients

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

instance Action RequestData where
  validate _ = pure ()


data Response = Response ResponseData | Broadcast ResponseData

sendResponse' :: Client -> ServerState -> Response -> IO ()
sendResponse' client _       (Response r ) = sendResponse (connection client) r
sendResponse' _      clients (Broadcast r) = broadcast clients r


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

broadcast :: ServerState -> ResponseData -> IO ()
broadcast clients responseData = do
    BL.putStrLn message
    forM_ clients $ \client -> WS.sendTextData (connection client) message
    where message = encode responseData


isValidUsername :: T.Text -> Bool
isValidUsername providedUsername = not $ or (map ($ providedUsername) [T.null, T.any isPunctuation, T.any isSpace])

invalidUsernameErrorMessage :: String
invalidUsernameErrorMessage = "Username cannot contain punctuation or whitespace, and cannot be empty"

usernameIsTakenErrorMessage :: String
usernameIsTakenErrorMessage = "Username is already taken by an existing user"


sendResponse :: WS.Connection -> ResponseData -> IO ()
sendResponse conn responseData = WS.sendTextData conn (encode responseData)


disconnect :: UserID -> MVar ServerState -> IO ()
disconnect userId' state = do
  currentState <- readMVar state

  case Map.lookup userId' currentState of
    Nothing -> return ()
    Just client -> do
      s <- modifyMVar state $ \s -> do
        let s' = removeClient userId' s
        return (s', s')
      broadcast s $ ServerMessage $ username client `mappend` " disconnected"


performRequestData :: (Monad m) => RequestData -> Client -> ServerState -> WriterT [Response] m ()
performRequestData (Ping targetId) client clients = do
  case Map.lookup targetId clients of
    Nothing         -> return ()
    Just targetUser -> tell [Broadcast $ ServerMessage $ username client `mappend` " pinged " `mappend` username targetUser]
performRequestData (Say message) client _ = do
  tell [Broadcast $ ServerMessage $ username client `mappend` ": " `mappend` message]


talk :: Client -> MVar ServerState -> IO ()
talk client state = do
  msg <- WS.receiveData (connection client)

  clients <- readMVar state

  let responses = case runReaderT (ingestData msg) clients of
        Left errorMsg -> [Response $ ServerMessage $ T.pack errorMsg]
        Right command -> execWriter (performRequestData command client clients)

  -- send the response/broadcasts
  mapM_ (sendResponse' client clients) responses


performConnectRequestData :: ConnectRequestData -> WS.Connection -> MVar ServerState -> IO ()
performConnectRequestData (Connect providedUsername) conn state = do
  -- Create a user id
  newUserId <- makeRandomUserID
  -- Create the actual client
  let client = Client { username = providedUsername, userId = newUserId, connection = conn }

  (serveConnection client state) `finally` (disconnect (newUserId) state)


serveConnection :: Client -> MVar ServerState -> IO ()
serveConnection client state = do
  -- Connect the client while collecting response messages
  responses <- modifyMVar state $ \clients ->
    (pure . runWriter) $ connectClient client clients

  -- Send the responses to the clients/peers
  newClients <- readMVar state
  let respond = sendResponse' client newClients
  mapM_ respond responses

  -- Serve subsequent requests for this client
  forever $ talk client state


ingestData :: (Action r, FromJSON r) => BL.ByteString -> Validation r
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
    BL.putStrLn msg

    case runReaderT (ingestData msg) clients of
      Left errorMsg -> sendResponse conn $ ServerMessage $ T.pack errorMsg
      Right command -> performConnectRequestData command conn state


connectClient :: Client -> ServerState -> WriterT [Response] Identity ServerState
connectClient client clients = do
  -- Send a welcome / motd
  tell [Response $ ServerMessage "Welcome to One Hour Chat!"]
  -- Tell the client what their user id is
  tell [Response $ ConnectionNotify (userId client)]

  -- Add the new client to the state
  let newClients = addClient client clients

  -- Notify everyone that the party has officially started
  tell [Broadcast $ ServerMessage $ username client `mappend` " joined"]
  tell [Broadcast $  ServerStateResponse newClients]

  -- return the updated list of clients
  pure newClients


main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state
