{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import Data.Char (isPunctuation, isSpace)
import qualified Data.Map.Strict as Map
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar, readMVar)
import qualified Data.Text as T

import qualified Network.WebSockets as WS

type UserID = UUID


assert :: Bool -> String -> Either String ()
assert True _  = Right ()
assert False m = Left m


data ConnectRequestData = Connect Text deriving Show
instance FromJSON ConnectRequestData where
  parseJSON = withObject "connect" $ \o -> do
    action <- o .: "action"
    case action of
      "connect"    -> Connect <$> o .: "username"
      _            -> fail ("unknown action " ++ action)


data RequestData = Ping UserID | Say Text deriving Show
instance FromJSON RequestData where
  parseJSON = withObject "ping or say" $ \o -> do
    action <- o .: "action"
    case action of
      "ping"       -> Ping <$> o .: "target"
      "say"        -> Say  <$> o .: "message"
      _            -> fail ("unknown action " ++ action)


data ResponseData = ServerStateResponse ServerState | ServerMessage Text | ConnectionNotify UserID
instance Show ResponseData where
  show (ServerStateResponse clients) = "Clients: " ++ (T.unpack $ T.intercalate ", " $ map (T.pack . show . fst) $ Map.toList clients)
  show (ServerMessage text)          = "Message: " ++ T.unpack text
  show (ConnectionNotify userId')    = "Notify: "  ++ show userId'
instance ToJSON ResponseData where
  toJSON (ServerStateResponse clients) = object [ "kind"    .= ("clients" :: Text)
                                                , "clients" .= Map.toList clients
                                                ]
  toJSON (ServerMessage text)          = object [ "kind" .= ("message" :: Text)
                                                , "data" .= text
                                                ]
  toJSON (ConnectionNotify userId')    = object [ "kind"    .= ("notify" :: Text)
                                                , "user_id" .= show userId'
                                                ]

data Client = Client { username :: Text, userId :: UserID, connection :: WS.Connection }

instance ToJSON Client where
  toJSON client = object [ "username" .= username client
                         , "id"       .= show (userId client)
                         ]

type ServerState = Map.Map UserID Client

newServerState :: ServerState
newServerState = Map.empty

numClients :: ServerState -> Int
numClients = Map.size

clientExistsWithUsername :: Text -> ServerState -> Bool
clientExistsWithUsername username' serverState = (Map.size matchingClients) > 0
  where matchingClients = Map.filter ((== username') . username) serverState

addClient :: UserID -> Client -> ServerState -> ServerState
addClient = Map.insert

removeClient :: UserID -> ServerState -> ServerState
removeClient = Map.delete

broadcast :: ServerState -> ResponseData -> IO ()
broadcast clients responseData = do
    BL.putStrLn message
    forM_ clients $ \client -> WS.sendTextData (connection client) message
    where message = encode responseData


isValidUsername :: Text -> Bool
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

  case (Map.lookup userId' currentState) of
    Nothing -> return ()
    Just client -> do
      s <- modifyMVar state $ \s -> do
        let s' = removeClient userId' s
        return (s', s')
      broadcast s $ ServerMessage $ username client `mappend` " disconnected"


talk :: Client -> MVar ServerState -> IO ()
talk client state = forever $ do
  msg <- WS.receiveData (connection client)

  -- use either monad wahoo
  let requestDecodeResult = do
        command <- (eitherDecode msg :: Either String RequestData)
        pure command

  -- Decode the JSON request data
  case requestDecodeResult of
    Left errorMsg -> sendResponse (connection client) $ ServerMessage $ T.pack errorMsg
    Right command -> case command of
      Ping targetId -> broadcast' $ ServerMessage $ username client `mappend` " pinged " `mappend` (T.pack $ show targetId)
      Say message   -> broadcast' $ ServerMessage $ username client `mappend` ": " `mappend` message
  where
    -- Convenience method for broadcasting data
    broadcast' m = do
      clients <- readMVar state
      broadcast clients m


application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar state
    BL.putStrLn msg

    -- Create a user id
    newUserId <- nextRandom

    let requestDecodeResult = do
          command <- (eitherDecode msg :: Either String ConnectRequestData)

          let (Connect providedUsername) = command
          assert (isValidUsername providedUsername) invalidUsernameErrorMessage

          assert (not $ clientExistsWithUsername providedUsername clients) usernameIsTakenErrorMessage

          pure (Client {username = providedUsername, userId = newUserId, connection = conn})

    case requestDecodeResult of
      (Left errorMsg) -> sendResponse conn $ ServerMessage $ T.pack errorMsg
      (Right client)  -> flip finally (disconnect newUserId state) $ do
        -- Send a welcome / motd
        sendResponse conn $ ServerMessage "Welcome to One Hour Chat!"
        -- Tell the client what their user id is
        sendResponse conn $ ConnectionNotify newUserId

        -- Add the new client to the state
        newClients <- modifyMVar state $ \s -> do
          let s' = addClient newUserId client s
          return (s', s')

        -- Notify everyone that the party has officially started
        broadcast newClients $ ServerMessage $ username client `mappend` " joined"
        broadcast newClients $ ServerStateResponse newClients

        -- Enter the main loop
        talk client state


main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state
