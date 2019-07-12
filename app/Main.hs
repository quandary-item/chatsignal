{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

type UserID = Int


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
      "say"        -> Say <$> o .: "message"
      _            -> fail ("unknown action " ++ action)


data ResponseData = ServerStateResponse ServerState | ServerMessage Text
instance Show ResponseData where
  show (ServerStateResponse clients) = "Clients: " ++ (T.unpack $ T.intercalate (T.pack ", ") $ map fst clients)
  show (ServerMessage text)          = T.unpack text
instance ToJSON ResponseData where
  toJSON (ServerStateResponse clients) = object ["kind" .= ("clients" :: Text), "clients" .= map fst clients]
  toJSON (ServerMessage text)          = object ["kind" .= ("message" :: Text), "data" .= text]

type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

revMap :: a -> [a -> b] -> [b]
revMap value = map ($ value)

isInvalidUsername :: Text -> Bool
isInvalidUsername username = not $ or (map ($ username) [T.null, T.any isPunctuation, T.any isSpace])

invalidUsernameErrorMessage :: String
invalidUsernameErrorMessage = "Username cannot contain punctuation or whitespace, and cannot be empty"

usernameIsTaken :: Text -> ServerState -> Bool
usernameIsTaken username = any ((== username) . fst)

usernameIsTakenErrorMessage :: String
usernameIsTakenErrorMessage = "Username is already taken by an existing user"

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar state

    let requestDecodeResult = do
          command <- (eitherDecode msg :: Either String ConnectRequestData)
          let (Connect username) = command
          let client = (username, conn)
          assert (not $ isInvalidUsername username) invalidUsernameErrorMessage
          assert (not $ clientExists client clients) usernameIsTakenErrorMessage
          
          pure client
          
    case requestDecodeResult of
      (Left errorMsg) -> WS.sendTextData conn (T.pack errorMsg)
      (Right client)  -> flip finally (disconnect client state) $ do
        -- Send a welcome / motd
        WS.sendTextData conn ("Welcome to One Hour Chat!" :: Text)

        -- Create a new user, broadcast the new user list to everyone else
        modifyMVar_ state $ \s -> do
          let s' = addClient client s
          WS.sendTextData conn $ "Current users: " `mappend` T.intercalate ", " (map fst s)
          broadcast (fst client `mappend` " joined") s'
          return s'

        -- Enter the main loop
        talk client state

disconnect :: Client -> MVar ServerState -> IO ()
disconnect client state = do
  s <- modifyMVar state $ \s -> do
    let s' = removeClient client s
    return (s', s')
  broadcast (fst client `mappend` " disconnected") s


talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  -- Decode the JSON request data
  case (eitherDecode msg :: Either String RequestData) of
    Left errorMsg -> WS.sendTextData conn (T.pack errorMsg)
    Right command -> case command of
      Ping targetId -> broadcast' (user `mappend` "pinged " `mappend` (T.pack $ show targetId))
      Say message   -> broadcast' (user `mappend` ": " `mappend` message)
  where
    -- Convenience method for broadcasting data
    broadcast' m = do
      clients <- readMVar state
      broadcast m clients
