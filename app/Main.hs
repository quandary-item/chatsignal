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

invalidUsernameErrorMessage :: Text
invalidUsernameErrorMessage = "Username cannot contain punctuation or whitespace, and cannot be empty"

usernameIsTaken :: Text -> ServerState -> Bool
usernameIsTaken username = any ((== username) . fst)

usernameIsTakenErrorMessage :: Text
usernameIsTakenErrorMessage = "Username is already taken by an existing user"

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar state
    case (eitherDecode msg :: Either String ConnectRequestData) of
      Left errorMsg -> WS.sendTextData conn (T.pack errorMsg)
      Right (Connect username) -> do
        case checkClient client clients of
          Just validationErrorMessage -> WS.sendTextData conn validationErrorMessage
          Nothing -> flip finally disconnect $ do
            -- send a welcome / motd
            WS.sendTextData conn ("Welcome to One Hour Chat!" :: Text)


            -- create a new user, broadcast the new user list to everyone else
            modifyMVar_ state $ \s -> do
              let s' = addClient client s
              WS.sendTextData conn $ "Current users: " `mappend` T.intercalate ", " (map fst s)
              broadcast (fst client `mappend` " joined") s'
              return s'

            -- enter the main loop
            talk client state

        where
          client = (username, conn)
          disconnect = do
            s <- modifyMVar state $ \s -> do
              let s' = removeClient client s
              return (s', s')
            broadcast (fst client `mappend` " disconnected") s

checkClient :: Client -> ServerState -> Maybe Text
checkClient client@(username, _) clients
  | isInvalidUsername username  = Just (invalidUsernameErrorMessage :: Text)
  | clientExists client clients = Just (usernameIsTakenErrorMessage :: Text)
  | otherwise = Nothing


talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  case (eitherDecode msg :: Either String RequestData) of
    Left errorMsg -> WS.sendTextData conn (T.pack errorMsg)
    Right (Ping targetId) -> readMVar state >>= broadcast (user `mappend` "pinged " `mappend` (T.pack $ show targetId))
    Right (Say message)   -> readMVar state >>= broadcast (user `mappend` ": " `mappend` message)
