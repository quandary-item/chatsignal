{-# LANGUAGE OverloadedStrings #-}

module ServerState where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS

import UserID (UserID)

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
