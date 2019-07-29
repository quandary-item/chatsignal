{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Responses where

import Control.Monad (forM_)
import Data.Aeson
import UserID (UserID)
import ServerState (ServerState, connection)
import qualified Data.Text as T
import GHC.Generics
import qualified Network.WebSockets as WS

import WebRTC(ICECandidate, SDPData)

data Response a = Response { kind :: T.Text, content :: a } deriving (Generic, ToJSON, Show)

class HasKind a where
  kind :: a -> T.Text

data ServerStateResponse = ServerStateResponse { clients :: ServerState } deriving (ToJSON, Generic)
data ServerMessage = ServerMessage { message :: T.Text } deriving (ToJSON, Generic)
data ConnectionNotify = ConnectionNotify { user_id :: UserID } deriving (ToJSON, Generic)
data OfferSDPResponse = OfferSDPResponse { from :: UserID, sdp :: SDPData } deriving (ToJSON, Generic)
data SendICEResponse = SendICEResponse { from :: UserID, ice :: ICECandidate } deriving (ToJSON, Generic)
data StartCallResponse = StartCallResponse { from :: UserID } deriving (ToJSON, Generic)
data AcceptCallResponse = AcceptCallResponse { from :: UserID } deriving (ToJSON, Generic)
data RejectCallResponse = RejectCallResponse { from :: UserID } deriving (ToJSON, Generic)
data BannedResponse = BannedResponse deriving (ToJSON, Generic)

instance HasKind ServerStateResponse where
  kind _ = "clients"

instance HasKind ServerMessage where
  kind _ = "message"

instance HasKind ConnectionNotify where
  kind _ = "notify"

instance HasKind OfferSDPResponse where
  kind _ = "offer"

instance HasKind SendICEResponse where
  kind _ = "ice"

instance HasKind StartCallResponse where
  kind _ = "startcall"

instance HasKind AcceptCallResponse where
  kind _ = "acceptcall"

instance HasKind RejectCallResponse where
  kind _ = "rejectcall"

instance HasKind BannedResponse where
  kind _ = "banned"

-- Methods for returning responses

makeResponse :: (HasKind a) => a -> Response a
makeResponse a = Response { kind = kind a, content = a }

sendSingle :: (HasKind a, Show a, ToJSON a) => a -> WS.Connection -> IO ()
sendSingle a = sendSingle' (makeResponse a)

sendBroadcast :: (HasKind a, Show a, ToJSON a) => a -> ServerState -> IO ()
sendBroadcast a = sendBroadcast' (makeResponse a)

sendSingle' :: (Show a, ToJSON a) => Response a -> WS.Connection -> IO ()
sendSingle' responseData conn = do
  putStrLn $ "Response: " ++ show responseData
  WS.sendTextData conn message'
    where message' = encode $ responseData

sendBroadcast' :: (Show a, ToJSON a) => Response a -> ServerState -> IO ()
sendBroadcast' responseData clients' = do
  putStrLn $ "Broadcast: " ++ show responseData
  forM_ clients' $ \client -> WS.sendTextData (connection client) message'
    where message' = encode $ responseData
