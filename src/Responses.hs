{-# LANGUAGE OverloadedStrings #-}

module Responses where

import Control.Monad (forM_)
import Data.Aeson
import UserID (UserID)
import ServerState (ServerState, clientIdList, clientList, connection)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import WebRTC(ICECandidate, SDPData)

newtype WrappedToJSON a = WrapToJSON { unwrapToJSON :: a }

class JSONResponse a where
  kind :: a -> T.Text
  toValue :: KeyValue b => a -> [b]

instance (JSONResponse a) => ToJSON (WrappedToJSON a) where
  toJSON (WrapToJSON responseData) = object
    $ [ "kind" .= kind responseData ] ++ toValue responseData

data ServerStateResponse = ServerStateResponse ServerState
data ServerMessage = ServerMessage T.Text
data ConnectionNotify = ConnectionNotify UserID
data OfferSDPResponse = OfferSDPResponse UserID SDPData
data SendICEResponse = SendICEResponse UserID ICECandidate
data StartCallResponse = StartCallResponse UserID
data AcceptCallResponse = AcceptCallResponse UserID
data RejectCallResponse = RejectCallResponse UserID
data BannedResponse = BannedResponse


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

instance Show BannedResponse where
  show (BannedResponse) = "User is banned"

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

instance JSONResponse BannedResponse where
  kind _ = "banned"
  toValue (BannedResponse) = []


-- Methods for returning responses

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
