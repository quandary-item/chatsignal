{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module RequestLang where

import Control.Monad(forM_)
import Control.Monad.Free
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, UTCTime)
import qualified Network.WebSockets as WS

import UserID(UserID)
import Requests(Ping(..), Say(..), OfferSDPRequest(..), SendICECandidate(..), StartCall(..), AcceptCall(..), RejectCall(..))
import Responses(Response(..), ServerMessage(..), OfferSDPResponse(..), SendICEResponse(..), StartCallResponse(..), AcceptCallResponse(..), RejectCallResponse(..))
import ServerState(Client(..), ServerState, connection, lookupClientById)
import WebRTC (ICECandidate, SDPData)

data DoThingF next
  = SendSingle BL.ByteString next
  | SendSingleTo BL.ByteString Client next
  | SendBroadcast BL.ByteString next
  | GetTime (UTCTime -> next)
  | GetClient (Client -> next)
  | GetState (ServerState -> next)
  | WriteLog String next
  | Done
  deriving Functor

type DoThing = Free DoThingF


infixr 0 ~>
type f ~> g = forall x. f x -> g x


runDoThing :: (Client, ServerState) -> DoThing a -> IO ()
runDoThing (client, clients) (Free (SendSingle msg next)) = do
  WS.sendTextData (connection client) msg
  runDoThing (client, clients) next
runDoThing (client, clients) (Free (SendSingleTo msg targetUser next)) = do
  WS.sendTextData (connection targetUser) msg
  runDoThing (client, clients) next
runDoThing (client, clients) (Free (SendBroadcast msg next)) = do
  forM_ clients $ \client -> WS.sendTextData (connection client) msg
  runDoThing (client, clients) next
runDoThing (client, clients) (Free (GetTime f)) = do
  t <- getCurrentTime
  runDoThing (client, clients) (f t)
runDoThing (client, clients) (Free (GetClient f)) = do
  runDoThing (client, clients) (f client)
runDoThing (client, clients) (Free (GetState f)) = do
  runDoThing (client, clients) (f clients)
runDoThing (client, clients) (Free (WriteLog msg next)) = do
  putStrLn msg
  runDoThing (client, clients) next
runDoThing (client, clients) (Free Done) = do
  pure ()

getClient :: DoThing Client
getClient = liftF (GetClient id)

getState :: DoThing ServerState
getState = liftF (GetState id)

getTime :: DoThing UTCTime
getTime = liftF (GetTime id)

--sendBroadcast :: BL.ByteString -> DoThing a
sendBroadcast message = liftF $ SendBroadcast message id
sendSingle message = liftF $ SendSingle message id
sendSingleTo message targetUser = liftF $ SendSingleTo message targetUser id

done :: DoThing a
done = liftF Done


doPing :: Ping -> Free DoThingF a
doPing Ping { target = targetId } = do
  Client { username = clientUsername } <- getClient
  clients <- getState

  case lookupClientById targetId clients of
      Just (Client { username = targetUsername }) -> do
        now <- getTime
        let response = ServerMessage (clientUsername `mappend` " pinged " `mappend` targetUsername) now

        let messageContent = encode $ Response { kind = "ping", content = response} 
        sendBroadcast messageContent        
        done


doSay :: Say -> Free DoThingF a
doSay Say { message = message } = do
  Client { username = clientUsername } <- getClient
  now <- getTime
  let response = ServerMessage (clientUsername `mappend` ": " `mappend` message) now
  sendBroadcast $ encode $ Response { kind = "", content = response }
  done


doOfferSdpRequest :: OfferSDPRequest -> Free DoThingF a
doOfferSdpRequest OfferSDPRequest { to = targetId, sdp = sdp } = do
  client <- getClient
  clients <- getState
  case lookupClientById targetId clients of
    Just targetUser -> do
      sendSingleTo (encode $ Response { kind = "offer", content = OfferSDPResponse (userId client) sdp}) targetUser
      done
    Nothing -> done


doSendIceCandidate :: SendICECandidate -> Free DoThingF a
doSendIceCandidate SendICECandidate { to = targetId, ice = targetIce } = do
  client <- getClient
  clients <- getState
  case lookupClientById targetId clients of
      Just targetUser -> do
        sendSingleTo (encode $ Response { kind = "ice", content = (SendICEResponse (userId client) targetIce) }) targetUser
        done
      Nothing -> done

doStartCall :: StartCall -> Free DoThingF a
doStartCall StartCall { to = targetId } = do
  client <- getClient
  clients <- getState
  case lookupClientById targetId clients of
    Just targetUser -> do
      sendSingleTo (encode $ Response { kind = "startcall", content = StartCallResponse (userId client) }) targetUser
      done
    Nothing -> done
  
doAcceptCall :: AcceptCall -> Free DoThingF a
doAcceptCall AcceptCall { to = targetId } = do
  client <- getClient
  clients <- getState
  case lookupClientById targetId clients of
      Just targetUser -> do
        sendSingleTo (encode $ Response { kind = "acceptcall", content = AcceptCallResponse (userId client)}) targetUser
        done
      Nothing -> done

doRejectCall :: RejectCall -> Free DoThingF a
doRejectCall RejectCall { to = targetId } = do
  client <- getClient
  clients <- getState
  case lookupClientById targetId clients of
    Just targetUser -> do
      sendSingleTo (encode $ Response { kind = "rejectcall", content = RejectCallResponse (userId client) }) targetUser
      done
    Nothing -> done

