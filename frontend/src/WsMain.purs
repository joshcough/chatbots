module WsMain where

import Prelude

import ChatBot.IrcMsg (RawIrcMsg(..))
import ChatBot.IrcMsg as Irc
import ChatBot.Models (ChatMessage(..))
import Concurrent.BoundedQueue as BQ
import Data.Argonaut (Json, class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Var (($=), get)
import Effect.Console (log)
import WebSocket (Connection(..), Message(..), URL(..), runMessageEvent, runMessage, runURL, newWebSocket)

main :: BQ.BoundedQueue ChatMessage -> Effect Unit
main q = do
  Connection socket <- newWebSocket (URL "ws://localhost:8081/chatbot/stream/artofthetroll") []

  socket.onopen $= \event -> do
    log "onopen: Connection opened"
    log <<< runURL =<< get socket.url

  socket.onmessage $= \event -> do
    let received = runMessage (runMessageEvent event)
    log $ "onmessage: Received '" <> received <> "'"
    case jsonParser received >>= decodeJson of
      Left err -> log "error"
      Right cm -> launchAff_ $ BQ.write q cm

  socket.onclose $= \event -> do
    log "onclose: Connection closed"

--    socket.send (Message "hello")
--    when (received == "goodbye") do
--      log "onmessage: closing connection"
--      socket.close


main2 :: BQ.BoundedQueue RawIrcMsg -> Effect Unit
main2 q = do
  Connection socket <- newWebSocket (URL "ws://irc-ws.chat.twitch.tv/") []

  let f s = log (Irc.renderRawIrcMsg s) >>= \_ -> socket.send (stringy s)

  socket.onopen $= \event -> do
    log "onopen: Connection opened"
    log <<< runURL =<< get socket.url
    _ <- f $ Irc.ircPass $ "oauth:jgiestm46pvmn3iiefhtlv9ix7y1ma"
    _ <- f $ Irc.ircNick $ "trollabot_test"
    _ <- f $ Irc.ircCapReq ["twitch.tv/membership"]
    _ <- f $ Irc.ircCapReq ["twitch.tv/commands"]
    _ <- f $ Irc.ircCapReq ["twitch.tv/tags"]
    _ <- f $ Irc.ircPing ["ping"]
    _ <- f $ Irc.ircJoin "#artofthetroll" Nothing
    pure unit

  socket.onmessage $= \event -> do
    let received = runMessage (runMessageEvent event)
    log $ "onmessage: Received '" <> received <> "'"
    -- TODO: have to parse the irc message, can't decode json, makes no sense.
    -- case Irc.parseMessage received of -- or whatever
    --   Left err -> log "error"
    --   Right msg -> do
    --     log $ show msg
    --     launchAff_ $ BQ.write q msg

  socket.onclose $= \event -> do
    log "onclose: Connection closed"

  where
  stringy = Message <<< Irc.renderRawIrcMsg
