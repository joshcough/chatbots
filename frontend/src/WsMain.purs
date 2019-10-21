module WsMain where

import Prelude

import ChatBot.Models (ChatMessage(..))
import Concurrent.BoundedQueue as BQ
import Data.Argonaut (Json, class DecodeJson, class EncodeJson, decodeJson, jsonParser)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Var (($=), get)
import Effect.Console (log)
import WebSocket (Connection(..), URL(..), runMessageEvent, runMessage, runURL, newWebSocket)

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