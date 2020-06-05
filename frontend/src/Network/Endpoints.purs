module Network.Endpoints
    ( getCommands
    , getQuotes
    , getRandomQuote
    , getStreams
    , loginToken
    , createUser
    ) where

import Prelude
import Auth.Models (CreateUser, Login)
import ChatBot.Models (ChannelName(..), Command, Quote)
import Network.HTTP (Method(..), buildReq, httpJSON, jsonData, noData)
import Types (OpM)
import Control.Monad.Reader.Trans (ask)
import Data.Maybe (Maybe, fromMaybe)
import Affjax.RequestBody as Request
import Data.Argonaut.Decode (class DecodeJson)

getStreams :: OpM (Array ChannelName)
getStreams = doPost "/chatbot/streams" noData

getQuotes :: ChannelName -> OpM (Array Quote)
getQuotes (ChannelName c) = doPost ("/chatbot/quotes/" <> c._unChannelName) noData

getRandomQuote :: ChannelName -> OpM (Maybe Quote)
getRandomQuote (ChannelName c) = doPost ("/chatbot/quotes/" <> c._unChannelName <> "random") noData

getCommands :: ChannelName -> OpM (Array Command)
getCommands (ChannelName c) = doPost ("/chatbot/commands/" <> c._unChannelName) noData

doPost :: forall a. DecodeJson a => String -> Maybe Request.RequestBody -> OpM a
doPost restOfUrl dat = do
  {hostname} <- ask
  -- HACK, but it works! was this:
  -- let baseUrl = (fromMaybe "http://localhost:8081" hostname)
  httpJSON $ buildReq POST (fromMaybe "" hostname <> restOfUrl) dat

loginToken :: Login -> OpM String
loginToken l = doPost "/login/token" (jsonData l)

createUser :: CreateUser -> OpM Int
createUser cu = doPost "/users" (jsonData cu)
