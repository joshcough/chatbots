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
getStreams = doGet "/chatbot/streams" noData

getQuotes :: ChannelName -> OpM (Array Quote)
getQuotes (ChannelName c) = doGet ("/chatbot/quotes/" <> c._unChannelName) noData

getRandomQuote :: ChannelName -> OpM (Maybe Quote)
getRandomQuote (ChannelName c) = doGet ("/chatbot/quotes/" <> c._unChannelName <> "/random") noData

getCommands :: ChannelName -> OpM (Array Command)
getCommands (ChannelName c) = doGet ("/chatbot/commands/" <> c._unChannelName) noData

doPost :: forall a. DecodeJson a => String -> Maybe Request.RequestBody -> OpM a
doPost = callURL POST

doGet :: forall a. DecodeJson a => String -> Maybe Request.RequestBody -> OpM a
doGet = callURL GET

callURL :: forall a. DecodeJson a => Method -> String -> Maybe Request.RequestBody -> OpM a
callURL method restOfUrl dat = do
  {hostname} <- ask
  -- HACK, but it works! was this:
  -- let baseUrl = (fromMaybe "http://localhost:8081" hostname)
  httpJSON $ buildReq method (fromMaybe "" hostname <> restOfUrl) dat

loginToken :: Login -> OpM String
loginToken l = doPost "/login/token" (jsonData l)

createUser :: CreateUser -> OpM Int
createUser cu = doPost "/users" (jsonData cu)
