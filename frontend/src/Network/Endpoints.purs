module Network.Endpoints
    ( getCommands
    , getQuestions
    , getQuotes
    , getQuestionStreams
    , getQuoteStreams
    , loginToken
    , createUser
    ) where

import Prelude
import Auth.Models (CreateUser, Login)
import ChatBot.Models (ChannelName, Command, Question, Quote)
import Network.HTTP (Method(..), buildReq, httpJSON, jsonData, noData)
import Types (OpM)
import Control.Monad.Reader.Trans (ask)
import Data.Maybe (Maybe, fromMaybe)
import Affjax.RequestBody as Request
import Data.Argonaut.Decode (class DecodeJson)

getQuestionStreams :: OpM (Array ChannelName)
getQuestionStreams = doPost "/chatbot/questions/streams" noData

getQuoteStreams :: OpM (Array ChannelName)
getQuoteStreams = doPost "/chatbot/quotes/streams" noData

getQuestions :: ChannelName -> OpM (Array Question)
getQuestions c = doPost "/chatbot/questions" (jsonData c)

getQuotes :: ChannelName -> OpM (Array Quote)
getQuotes c = doPost "/chatbot/quotes" (jsonData c)

getCommands :: ChannelName -> OpM (Array Command)
getCommands c = doPost "/chatbot/commands" (jsonData c)

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
