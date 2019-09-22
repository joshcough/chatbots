module Network.Endpoints
    ( getCommands
    , getQuotes
    , getStreams
    ) where

import Prelude
import ChatBot.Models (ChannelName, Command, Quote)
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, jsonData, noData)
import Types ( OpM, Config )
import Control.Monad.Reader.Trans (ask)
import Data.Maybe (Maybe, fromMaybe)
import Affjax.RequestBody as Request
import Data.Argonaut.Decode (class DecodeJson)

getStreams :: OpM (Array ChannelName)
getStreams = doPost "/streams" noData

getQuotes :: ChannelName -> OpM (Array Quote)
getQuotes c = doPost "/quotes" (jsonData c)

getCommands :: ChannelName -> OpM (Array Command)
getCommands c = doPost "/commands" (jsonData c)

doPost :: forall a. DecodeJson a => String -> Maybe Request.RequestBody -> OpM a
doPost restOfUrl dat = do
  {hostname} <- ask
  -- let baseUrl = (fromMaybe "http://localhost:8081" hostname) <> "/chatbot"
  let baseUrl = (fromMaybe "" hostname) <> "/chatbot"
  httpJSON $ buildReq POST (baseUrl <> restOfUrl) dat
