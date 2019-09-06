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

getStreams :: forall m . MonadAff m => MonadError HttpException m => m (Array ChannelName)
getStreams = httpJSON $ buildReq POST "http://localhost:8081/chatbot/streams" noData

getQuotes :: forall m . MonadAff m => MonadError HttpException m => ChannelName -> m (Array Quote)
getQuotes c = httpJSON $ buildReq POST "http://localhost:8081/chatbot/quotes" (jsonData c)

getCommands :: forall m . MonadAff m => MonadError HttpException m => ChannelName -> m (Array Command)
getCommands c = httpJSON $ buildReq POST "http://localhost:8081/chatbot/commands" (jsonData c)
