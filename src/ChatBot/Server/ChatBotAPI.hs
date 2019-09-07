module ChatBot.Server.ChatBotAPI (
    ChatBotAPI
  , chatBotServer
  ) where

import Control.Monad.Except (MonadIO)
import Protolude
import ServantHelpers hiding (Stream)

import ChatBot.Models (Command(..), Quote(..), ChannelName(..))
import Types (AppT)
import ChatBot.Server.ChatBotServerMonad (ChatBotServerMonad(..))

type ChatBotAPI = "chatbot" :> Compose ChatBot

data ChatBot route = ChatBot {
    chatBotGetStreams :: route :- "streams" :> Post '[JSON] [ChannelName]
  , chatBotGetCommands :: route :- "commands" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Command]
  , chatBotGetQuotes :: route :- "quotes" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Quote]
  } deriving Generic

-- | The server that runs the ChatBotAPI
chatBotServer :: (MonadIO m) => ServerT ChatBotAPI (AppT m)
chatBotServer = toServant $ ChatBot {
    chatBotGetStreams = getStreams
  , chatBotGetCommands = getCommands
  , chatBotGetQuotes = getQuotes
  }
