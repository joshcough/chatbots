module ChatBot.Server.ChatBotAPI (
    ChatBotAPI
  , chatBotServer
  ) where

import ChatBot.Models (ChannelName(..), Command(..), Quote(..))
import ChatBot.Server.ChatBotServerMonad (ChatBotServerMonad(..))
import Control.Monad.Except (MonadIO)
import Protolude
import ServantHelpers hiding (Stream)
import Types (AppT)

type ChatBotAPI = "chatbot" :> Compose ChatBot

data ChatBot r = ChatBot {
    chatBotGetStreams :: r :- "streams" :> Post '[JSON] [ChannelName]
  , chatBotGetCommands :: r :- "commands" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Command]
  , chatBotGetQuotes :: r :- "quotes" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Quote]
  , chatBotConnectConnect :: r :- "connect" :> Capture "channel" ChannelName :> Get '[JSON] ()
  , chatBotConnectDisconnect :: r :- "disconnect" :> Capture "channel" ChannelName :> Get '[JSON] ()
  } deriving Generic

-- | The server that runs the ChatBotAPI
chatBotServer :: (MonadIO m) => ServerT ChatBotAPI (AppT m)
chatBotServer = toServant $ ChatBot {
    chatBotGetStreams = getStreams
  , chatBotGetCommands = getCommands
  , chatBotGetQuotes = getQuotes
  , chatBotConnectConnect = channelConnect
  , chatBotConnectDisconnect = channelDisconnect
  }
