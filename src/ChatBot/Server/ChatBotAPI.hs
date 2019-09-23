module ChatBot.Server.ChatBotAPI (
    UnprotectedChatBotAPI
  , ProtectedChatBotAPI
  , chatBotServerUnprotected
  , chatBotServerProtected
  ) where

import ChatBot.Models (ChannelName(..), Command(..), Quote(..))
import ChatBot.Server.ChatBotServerMonad (ChatBotServerMonad(..))
import Control.Monad.Except (MonadIO)
import Protolude
import ServantHelpers hiding (Stream)
import Types (AppT)
import Auth.Models (User)

type UnprotectedChatBotAPI = "chatbot" :> Compose ChatBotUnprotected
type ProtectedChatBotAPI= "chatbot" :> Compose ChatBotProtected

data ChatBotUnprotected r = ChatBotUnprotected {
    chatBotGetStreams :: r :- "streams" :> Post '[JSON] [ChannelName]
  , chatBotGetCommands :: r :- "commands" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Command]
  , chatBotGetQuotes :: r :- "quotes" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Quote]
  } deriving Generic

data ChatBotProtected r = ChatBotProtected {
    chatBotConnectConnect :: r :- "connect" :> Capture "channel" ChannelName :> Get '[JSON] ()
  , chatBotConnectDisconnect :: r :- "disconnect" :> Capture "channel" ChannelName :> Get '[JSON] ()
  } deriving Generic

-- | The server that runs the ChatBotAPI
chatBotServerUnprotected :: (MonadIO m) => ServerT UnprotectedChatBotAPI (AppT m)
chatBotServerUnprotected = toServant $ ChatBotUnprotected {
    chatBotGetStreams = getStreams
  , chatBotGetCommands = getCommands
  , chatBotGetQuotes = getQuotes
  }

-- | The server that runs the ChatBotAPI
chatBotServerProtected :: (MonadIO m) => User -> ServerT ProtectedChatBotAPI (AppT m)
chatBotServerProtected user = toServant $ ChatBotProtected {
    chatBotConnectConnect = adminOr401 user . channelConnect
  , chatBotConnectDisconnect = adminOr401 user . channelDisconnect
  }