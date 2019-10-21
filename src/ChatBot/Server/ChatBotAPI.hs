module ChatBot.Server.ChatBotAPI (
    UnprotectedChatBotAPI
--  , ProtectedChatBotAPI
  , chatBotServerUnprotected
--  , chatBotServerProtected
  ) where

import ChatBot.Config (ChatBotExecutionConfig(..))
import ChatBot.Models (ChannelName, ChatMessage'(..), Command(..), Question(..), Quote(..))
import ChatBot.Server.ChatBotServerMonad (ChatBotServerMonad(..))
import Config (Config(..))
import Control.Concurrent.STM.TChan (TChan, dupTChan, readTChan)
import Control.Monad.Except (MonadIO)
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Network.WebSockets (Connection, forkPingThread, sendTextData)
import Logging ((.=), logDebug)
import Protolude
import Servant.API.WebSocket (WebSocket)
import ServantHelpers hiding (Stream)
import Types (AppT)
--import Auth.Models (User)

type UnprotectedChatBotAPI = "chatbot" :> Compose ChatBotUnprotected
--type ProtectedChatBotAPI= "chatbot" :> Compose ChatBotProtected

data ChatBotUnprotected r = ChatBotUnprotected {
    chatBotGetCommands :: r :- "commands" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Command]
  , chatBotGetQuestions :: r :- "questions" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Question]
  , chatBotGetQuestionStreams :: r :- "questions" :> "streams" :> Post '[JSON] [ChannelName]
  , chatBotGetQuotes :: r :- "quotes" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Quote]
  , chatBotGetQuoteStreams :: r :- "quotes" :> "streams" :> Post '[JSON] [ChannelName]
  --
  , chatBotConnectConnect :: r :- "connect" :> Capture "channel" ChannelName :> Get '[JSON] ()
  , chatBotConnectDisconnect :: r :- "disconnect" :> Capture "channel" ChannelName :> Get '[JSON] ()
  , chatBotChatWebSocket :: r :- "stream" :> Capture "channel" ChannelName :> WebSocket
  } deriving Generic

--data ChatBotProtected r = ChatBotProtected {
--    chatBotConnectConnect :: r :- "connect" :> Capture "channel" ChannelName :> Get '[JSON] ()
--  , chatBotConnectDisconnect :: r :- "disconnect" :> Capture "channel" ChannelName :> Get '[JSON] ()
--  } deriving Generic

-- | The server that runs the ChatBotAPI
chatBotServerUnprotected :: (MonadIO m) => ServerT UnprotectedChatBotAPI (AppT m)
chatBotServerUnprotected = toServant $ ChatBotUnprotected {
    chatBotGetQuoteStreams = getQuoteStreams
  , chatBotGetQuestionStreams = getQuestionStreams
  , chatBotGetCommands = getCommands
  , chatBotGetQuestions = getQuestions
  , chatBotGetQuotes = getQuotes
  --
  , chatBotConnectConnect = channelConnect
  , chatBotConnectDisconnect = channelDisconnect
  --
  , chatBotChatWebSocket = \stream conn -> do
      chanStm <- dupTChan . _cbecOutputChan . _configChatBotExecution <$> ask
      chan <- liftIO . atomically $ chanStm
      webSocket stream chan conn
  }

---- | The server that runs the ChatBotAPI
--chatBotServerProtected :: (MonadIO m) => User -> ServerT ProtectedChatBotAPI (AppT m)
--chatBotServerProtected user = toServant $ ChatBotProtected {
--    chatBotConnectConnect = adminOr401 user . channelConnect
--  , chatBotConnectDisconnect = adminOr401 user . channelDisconnect
--  }

webSocket :: (MonadIO m) => ChannelName -> TChan (ChatMessage' Value) -> Connection -> AppT m ()
webSocket stream chan conn = liftIO (forkPingThread conn 10) >> loop
  where
    loop = do
      $(logDebug) "reading from tchan" ["stream" .= stream]
      chatMessage <- liftIO . atomically $ readTChan chan
      $(logDebug) "writing to websocket" ["chatMessage" .= chatMessage]
      when (cmChannel chatMessage == stream) $ liftIO $ sendTextData conn (encodePretty chatMessage)
      loop
