module ChatBot.Server.ChatBotServerMonad (
    ChatBotServerMonad(..)
  ) where

import ChatBot.Config (ChatBotExecutionConfig(..), ChatBotFrontendMessage(..))
import ChatBot.Models (ChannelName(..), Command(..), Quote(..), Question(..))
import qualified ChatBot.Storage as Storage
import Config (HasConfig, configChatBotExecution)
import Control.Lens (view)
import Control.Monad.Except (MonadIO)
import Protolude
import Types (AppT')

class Monad m => ChatBotServerMonad m where
  getQuoteStreams :: m [ChannelName]
  getQuestionStreams :: m [ChannelName]
  getCommands :: ChannelName -> m [Command]
  getQuotes :: ChannelName -> m [Quote]
  getQuestions :: ChannelName -> m [Question]
  channelConnect :: ChannelName -> m ()
  channelDisconnect :: ChannelName -> m ()

instance (HasConfig c, MonadIO m) => ChatBotServerMonad (AppT' e m c) where
  getQuoteStreams = Storage.getQuoteStreams
  getQuestionStreams = Storage.getQuestionsStreams
  getCommands = Storage.getCommands
  getQuestions = Storage.getQuestions
  getQuotes = Storage.getQuotes
  channelConnect = writeToInputChan . ConnectTo
  channelDisconnect = writeToInputChan . DisconnectFrom

writeToInputChan :: (MonadReader c m, HasConfig c, MonadIO m) => ChatBotFrontendMessage -> m ()
writeToInputChan c = do
    chan <- _cbecInputChan <$> view configChatBotExecution
    liftIO $ writeChan chan c
