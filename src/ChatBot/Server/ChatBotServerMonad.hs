module ChatBot.Server.ChatBotServerMonad (
    ChatBotServerMonad(..)
  ) where

import           ChatBot.Config       (ChatBotExecutionConfig (..), ChatBotFrontendMessage (..))
import           ChatBot.Models       (ChannelName, Command (..), Quote (..), Stream (..))
import qualified ChatBot.Storage      as Storage
import           Config               (HasConfig, configChatBotExecution)
import           Control.Lens         (view)
import           Error                (ChatBotError)
import           Protolude
import           Types                (AppT')

class Monad m => ChatBotServerMonad m where
  getStreams :: m [ChannelName]
  getCommands :: ChannelName -> m [Command]
  getQuotes :: ChannelName -> m [Quote]
  getRandomQuote :: ChannelName -> m (Maybe Quote)
  channelConnect :: ChannelName -> m ()
  channelDisconnect :: ChannelName -> m ()

instance (HasConfig c, MonadIO m) => ChatBotServerMonad (AppT' ChatBotError m c) where
  getStreams = fmap _streamChannelName <$> Storage.getStreams
  getCommands = Storage.getCommands
  getQuotes = Storage.getQuotes
  getRandomQuote = Storage.getRandomQuote
  channelConnect = writeToInputChan . ConnectTo
  channelDisconnect = writeToInputChan . DisconnectFrom

writeToInputChan :: (MonadReader c m, HasConfig c, MonadIO m) => ChatBotFrontendMessage -> m ()
writeToInputChan c = do
  chan <- _cbecInputChan <$> view configChatBotExecution
  liftIO $ writeChan chan c
