module ChatBot.Server.ChatBotServerMonad (
    ChatBotServerMonad(..)
  ) where

import Control.Monad.Except (MonadIO)
import Protolude

import ChatBot.Models (ChannelName(..), Command(..), Quote(..))
import qualified ChatBot.Storage as Storage
import Types (AppT')
import Config (HasConfig)

class Monad m => ChatBotServerMonad m where
  getStreams :: m [ChannelName]
  getCommands :: ChannelName -> m [Command]
  getQuotes :: ChannelName -> m [Quote]

instance (HasConfig c, MonadIO m) => ChatBotServerMonad (AppT' e m c) where
  getStreams = Storage.getStreams
  getCommands = Storage.getCommands
  getQuotes = Storage.getQuotes
