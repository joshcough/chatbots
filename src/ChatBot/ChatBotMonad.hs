module ChatBot.ChatBotMonad (
    ChatBotMonad(..)
  ) where

import Control.Monad.Except (MonadIO)
import Protolude

import ChatBot.Models (ChannelName(..), Command(..), Quote(..))
import qualified ChatBot.Storage as Storage
import Types (AppT')
import Config (HasConfig)

class Monad m => ChatBotMonad m where
  getCommands :: ChannelName -> m [Command]
  getQuotes :: ChannelName -> m [Quote]

instance (HasConfig c, MonadIO m) => ChatBotMonad (AppT' e m c) where
  getCommands = Storage.getCommands
  getQuotes = Storage.getQuotes
