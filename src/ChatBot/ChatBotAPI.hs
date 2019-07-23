module ChatBot.ChatBotAPI (
    ChatBotAPI
  , chatBotServer
  ) where

import Protolude
import           Control.Monad.Except        (MonadIO)
import           Database.Esqueleto          (Entity(..))
import           ServantHelpers

import           ChatBot.DatabaseModels      (DbCommand(..))
import           ChatBot.Storage             (CommandsDb(..))
import           Types                       (AppT)

type ChatBotAPI = "chatbot" :> Compose ChatBot

newtype ChatBot route = ChatBot {
    getCommands :: route :- "commands" :> Get '[JSON] [DbCommand]
  } deriving Generic

-- | The server that runs the ChatBotAPI
chatBotServer :: (MonadIO m) => ServerT ChatBotAPI (AppT m)
chatBotServer = toServant $ ChatBot { .. }
    where
    getCommands = fmap entityVal <$> getAllCommands

