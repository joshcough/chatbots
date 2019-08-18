module ChatBot.ChatBotAPI (
    ChatBotAPI
  , chatBotServer
  ) where

import Protolude
import           Control.Monad.Except        (MonadIO)
import           Database.Esqueleto          (Entity(..))
import           ServantHelpers

import           ChatBot.DatabaseModels      (DbCommand(..), DbQuote(..))
import           ChatBot.Storage             (CommandsDb(..), QuotesDb(..))
import           Types                       (AppT)

type ChatBotAPI = "chatbot" :> Compose ChatBot

data ChatBot route = ChatBot {
    getCommands :: route :- "commands" :> Get '[JSON] [DbCommand]
  , getQuotes :: route :- "quotes" :> Get '[JSON] [DbQuote]
  } deriving Generic

-- | The server that runs the ChatBotAPI
chatBotServer :: (MonadIO m) => ServerT ChatBotAPI (AppT m)
chatBotServer = toServant $ ChatBot { .. }
    where
    getCommands = fmap entityVal <$> getAllCommands
    getQuotes = fmap entityVal <$> getAllQuotes

