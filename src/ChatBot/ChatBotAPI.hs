module ChatBot.ChatBotAPI (
    ChatBotAPI
  , chatBotServer
  ) where

import Control.Monad.Except (MonadIO)
import Database.Esqueleto (Entity(..))
import Protolude
import ServantHelpers

import ChatBot.DatabaseModels (DbCommand(..), DbQuote(..))
import ChatBot.Models (Command(..), Quote(..))
import ChatBot.Storage (CommandsDb(..), QuotesDb(..))
import Types (AppT)

type ChatBotAPI = "chatbot" :> Compose ChatBot

data ChatBot route = ChatBot {
    getCommands :: route :- "commands" :> Get '[JSON] [Command]
  , getQuotes :: route :- "quotes" :> Get '[JSON] [Quote]
  } deriving Generic

-- | The server that runs the ChatBotAPI
chatBotServer :: (MonadIO m) => ServerT ChatBotAPI (AppT m)
chatBotServer = toServant $ ChatBot { .. }
    where
    getCommands = fmap (dbCommandToCommand . entityVal) <$> getAllCommands
    getQuotes = fmap (dbQuoteToCommand .entityVal) <$> getAllQuotes
    dbCommandToCommand (DbCommand chan name body) = Command chan name body
    dbQuoteToCommand (DbQuote chan name qid) = Quote chan name qid

