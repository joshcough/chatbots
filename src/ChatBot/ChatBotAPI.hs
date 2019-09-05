module ChatBot.ChatBotAPI (
    ChatBotAPI
  , chatBotServer
  ) where

import Control.Monad.Except (MonadIO)
import Database.Esqueleto (Entity(..))
import Protolude
import ServantHelpers hiding (Stream)

import ChatBot.DatabaseModels (DbCommand(..), DbQuote(..))
import ChatBot.Models (Command(..), Quote(..), ChannelName(..))
import qualified ChatBot.Storage as Storage
import Types (AppT)

type ChatBotAPI = "chatbot" :> Compose ChatBot

data ChatBot route = ChatBot {
    getCommands :: route :- "commands" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Command]
  , getQuotes :: route :- "quotes" :> ReqBody '[JSON] ChannelName :> Post '[JSON] [Quote]
  } deriving Generic

-- | The server that runs the ChatBotAPI
chatBotServer :: (MonadIO m) => ServerT ChatBotAPI (AppT m)
chatBotServer = toServant $ ChatBot { .. }
    where
    getCommands stream = fmap (dbCommandToCommand . entityVal) <$> Storage.getCommands stream
    getQuotes stream = fmap (dbQuoteToCommand .entityVal) <$> Storage.getQuotes stream
    dbCommandToCommand (DbCommand chan name body) = Command (ChannelName chan) name body
    dbQuoteToCommand (DbQuote chan name qid) = Quote (ChannelName chan) name qid

