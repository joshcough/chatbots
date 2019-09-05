module ChatBot.Commands
  ( BotCommand(..)
  , Response(..)
  , builtinCommands
  , getCommandFromDb
  ) where

import Protolude

import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import Text.Trifecta (Parser)

import ChatBot.Models (ChannelName(..), Command(..), Quote(..))
import ChatBot.Parsers ((~~), number, slurp)
import qualified ChatBot.Parsers as P
import ChatBot.Storage (CommandsDb(..), QuotesDb(..))
import Config (HasConfig)
import Types (AppTEnv')

data BotCommand m =
  forall a. BotCommand (Parser a) (ChannelName -> a -> m Response)

data Response
  = RespondWith Text
  | Nada

builtinCommands ::
     (HasConfig c, MonadIO m) => Map Text (BotCommand (AppTEnv' e m c))
builtinCommands =
  Map.fromList
    [ ("!echo", echoCommand)
    , ("!addQuote", addQuoteCommand)
    , ("!quote", getQuoteCommand)
    , ("!addComm", addCommandCommand)
    , ("!delComm", deleteCommandCommand)
    ]

echoCommand :: Applicative m => BotCommand m
echoCommand = BotCommand slurp $ const $ pure . RespondWith

addQuoteCommand :: QuotesDb m => BotCommand m
addQuoteCommand =
  BotCommand slurp $ \c t -> do
    q <- insertQuote c t
    pure $ RespondWith $ cs $ "added quote #" ++ show (quoteQid q) ++ ": " ++ cs t

getQuoteCommand :: QuotesDb m => BotCommand m
getQuoteCommand = BotCommand number $ \c n -> f n <$> getQuote c n
  where
    f n Nothing = RespondWith $ "I couldn't find quote: #" <> show n
    f _ (Just q) = RespondWith $ quoteBody q

addCommandCommand :: CommandsDb m => BotCommand m
addCommandCommand = BotCommand (P.commandName ~~ slurp) $ \c (n, t) -> do
    insertCommand c n t
    pure $ RespondWith $ cs $ "added command:" <> n

deleteCommandCommand :: CommandsDb m => BotCommand m
deleteCommandCommand  = BotCommand P.commandName $ \c n -> do
    deleteCommand c n
    pure $ RespondWith $ cs $ "deleted command:" <> n

getCommandFromDb :: CommandsDb m => ChannelName -> Text -> m (Maybe Text)
getCommandFromDb c = fmap (fmap commandBody) . getCommand c

--import           Network.HTTP            (getRequest, getResponseBody, simpleHTTP)
--fetchUrl :: MonadIO m => ConvertibleStrings a String => a -> m String
--fetchUrl u = liftIO $ simpleHTTP (getRequest $ cs u) >>= getResponseBody
--const2 :: c -> a -> b -> c
--const2 c _ _ = c
