module ChatBot.WebSocket.Commands
  ( BotCommand(..)
  , Response(..)
  , builtinCommands
  , getCommandFromDb
  ) where

import Protolude

import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Trifecta (Parser)

import ChatBot.Models (ChannelName(..), Command(..), Quote(..))
import ChatBot.Storage (CommandsDb(..), QuotesDb(..))
import ChatBot.WebSocket.Parsers ((~~), anything, number, slurp)
import qualified ChatBot.WebSocket.Parsers as P
import Config (Config(..), HasConfig(..),  Environment(Development))
import Control.Lens (view)

data BotCommand m =
  forall a. BotCommand (Parser a) (ChannelName -> a -> m Response)

data Response
  = RespondWith Text
  | Nada

builtinCommands :: (CommandsDb m, QuotesDb m, MonadReader c m, HasConfig c) => Map Text (BotCommand m)
builtinCommands =
  Map.fromList
    [ ("!echo", echoCommand)
    , ("!addQuote", addQuoteCommand)
    , ("!quote", getQuoteCommand)
    , ("!addComm", addCommandCommand)
    , ("!delComm", deleteCommandCommand)
    , ("!quotes", getQuotesUrlCommand)
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

getQuotesUrlCommand :: (CommandsDb m, MonadReader c m, HasConfig c) => BotCommand m
getQuotesUrlCommand = BotCommand anything $ \(ChannelName c) _ -> do
    conf <- view config
    let env = _configEnv conf
    let f p = "https://" <> _configHost conf <> p <> "/" <> "?stream=" <> T.drop 1 c
    let url = if env == Development
              then  f (":" <> show (_configPort conf))
              else f ""
    pure $ RespondWith url

addCommandCommand :: CommandsDb m => BotCommand m
addCommandCommand = BotCommand (P.commandName ~~ slurp) $ \c (n, t) -> do
    -- TODO: here is where i would intercept !addComm !quoteN commands. just check if n is quote* or something.
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
