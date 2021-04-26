module ChatBot.WebSocket.Commands
  ( BotCommand(..)
  , Permission(..)
  , Response(..)
  , builtinCommands
--  , getCommandFromDb
  ) where

import           Protolude

import           ChatBot.Models            (ChannelName, ChatUser (..), Quote (..), getChannelName)
import           ChatBot.Storage           (QuotesDb (..))
import           ChatBot.WebSocket.Parsers (anything, number, slurp)
import           Config                    (Config (..), Environment (Development), HasConfig (..))
import           Control.Lens              (view)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Text.Trifecta             (Parser, optional)

data Permission = ModOnly | Anyone
  deriving stock (Eq, Show)

data View = Quotes
  deriving stock (Eq, Show)

data BotCommand m = forall a . BotCommand
  { bcPermission :: Permission
  , bcParser :: Parser a
  , bcExecute :: ChannelName -> ChatUser -> a -> m Response
  }

data Response = RespondWith Text | Nada
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type Db m = (QuotesDb m)

builtinCommands :: (Db m, MonadReader c m, HasConfig c) => Map Text (BotCommand m)
builtinCommands = Map.fromList
  [ ("!addQuote", addQuoteCommand)
  , ("!delQuote", delQuoteCommand)
  , ("!quote", getQuoteCommand)
  , ("!quotes", getQuotesUrlCommand)
--    , ("!addComm", addCommandCommand)
--    , ("!delComm", deleteCommandCommand)
  ]

addQuoteCommand :: QuotesDb m => BotCommand m
addQuoteCommand = BotCommand ModOnly slurp $ \c cu t -> do
  q <- insertQuote c (cuUserName cu) t
  pure $ RespondWith $ cs $ "Added " <> displayQuote q

delQuoteCommand :: QuotesDb m => BotCommand m
delQuoteCommand = BotCommand ModOnly number $ \c _ n -> do
  deleteQuote c n
  pure $ RespondWith $ cs $ "Deleted Quote #" ++ show n

getQuoteCommand :: QuotesDb m => BotCommand m
getQuoteCommand = BotCommand Anyone (optional number) $ \c _ mn -> case mn of
  Nothing -> f "I couldn't find any quotes, man." <$> getRandomQuote c
  Just n -> f ("I couldn't find quote #" <> show n <> ", man.") <$> getQuote c n
  where f msg mq = RespondWith $ maybe msg displayQuote mq

displayQuote :: Quote -> Text
displayQuote Quote {..} = "Quote #" <> show quoteQid <> ": " <> quoteBody

getQuotesUrlCommand :: (MonadReader c m, HasConfig c) => BotCommand m
getQuotesUrlCommand = BotCommand Anyone anything $ \c _ _ -> RespondWith <$> mkUrl c Quotes

mkUrl :: (MonadReader c m, HasConfig c) => ChannelName -> View -> m Text
mkUrl chan viewtype = do
  let c = getChannelName chan
  Config {..} <- view config
  let ending = T.toLower . show $ viewtype
  let f p = "https://" <> _configHost <> p <> "/" <> "?stream=" <> c <> "&view=" <> ending
  pure $ if _configEnv == Development then f (":" <> show _configPort) else f ""

{-
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
-}

--import           Network.HTTP            (getRequest, getResponseBody, simpleHTTP)
--fetchUrl :: MonadIO m => ConvertibleStrings a String => a -> m String
--fetchUrl u = liftIO $ simpleHTTP (getRequest $ cs u) >>= getResponseBody
--const2 :: c -> a -> b -> c
--const2 c _ _ = c
