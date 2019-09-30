module ChatBot.WebSocket.Commands
  ( BotCommand(..)
  , Permission(..)
  , Response(..)
  , builtinCommands
--  , getCommandFromDb
  ) where

import Protolude

import ChatBot.Models (ChannelName(..), Question(..), Quote(..))
import ChatBot.Storage (CommandsDb(..), QuestionsDb(..), QuotesDb(..))
import ChatBot.WebSocket.Parsers (anything, number, slurp)
import Config (Config(..), Environment(Development), HasConfig(..))
import Control.Lens (view)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Trifecta (Parser)

data Permission = ModOnly | Anyone
  deriving stock (Eq, Show)

data View = Questions | Quotes
  deriving stock (Eq, Show)

data BotCommand m = forall a. BotCommand {
   bcPermission :: Permission
 , bcParser :: Parser a
 , bcExecute :: ChannelName -> a -> m Response
 }

data Response = RespondWith Text | Nada
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type Db m = (CommandsDb m, QuestionsDb m, QuotesDb m)

builtinCommands :: (Db m, MonadReader c m, HasConfig c) => Map Text (BotCommand m)
builtinCommands =
  Map.fromList
    [ ("!addQuote", addQuoteCommand)
    , ("!quote", getQuoteCommand)
    , ("!quotes", getQuotesUrlCommand)
    , ("!addQuestion", addQuestionCommand)
    , ("!question", getQuestionCommand)
    , ("!questions", getQuestionsUrlCommand)
--    , ("!addComm", addCommandCommand)
--    , ("!delComm", deleteCommandCommand)
    ]

addQuoteCommand :: QuotesDb m => BotCommand m
addQuoteCommand = BotCommand ModOnly slurp $ \c t -> do
    q <- insertQuote c t
    pure $ RespondWith $ cs $ "added quote #" ++ show (quoteQid q) ++ ": " ++ cs t

getQuoteCommand :: QuotesDb m => BotCommand m
getQuoteCommand = BotCommand Anyone number $ \c n -> f n <$> getQuote c n
  where
    f n Nothing = RespondWith $ "I couldn't find quote: #" <> show n
    f _ (Just q) = RespondWith $ quoteBody q

getQuotesUrlCommand :: (CommandsDb m, MonadReader c m, HasConfig c) => BotCommand m
getQuotesUrlCommand = BotCommand Anyone anything $ \c _ -> RespondWith <$> mkUrl c Quotes

addQuestionCommand :: QuestionsDb m => BotCommand m
addQuestionCommand = BotCommand ModOnly slurp $ \c t -> do
    q <- insertQuestion c t
    pure $ RespondWith $ cs $ "added question #" ++ show (questionQid q) ++ ": " ++ cs t

getQuestionCommand :: QuestionsDb m => BotCommand m
getQuestionCommand = BotCommand Anyone number $ \c n -> f n <$> getQuestion c n
  where
    f n Nothing = RespondWith $ "I couldn't find question: #" <> show n
    f _ (Just q) = RespondWith $ questionBody q

getQuestionsUrlCommand :: (QuestionsDb m, MonadReader c m, HasConfig c) => BotCommand m
getQuestionsUrlCommand = BotCommand Anyone anything $ \c _ -> RespondWith <$> mkUrl c Questions

mkUrl :: (MonadReader c m, HasConfig c) => ChannelName -> View -> m Text
mkUrl (ChannelName c) viewtype = do
  Config{..} <- view config
  let ending = T.toLower . show $ viewtype
  let f p = "https://" <> _configHost <> p <> "/" <> "?stream=" <> T.drop 1 c <> "&view=" <> ending
  pure $ if _configEnv == Development
           then f (":" <> show _configPort)
           else f ""

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
