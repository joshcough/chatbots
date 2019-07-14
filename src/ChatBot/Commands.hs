module ChatBot.Commands
  ( Command(..)
  , Response(..)
  , builtinCommands
  , getCommandFromDb
  ) where

import Protolude

import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist (Entity(..))
import Text.Trifecta (Parser)

import ChatBot.DatabaseModels (DbCommand(..), DbQuote(..))
import ChatBot.Models (ChannelName(..))
import ChatBot.Parsers ((~~), commandName, number, slurp)
import ChatBot.Storage (CommandsDb(..), QuotesDb(..))
import Config (HasConfig)
import Types (AppTEnv')

data Command m =
  forall a. Command (Parser a) (ChannelName -> a -> m Response)

data Response
  = RespondWith Text
  | Nada

builtinCommands ::
     (HasConfig c, MonadIO m) => Map Text (Command (AppTEnv' e m c))
builtinCommands =
  Map.fromList
    [ ("!echo", echoCommand)
    , ("!addQuote", addQuoteCommand)
    , ("!quote", getQuoteCommand)
    , ("!addComm", addCommandCommand)
    , ("!delComm", deleteCommandCommand)
    ]

echoCommand :: Applicative m => Command m
echoCommand = Command slurp $ const $ pure . RespondWith

addQuoteCommand :: QuotesDb m => Command m
addQuoteCommand =
  Command slurp $ \c t -> do
    (Entity _ (DbQuote _ _ qid)) <- insertQuote c t
    pure $ RespondWith $ cs $ "added quote #" ++ show qid ++ ": " ++ cs t

getQuoteCommand :: QuotesDb m => Command m
getQuoteCommand = Command number $ \c n -> f n <$> getQuote c n
  where
    f n Nothing = RespondWith $ "I couldn't find quote: #" <> show n
    f _ (Just (Entity _ (DbQuote _ t _))) = RespondWith t

addCommandCommand :: CommandsDb m => Command m
addCommandCommand = Command (commandName ~~ slurp) $ \c (n, t) -> do
    _ <- insertCommand c n t
    pure $ RespondWith $ cs $ "added command:" <> n

deleteCommandCommand :: CommandsDb m => Command m
deleteCommandCommand  = Command commandName $ \c n -> do
    deleteCommand c n
    pure $ RespondWith $ cs $ "deleted command:" <> n

getCommandFromDb :: CommandsDb m => ChannelName -> Text -> m (Maybe Text)
getCommandFromDb c = fmap (fmap (dbCommandBody . entityVal)) . getCommand c

--import           Network.HTTP            (getRequest, getResponseBody, simpleHTTP)
--fetchUrl :: MonadIO m => ConvertibleStrings a String => a -> m String
--fetchUrl u = liftIO $ simpleHTTP (getRequest $ cs u) >>= getResponseBody
--const2 :: c -> a -> b -> c
--const2 c _ _ = c
