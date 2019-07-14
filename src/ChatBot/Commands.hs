module ChatBot.Commands (Command(..), Response(..), defaultCommands) where

import Protolude
import Prelude (String) -- TODO: kill me

import qualified Data.Map                as Map
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Data.Text               (Text)
import           Database.Persist        (Entity(..))
import           Network.HTTP            (getRequest, getResponseBody, simpleHTTP)
import           Text.Trifecta           (Parser)

import           ChatBot.DatabaseModels  (DbQuote(..))
import           ChatBot.Models          (ChannelName(..))
import           ChatBot.Parsers         (number, slurp, url)
import           ChatBot.Storage         (QuotesDb(..))
import           Config                  (HasConfig)
import           Types                   (AppTEnv')

defaultCommands :: (HasConfig c, MonadIO m) => Map Text (Command (AppTEnv' e m c))
defaultCommands = Map.fromList
  [ ("!echo",  echoCommand)
  , ("!url",   Command url      $ const $ fmap (RespondWith . cs . take 100) . fetchUrl)
  , ("!addQuote", addQuoteCommand)
  , ("!quote", getQuoteCommand)
  ]

echoCommand :: Applicative m => Command m
echoCommand = Command slurp $ const $ pure . RespondWith

addQuoteCommand :: (HasConfig c, MonadIO m) => Command (AppTEnv' e m c)
addQuoteCommand = Command slurp $ \c t -> do
    (Entity _ (DbQuote _ _ qid)) <- insertQuote c t
    pure $ RespondWith $ cs $ "added quote #" ++ show qid ++ ": " ++ cs t

getQuoteCommand :: (HasConfig c, MonadIO m) => Command (AppTEnv' e m c)
getQuoteCommand = Command number $ \c n ->
    getQuote c n >>= pure . \case
        Nothing -> RespondWith $ "no such quote: #" <> show n
        Just (Entity _ (DbQuote _ t _)) -> RespondWith t

data Command m = forall a . Command (Parser a) (ChannelName -> a -> m Response)
data Response = RespondWith Text | Nada

fetchUrl :: MonadIO m => ConvertibleStrings a String => a -> m String
fetchUrl u = liftIO $ simpleHTTP (getRequest $ cs u) >>= getResponseBody

--const2 :: c -> a -> b -> c
--const2 c _ _ = c
