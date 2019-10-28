module ChatBot.Storage (
    ChatBotDB
  , CommandsDb(..)
  , QuotesDb(..)
  , StreamsDb(..)
  ) where

import Protolude hiding (from)

import Control.Monad.Except (MonadIO, throwError)
import Data.List ((!!), sort)
import Database.Esqueleto
import Database.Persist.Postgresql (insert)
import qualified Database.Persist.Postgresql as P
import Logging ((.=), logDebug)
import System.Random (randomRIO)
import Types (runDb, AppT')

import ChatBot.DatabaseModels
  ( DbCommand(..)
  , DbQuote(..)
  , DbStream(..)
  , EntityField(..)
  )
import ChatBot.Models
  ( ChannelName
  , ChatUserName(..)
  , Command(..)
  , Quote(..)
  , Stream(..)
  , getChannelName
  , mkChannelName
  )
import Config (HasConfig)
import Error (ChatBotError, miscError)

type ChatBotDB m = (QuotesDb m)

class Monad m => StreamsDb m where
    insertStream :: ChannelName -> m Stream
    getStream :: ChannelName -> m (Maybe Stream)
    getStreams :: m [Stream]

class Monad m => CommandsDb m where
    insertCommand :: ChannelName -> Text -> Text -> m ()
    getCommand :: ChannelName -> Text -> m (Maybe Command)
    deleteCommand :: ChannelName -> Text -> m ()
    getCommands :: ChannelName -> m [Command]

class Monad m => QuotesDb m where
    insertQuote :: ChannelName -> ChatUserName -> Text -> m Quote
    getQuote :: ChannelName -> Int -> m (Maybe Quote)
    deleteQuote :: ChannelName -> Int -> m ()
    getQuotes :: ChannelName -> m [Quote]
    getRandomQuote :: ChannelName -> m (Maybe Quote)


withStream :: (StreamsDb m, MonadError ChatBotError m) =>
               ChannelName -> (Stream -> m a) -> m a
withStream c f = getStream c >>= \case
    Nothing -> throwError . miscError $ "Stream " <> getChannelName c <> " does not exist"
    Just s -> f s

instance (HasConfig c, MonadIO m) => StreamsDb (AppT' e m c) where
    getStreams = do
        $(logDebug) "getStreams" []
        fmap (sort . fmap dbStreamToStream) . runDb $ select $ from pure

    getStream c = do
        let channel = getChannelName c
        $(logDebug) "getStream" ["channel" .= channel]
        ms <- runDb $ selectFirst [DbStreamName P.==. channel] []
        pure $ dbStreamToStream <$> ms

    insertStream c = do
        let channel = getChannelName c
        $(logDebug) "insertStream" ["channel" .= channel]
        getStream c >>= \case
            Nothing -> do
                k <- runDb $ insert $ DbStream channel
                pure $ Stream (fromSqlKey k) c
            -- stream already exists, do nothing
            Just s -> pure s

instance (HasConfig c, MonadIO m) => CommandsDb (AppT' ChatBotError m c) where
    insertCommand c commandName commandText = do
        let channel = getChannelName c
        $(logDebug) "insertCommand" ["channel" .= channel, "name" .= commandName, "text" .= commandText]
        withStream c $ \s -> runDb $ insert $ DbCommand (toSqlKey $ _streamId s) commandName commandText
        return ()
    getCommand c commandName = do
        let channel = getChannelName c
        $(logDebug) "getCommand" ["channel" .= channel, "name" .= commandName]
        withStream c $ \s -> fmap (fmap (dbCommandToCommand s)) . runDb $
            selectFirst [DbCommandChannel P.==. toSqlKey (_streamId s), DbCommandName P.==. commandName] []
    deleteCommand c commandName = do
        let channel = getChannelName c
        $(logDebug) "deleteCommand" ["channel" .= channel, "name" .= commandName]
        withStream c $ \s -> runDb $ delete $ from $ \command ->
            where_ (
                (command ^. DbCommandChannel) ==. val (toSqlKey $ _streamId s)
                &&.
                (command ^. DbCommandName) ==. val commandName
             )
    getCommands c = do
       let channel = getChannelName c
       $(logDebug) "getCommands" ["channel" .= channel]
       withStream c $ \s ->
         fmap (fmap (dbCommandToCommand s)) . runDb $ select $ from $ \command -> do
           where_ $ (command ^. DbCommandChannel) ==. val (toSqlKey $ _streamId s)
           pure command

instance (HasConfig c, MonadIO m) => QuotesDb (AppT' ChatBotError m c) where
    insertQuote c user quoteText = do
        let channel = getChannelName c
        $(logDebug) "insertQuote" ["channel" .= channel, "user" .= user, "quoteText" .= quoteText]
        withStream c $ \s -> do
            qid <- nextQuoteId c
            _ <- runDb $ insert $ DbQuote (toSqlKey $ _streamId s) quoteText user qid
            pure $ Quote c quoteText user qid
    getQuote c qid = do
        let channel = getChannelName c
        $(logDebug) "getQuote" ["channel" .= channel, "qid" .= qid]
        withStream c $ \s ->
          -- select * from quote where channel = channel and qid = qid
          fmap (fmap (dbQuoteToQuote s)) . runDb $ selectFirst
            [DbQuoteChannel P.==. toSqlKey (_streamId s), DbQuoteQid P.==. qid] []
    deleteQuote c qid = do
        let channel = getChannelName c
        $(logDebug) "deleteQuote" ["channel" .= channel, "qid" .= qid]
        withStream c $ \s ->
            -- delete from quote where channel = channel and qid = qid
            runDb $ delete $ from $ \quote ->
                where_ (
                    (quote ^. DbQuoteChannel) ==. val (toSqlKey $ _streamId s)
                    &&.
                    (quote ^. DbQuoteQid) ==. val qid
                 )
    getQuotes c = do
        let channel = getChannelName c
        $(logDebug) "getQuotes" ["channel" .= channel]
        withStream c $ \s ->
          fmap (fmap (dbQuoteToQuote s)) . runDb $ select $ from $ \quote -> do
            where_ $ (quote ^. DbQuoteChannel) ==. val (toSqlKey $ _streamId s)
            pure quote
    getRandomQuote c = do
        let channel = getChannelName c
        $(logDebug) "getRandomQuote" ["channel" .= channel]
        quoteIds c >>= pick >>= \case
          Nothing -> return Nothing
          Just qid -> getQuote c qid

nextQuoteId :: (HasConfig c, MonadIO m) => ChannelName -> AppT' ChatBotError m c Int
nextQuoteId c =
    withStream c $ \s -> runDb $ do
        [Value mn] <- select $ from $ \quote -> do
            where_ $ (quote ^. DbQuoteChannel) ==. val (toSqlKey $ _streamId s)
            return (max_ $ quote ^. DbQuoteQid)
        pure $ maybe 1 (+1) mn

pick :: MonadIO m => [a] -> m (Maybe a)
pick [] = return Nothing
pick xs = liftIO $ Just . (xs !!) <$> randomRIO (0, length xs - 1)

quoteIds :: (HasConfig c, MonadIO m) => ChannelName -> AppT' ChatBotError m c [Int]
quoteIds c = withStream c $ \s -> runDb $
  fmap (fmap unValue) $ select $ from $ \quote -> do
    where_ $ (quote ^. DbQuoteChannel) ==. val (toSqlKey $ _streamId s)
    return (quote ^. DbQuoteQid)


dbCommandToCommand :: Stream -> Entity DbCommand -> Command
dbCommandToCommand s (Entity _ (DbCommand _ name body)) = Command (_streamChannelName s) name body

dbQuoteToQuote :: Stream -> Entity DbQuote -> Quote
dbQuoteToQuote s (Entity _ (DbQuote _ name user qid)) = Quote (_streamChannelName s) name user qid

dbStreamToStream :: Entity DbStream -> Stream
dbStreamToStream (Entity k (DbStream name)) = Stream (fromSqlKey k) (mkChannelName name)
