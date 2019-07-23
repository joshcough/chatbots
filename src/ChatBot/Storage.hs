module ChatBot.Storage (
    ChatBotDB
  , CommandsDb(..)
  , QuotesDb(..)
  ) where

import Protolude hiding (from)

import           Control.Monad.Except        (MonadIO)
import           Data.Aeson                  ((.=))
import           Database.Persist.Postgresql (insert)
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import           Logging                     (logDebug, logDebug_)
import           Types                       (AppT', runDb)

import           ChatBot.Config              (ChannelName(..))
import           ChatBot.DatabaseModels      (DbCommand(..), DbQuote(..), EntityField(..))
import           Config                      (HasConfig)

type ChatBotDB m = (CommandsDb m, QuotesDb m)

class Monad m => CommandsDb m where
    insertCommand :: ChannelName -> Text -> Text -> m (Entity DbCommand)
    getCommand :: ChannelName -> Text -> m (Maybe (Entity DbCommand))
    deleteCommand :: ChannelName -> Text -> m ()
    getAllCommands :: m [Entity DbCommand]

class Monad m => QuotesDb m where
    insertQuote :: ChannelName -> Text -> m (Entity DbQuote)
    getQuote :: ChannelName -> Int -> m (Maybe (Entity DbQuote))
    getQuoteByPK :: Int64 -> m (Maybe DbQuote)
    deleteQuote :: ChannelName -> Int -> m ()

instance (HasConfig c, MonadIO m) => CommandsDb (AppT' e m c) where
    insertCommand (ChannelName channel) commandName commandText = do
        $(logDebug) "insertCommand" ["channel" .= channel, "name" .= commandName, "text" .= commandText]
        let q = DbCommand channel commandName commandText
        k <- runDb $ insert q
        pure $ Entity k q
    getCommand (ChannelName channel) commandName = do
        $(logDebug) "getCommand" ["channel" .= channel, "name" .= commandName]
        runDb $ selectFirst [DbCommandChannel P.==. channel, DbCommandName P.==. commandName] []
    deleteCommand (ChannelName channel) commandName = do
        $(logDebug) "deleteCommand" ["channel" .= channel, "name" .= commandName]
        runDb $ delete $ from $ \command ->
            where_ (
                (command ^. DbCommandChannel) ==. val channel
                &&.
                (command ^. DbCommandName) ==. val commandName
             )
    getAllCommands = do
        $(logDebug_) "getAllCommands"
        runDb $ select $ from $ \command -> pure command

instance (HasConfig c, MonadIO m) => QuotesDb (AppT' e m c) where
    insertQuote (ChannelName channel) quoteText = do
        $(logDebug) "insertQuote" ["channel" .= channel, "quoteText" .= quoteText]
        -- insert into quotes values (
        --   channel, quote, select count(*) + 1 from quotes where channel = channel
        -- )
        runDb $ do
            qid <- fmap length $ select $ from $ \quote -> do
                     where_ $ (quote ^. DbQuoteChannel) ==. val channel
                     return quote
            let q = DbQuote channel quoteText qid
            k <- insert q
            pure $ Entity k q
    getQuote (ChannelName channel) qid = do
        $(logDebug) "getQuote" ["channel" .= channel, "qid" .= qid]
        -- select * from quote where channel = channel and qid = qid
        runDb $ selectFirst [DbQuoteChannel P.==. channel, DbQuoteQid P.==. qid] []
    getQuoteByPK qid = do
        $(logDebug) "getQuote" ["id" .= qid]
        -- select * from quote where id = qid
        runDb $ P.get (toSqlKey qid)
    deleteQuote (ChannelName channel) qid = do
        $(logDebug) "deleteQuote" ["channel" .= channel, "qid" .= qid]
        -- delete from quote where channel = channel and qid = qid
        runDb $ delete $ from $ \quote ->
            where_ (
                (quote ^. DbQuoteChannel) ==. val channel
                &&.
                (quote ^. DbQuoteQid) ==. val qid
             )
