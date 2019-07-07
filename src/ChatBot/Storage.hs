module ChatBot.Storage (
    QuotesDb(..)
  ) where

import Protolude hiding (from)

import           Control.Monad.Except        (MonadIO)
import           Data.Aeson                  ((.=))
import           Database.Persist.Postgresql (insert)
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import           Logging                     (logDebug)
import           Types                       (AppT', runDb)

import           ChatBot.Config              (ChannelName(..))
import           ChatBot.DatabaseModels      (DbQuote(..), EntityField( DbQuoteChannel, DbQuoteQid ))
import           Config                      (HasConfig)

class Monad m => QuotesDb m where
    insertQuote :: ChannelName -> Text -> m (Entity DbQuote)
    getQuote :: ChannelName -> Int -> m (Maybe (Entity DbQuote))
    getQuoteByPK :: Int64 -> m (Maybe DbQuote)
    deleteQuote :: ChannelName -> Int -> m ()

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
