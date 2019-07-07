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

class Monad m => QuotesDb m where
    insertQuote :: ChannelName -> Text -> m (Entity DbQuote)
    getQuote :: ChannelName -> Int -> m (Maybe (Entity DbQuote))
    getQuoteByPK :: Int64 -> m (Maybe DbQuote)
    deleteQuote :: ChannelName -> Int -> m ()

instance MonadIO m => QuotesDb (AppT' e m) where
    insertQuote channel quoteText = do
        $(logDebug) "insertQuote" ["channel" .= channel, "quoteText" .= quoteText]
        runDb $ insertQuote channel quoteText
    getQuote channel qid = do
        $(logDebug) "getQuote" ["channel" .= channel, "qid" .= qid]
        runDb $ getQuote channel qid
    getQuoteByPK qid = do
        $(logDebug) "getQuote" ["id" .= qid]
        runDb $ getQuoteByPK qid
    deleteQuote channel qid = do
        $(logDebug) "deleteQuote" ["channel" .= channel, "qid" .= qid]
        runDb $ deleteQuote channel qid

instance MonadIO m => QuotesDb (SqlPersistT m) where
    -- insert into quotes values (
    --   channel, quote, select count(*) + 1 from quotes where channel = channel
    -- )
    insertQuote (ChannelName channel) quoteText = do
        qid <- fmap length $ select $ from $ \quote -> do
                 where_ $ (quote ^. DbQuoteChannel) ==. val channel
                 return quote
        let q = DbQuote channel quoteText qid
        k <- insert q
        pure $ Entity k q
    -- select * from quote where channel = channel and qid = qid
    getQuote (ChannelName channel) qid =
        selectFirst [DbQuoteChannel P.==. channel, DbQuoteQid P.==. qid] []
    -- select * from quote where id = qid
    getQuoteByPK qid = P.get (toSqlKey qid)
    -- delete from quote where channel = channel and qid = qid
    deleteQuote (ChannelName channel) qid = delete $ from $ \quote ->
        where_ (
            (quote ^. DbQuoteChannel) ==. val channel
            &&.
            (quote ^. DbQuoteQid) ==. val qid
         )
