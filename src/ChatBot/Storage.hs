module ChatBot.Storage (
    QuotesDb(..)
  ) where

import Protolude
import Prelude (error)

import           Control.Monad               (forM)
import           Control.Monad.Except        (MonadIO)
import           Database.Persist.Postgresql (insert)
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import           Database.Esqueleto

import           Types                       (AppT', runDb)
import           ChatBot.DatabaseModels      (DbQuote(..), DbQuoteId)

class Monad m => QuotesDb m where
    insertQuote :: Text -> Text -> m (Entity DbQuote)
    getQuote :: Text -> Int -> m (Entity DbQuote)
    deleteQuote :: Text -> Int -> m ()

{-
DbQuote json sql=quotes
    channel             Text
    text                Text
    number              Int
    deriving Show Eq
|]-}

--instance MonadIO m => QuotesDb (AppT' e m) where
--    insertQuote channel = runDb $ insertFile channel
--    getQuote channel = runDb . getQuote channel
--    deleteQuote channel = runDb . deleteQuote channel

instance MonadIO m => QuotesDb (SqlPersistT m) where
    insertQuote channel quote = error "todo"
    getQuote fid = error "todo"
    deleteQuote channel qid = error "todo"

