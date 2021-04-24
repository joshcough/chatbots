{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoDeriveAnyClass           #-}
{-# LANGUAGE QuasiQuotes                #-}

module ChatBot.DatabaseModels where

import           Protolude

import           ChatBot.Models                   (ChatUserName)
import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.TH

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings] [persistLowerCase|
DbStream json sql=streams
    name Text
    deriving Show Eq Ord Generic

DbCommand json sql=commands
    channel DbStreamId
    name    Text
    body    Text
    deriving Show Eq Ord Generic

DbQuote json sql=quotes
    channel             DbStreamId
    text                Text
    user_id             ChatUserName
    qid                 Int
    deriving Show Eq Ord Generic
|]
