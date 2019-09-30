{-# LANGUAGE NoDeriveAnyClass, GeneralizedNewtypeDeriving, QuasiQuotes #-}

module ChatBot.DatabaseModels where

import Prelude (error)
import Protolude

import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings] [persistLowerCase|
DbCommand json sql=commands
    channel Text
    name    Text
    body    Text
    deriving Show Eq Ord Generic

DbQuote json sql=quotes
    channel             Text
    text                Text
    qid                 Int
    deriving Show Eq Ord Generic

DbQuestion json sql=questions
    channel             Text
    text                Text
    qid                 Int
    deriving Show Eq Ord Generic
|]
