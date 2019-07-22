{-# LANGUAGE NoDeriveAnyClass, GeneralizedNewtypeDeriving, QuasiQuotes #-}

module ChatBot.DatabaseModels where

import Protolude

import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.TH              (mkDeleteCascade, mkPersist, persistLowerCase, share, sqlSettings)
import           Data.Text                        (Text)

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
|]