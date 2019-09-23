{-# LANGUAGE NoDeriveAnyClass, GeneralizedNewtypeDeriving, QuasiQuotes #-}

module Auth.DatabaseModels where

import Prelude (error)
import Protolude

import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings] [persistLowerCase|
DbUser json sql=users
    name           Text
    email          Text
    hashedPassword Text
    admin          Bool
    DbUserUniqueEmail email
    DbUserLogin email hashedPassword
    deriving Show Eq
|]
