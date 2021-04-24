{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoDeriveAnyClass           #-}
{-# LANGUAGE QuasiQuotes                #-}

module Auth.DatabaseModels where

import           Protolude

import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.TH

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
