module Helpers where

import Prelude (IO)
import Protolude

import Config (Config(..), acquireConfigWithConnStr)
import Data.String.Conversions (cs)
import Data.Text (pack)
import Database.Persist.Sql (rawExecute)
import Database.PostgreSQL.Simple.Options (Options(..))
import Database.Postgres.Temp (DB(..), startLocalhost, stop, defaultOptions)
import Turtle.Prelude
import Turtle.Shell
import Types (runAppToIO, runDb)

---
--- Setup and teardown helpers
---

setupTeardown :: (Config -> IO ()) -> IO ()
setupTeardown runTestsWith = do
    (db, connStr) <- createDatabase
    putStrLn $ "CONNECTION STRING: " <> connStr
    config <- acquireConfigWithConnStr connStr
    setupTeardownDb config
    runTestsWith config
    void $ stop db

createDatabase :: IO (DB, Text)
createDatabase = do
    Right db@DB{..} <- startLocalhost defaultOptions
    let connStr = myToConnectionString options
    sh $ do export "DBM_DATABASE" connStr
            proc "moo-postgresql" ["upgrade"] empty
    return (db, connStr)

-- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
setupTeardownDb :: Config -> IO ()
setupTeardownDb config = runAppToIO config . runDb $ truncateTables
    where
    tables = ["commands", "quotes"]
    truncateTables = rawExecute (pack $ "TRUNCATE TABLE " ++ intercalate ", " tables ++ " RESTART IDENTITY CASCADE") []

myToConnectionString :: Options -> Text
myToConnectionString Options {..} = "postgresql://" <> user <> "@" <> host <> ":" <> port <> "/" <> cs oDbname
    where
    host = cs $ fromMaybe "localhost" oHost
    user = cs $ fromMaybe "josh" oUser
    port = show $ fromMaybe 5432 oPort

