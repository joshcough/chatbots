module Helpers (withDB) where

import Prelude (IO)
import Protolude

import Config (Config(..), acquireConfigWithConnStr)
import Data.String.Conversions (cs)
import Data.Text (pack)
import Database.Persist.Sql (rawExecute)
import Database.PostgreSQL.Simple.Options (Options(..))
import Database.Postgres.Temp (DB(..), startLocalhost, stop, defaultOptions)
import Test.Hspec
import Turtle.Prelude
import Turtle.Shell
import Types (runAppToIO, runDb)

---
--- Setup and teardown helpers
---

createDatabase :: IO (DB, Config)
createDatabase = do
    Right db@DB{..} <- startLocalhost defaultOptions
    let connStr = toConnectionString options
    sh $ do export "DBM_DATABASE" connStr
            proc "moo-postgresql" ["upgrade"] empty
    config <- acquireConfigWithConnStr connStr
    return (db, config)

-- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
truncateDb :: Config -> IO ()
truncateDb config = runAppToIO config . runDb $ truncateTables
    where
    tables = ["commands", "quotes"]
    truncateTables = rawExecute (pack $ "TRUNCATE TABLE " ++ intercalate ", " tables ++ " RESTART IDENTITY CASCADE") []

toConnectionString :: Options -> Text
toConnectionString Options {..} = "postgresql://" <> user <> "@" <> host <> ":" <> port <> "/" <> cs oDbname
    where
    host = cs $ fromMaybe "localhost" oHost
    user = cs $ fromMaybe "josh" oUser
    port = show $ fromMaybe 5432 oPort

withDB :: SpecWith (DB, Config) -> Spec
withDB = beforeAll createDatabase . afterAll (void . stop . fst) . after (truncateDb . snd)
