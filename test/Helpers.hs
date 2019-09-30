module Helpers (withDB) where

--import Prelude (IO, String)
import Protolude

import Config (Config(..), acquireConfigWithConnStr)
import Data.String.Conversions (cs)
import Data.Text (pack) --, unpack)
--import Database.Persist (Entity(..))
import Database.Persist.Sql (rawExecute) --, rawSql)
import Database.PostgreSQL.Simple.Options (Options(..))
import Database.Postgres.Temp (DB(..), defaultOptions)
import qualified Database.Postgres.Temp as PG
import Settings (lookupReadableSetting)
import System.IO (IOMode(WriteMode), openFile)
import Test.Hspec
import Types (runAppToIO, runDb)
import qualified MooPostgreSQL as Moo

--
-- NOTE: if having trouble with db, do this: DBLOGGING=VERBOSE stack test
-- Also: for quieter tests: LOG_LEVEL=LevelError stack test
--

---
--- Setup and teardown helpers
---
data DBLogging = VERBOSE | SILENT deriving Read
data TestType = Local | Travis deriving Read

withDB :: SpecWith (IO (), Config) -> Spec
withDB = beforeAll getDatabase . afterAll fst . after (truncateDb . snd)
  where
    getDatabase :: IO (IO (), Config)
    getDatabase = lookupReadableSetting "TEST_TYPE" Local >>= \case
        Local -> createTmpDatabase
        Travis -> do
          let connStr = "postgresql://postgres@localhost/travis_ci_test"
          config <- acquireConfigWithConnStr connStr
          Moo.runUpgrade connStr
          pure (pure (), config)

    createTmpDatabase :: IO (IO (), Config)
    createTmpDatabase = do
      verbosity <- lookupReadableSetting "DBLOGGING" SILENT
      (db, cleanup) <- startDb verbosity
      config <- acquireConfigWithConnStr $ toConnectionString db
      return (cleanup, config)

    -- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
    truncateDb :: Config -> IO ()
    truncateDb config = runAppToIO config . runDb $ truncateTables
      where
      tables = ["users", "commands", "questions", "quotes"]
      truncateStatement = "TRUNCATE TABLE " <> intercalate ", " tables <> " RESTART IDENTITY CASCADE"
      truncateTables = rawExecute (pack truncateStatement) []

    toConnectionString :: DB -> Text
    toConnectionString DB {..} = "postgresql://" <> user <> "@" <> host <> ":" <> show port <> "/" <> cs oDbname
      where
        Options {..} = options
        host = cs $ fromMaybe "localhost" oHost
        user = cs $ fromMaybe "josh" oUser

    startDb :: DBLogging -> IO (DB, IO ())
    startDb verbosity = mask $ \restore -> do
      (outHandle, errHandle) <- case verbosity of
        VERBOSE -> pure (stdout, stderr)
        SILENT -> (,) <$> devNull <*> devNull
      db <- PG.startWithHandles PG.Localhost defaultOptions outHandle errHandle >>= either throwIO pure
      restore (Moo.runUpgrade (toConnectionString db) >> pure (db, cleanup db)) `onException` cleanup db
      where
        devNull = openFile "/dev/null" WriteMode
        cleanup = void . PG.stop

{-
    -- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
    truncateDb :: Config -> IO ()
    truncateDb config = do
        tables <- fmap (unpack . entityVal) <$> selectTables config
        let truncateStatement = "TRUNCATE TABLE " <> intercalate ", " tables <> " RESTART IDENTITY CASCADE"
        runAppToIO config . runDb $ rawExecute (pack truncateStatement) []

    selectTables :: Config -> IO [Entity Text]
    selectTables config = runAppToIO config . runDb $ rawSql q []
      where
      q = "SELECT table_name FROM information_schema.tables WHERE table_schema='public' AND table_type='BASE TABLE';"
-}
