module Util.DatabaseHelpers where

import           Protolude

import           Config                                 (Config (..), acquireConfigWithConnStr)
import           Data.String.Conversions                (ConvertibleStrings (..), cs)
import           Data.Text                              (pack)
import           Database.Persist.Sql                   (rawExecute)
import           Database.PostgreSQL.Simple.Options     (Options (..), defaultOptions)
import           Database.Postgres.Temp                 (DB, defaultConfig, verboseConfig)
import qualified Database.Postgres.Temp                 as PG
import           Database.Postgres.Temp.Internal.Config (optionsToConfig)
import           Settings                               (lookupReadableSetting)
import           Types                                  (runAppTInTestAndThrow, runDb)
import qualified Util.Migrator                          as Migrator (migrate)

--
-- NOTE: if having trouble with db, do this: DBLOGGING=VERBOSE stack test
-- Also: for quieter tests: LOG_LEVEL=LevelError stack test
--

---
--- Setup and teardown helpers
---
data DBLogging = VERBOSE | SILENT deriving (Eq, Read, Show)
data TestType = Local | Travis deriving (Eq, Read, Show)

getDatabase :: IO (IO (), Config)
getDatabase = lookupReadableSetting "TEST_TYPE" Local >>= \case
  Local -> createTmpDatabase
  Travis -> do
    let connStr = "postgresql://postgres@localhost/travis_ci_test"
    config <- acquireConfigWithConnStr connStr
    Migrator.migrate connStr
    pure (pure (), config)

createTmpDatabase :: IO (IO (), Config)
createTmpDatabase = do
  verbosity <- lookupReadableSetting "DBLOGGING" SILENT
  (db, cleanup) <- startDb verbosity
  let connStr = toConnectionString db
  config <- acquireConfigWithConnStr connStr
  return (cleanup, config)

toConnectionString :: DB -> Text
toConnectionString db = connStr
 where
  Options {..} = PG.toConnectionOptions db
  connStr :: Text
  connStr = "postgresql://" <> user' <> "@" <> host' <> ":" <> port' <> "/" <> dbname'
  host' = f "localhost" host
  user' = f "josh" user
  dbname' = f "postgres" user
  port' = f "5432" ((show <$> port) :: Last Text)
  f :: ConvertibleStrings a Text => a -> Last a -> Text
  f def fld = (cs $ fromMaybe def $ getLast fld) :: Text

startDb :: DBLogging -> IO (DB, IO ())
startDb verbosity = mask $ \restore -> do
  db <- PG.startConfig config >>= either throwIO pure
  let connStr = cs $ PG.toConnectionString db
  print connStr
  restore (Migrator.migrate connStr >> pure (db, cleanup db)) `onException` cleanup db
 where
  baseConfig = if verbosity == VERBOSE then verboseConfig else defaultConfig
  config = baseConfig <> optionsToConfig defaultOptions { port = Last $ Just 54321 }
  cleanup = void . PG.stop

-- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
truncateDb :: Config -> IO ()
truncateDb config = runAppTInTestAndThrow config . runDb $ truncateTables
 where
  tables = ["users", "commands", "quotes", "streams"]
  truncateStatement = "TRUNCATE TABLE " <> intercalate ", " tables <> " RESTART IDENTITY CASCADE"
  truncateTables = rawExecute (pack truncateStatement) []

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
