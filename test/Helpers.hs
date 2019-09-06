module Helpers (withDB) where

import Prelude (IO)
import Protolude

import Config (Config(..), acquireConfigWithConnStr)
import Data.String.Conversions (cs)
import Data.Text (pack)
import Database.Persist.Sql (rawExecute)
import Database.PostgreSQL.Simple.Options (Options(..))
import Database.Postgres.Temp (DB(..), defaultOptions)
import qualified Database.Postgres.Temp as PG
import Settings (lookupReadableSetting)
import System.IO (IOMode(WriteMode), openFile)
import Test.Hspec
import Turtle.Prelude hiding (stderr, stdout)
import Turtle.Shell
import Types (runAppToIO, runDb)

--
-- NOTE: if having trouble with db, do this: DBLOGGING=VERBOSE stack test
--

---
--- Setup and teardown helpers
---
data DBLogging = VERBOSE | SILENT deriving Read

withDB :: SpecWith ((DB, IO ()), Config) -> Spec
withDB = beforeAll createDatabase . afterAll (snd . fst) . after (truncateDb . snd)
  where
    createDatabase :: IO ((DB, IO ()), Config)
    createDatabase = do
      verbosity <- lookupReadableSetting "DBLOGGING" SILENT
      (db, io) <- startDb verbosity
      config <- acquireConfigWithConnStr $ toConnectionString db
      return ((db, io), config)
    
    -- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
    truncateDb :: Config -> IO ()
    truncateDb config = runAppToIO config . runDb $ truncateTables
      where
      tables = ["commands", "quotes"]
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
      restore (setupDB db >> pure (db, cleanup db)) `onException` cleanup db
      where
        devNull = openFile "/dev/null" WriteMode
        setupDB db =
          sh $ do
            export "DBM_DATABASE" $ toConnectionString db
            proc "moo-postgresql" ["upgrade"] empty
        cleanup = void . PG.stop
