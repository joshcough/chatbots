module Helpers where

import Protolude
import Prelude (IO)

import           Data.Text                   (pack)
import           Database.Persist.Sql        (rawExecute)
import           Config                      (Config (..), acquireConfig)
import           Types                       (App, runAppTInTest, runDb)

---
--- Setup and teardown helpers
---

runAppToIO :: Config -> App a -> IO a
runAppToIO config app' = do
    result <- runAppTInTest app' config
    either (throwIO . fmap (const (ErrorCall "error"))) return result

setupTeardown :: (Config -> IO ()) -> IO ()
setupTeardown runTestsWith = do
    config <- getTestConfig
    setupTeardownDb config
    runTestsWith config

-- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
setupTeardownDb :: Config -> IO ()
setupTeardownDb config = runAppToIO config . runDb $ truncateTables
    where
    tables = ["quotes"]
    truncateTables = rawExecute (pack $ "TRUNCATE TABLE " ++ intercalate ", " tables ++ " RESTART IDENTITY CASCADE") []

getTestConfig :: IO Config
getTestConfig = acquireConfig
