module Helpers where

import Prelude (IO)
import Protolude

import Config (Config(..), acquireConfig)
import Data.Text (pack)
import Database.Persist.Sql (rawExecute)
import Types (runAppToIO, runDb)

---
--- Setup and teardown helpers
---

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
