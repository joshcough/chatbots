module MooPostgreSQL
    ( runUpgrade
    )
where

import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Prelude  hiding (lookup)
import System.Exit

import Data.Text (Text, unpack)
import Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import Moo.Core
import Moo.Main

runUpgrade :: Text -> IO ()
runUpgrade connStr = do
  let args = ["upgrade"]
  (_, opts, _) <- procArgs args
  loadedConf <- loadConfiguration $ _configFilePath opts
  case loadedConf of
    Left e -> putStrLn e >> exitFailure
    Right conf -> do
      connection <- connectPostgreSQL $ unpack connStr
      let backend = hdbcBackend connection
          parameters = makeParameters conf backend
      mainWithParameters args parameters