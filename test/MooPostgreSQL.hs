module MooPostgreSQL
  ( runUpgrade
  ) where

import           Data.Text                               (Text, unpack)
import           Database.HDBC.PostgreSQL                (connectPostgreSQL)
import           Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import           Moo.Core                                (CommandOptions (..),
                                                          loadConfiguration,
                                                          makeParameters)
import           Moo.Main                                (mainWithParameters,
                                                          procArgs)
import           Protolude
import           System.Exit                             (exitFailure)

runUpgrade :: Text -> IO ()
runUpgrade connStr = do
  let args = ["upgrade"]
  (_, opts, _) <- procArgs args
  loadConfiguration (_configFilePath opts) >>= \case
    Left e -> putStrLn e >> exitFailure
    Right conf -> do
      connection <- connectPostgreSQL $ unpack connStr
      mainWithParameters args $ makeParameters conf (hdbcBackend connection)
