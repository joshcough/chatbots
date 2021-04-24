
module Settings where

import           Prelude            (error)
import           Protolude

import qualified Data.Text          as T
import           Safe               (readMay)
import           System.Environment (getEnv, lookupEnv)

-- | Looks up a text setting in the environment, with a provided default
lookupTextSetting :: Text -> Text -> IO Text
lookupTextSetting env def = maybe def T.pack <$> lookupEnv (T.unpack env)

-- |
lookupRequiredSetting :: Text -> IO Text
lookupRequiredSetting e = T.pack <$> getEnv (T.unpack e)

-- |
lookupOptionalSetting :: Text -> IO (Maybe Text)
lookupOptionalSetting e = fmap T.pack <$> lookupEnv (T.unpack e)

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupReadableSetting :: Read a => Text -> a -> IO a
lookupReadableSetting env def = do
  maybeValue <- fmap T.pack <$> lookupEnv (T.unpack env)
  maybe
    (return def)
    (\str -> maybe (handleFailedRead str) return (readMay $ T.unpack str))
    maybeValue
 where
  handleFailedRead str =
    error . T.unpack $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]
