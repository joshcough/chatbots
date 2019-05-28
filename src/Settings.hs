
module Settings where

import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import           Safe                        (readMay)
import           System.Environment          (getEnv, lookupEnv)

-- | Looks up a text setting in the environment, with a provided default
lookupTextSetting :: Text -> Text -> IO Text
lookupTextSetting env def = maybe def pack <$> lookupEnv (unpack env)

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupReadableSetting :: Read a => Text -> a -> IO a
lookupReadableSetting env def = do
    maybeValue <- fmap pack <$> lookupEnv (unpack env)
    maybe (return def) (\str -> maybe (handleFailedRead str) return (readMay $ unpack str)) maybeValue
  where
    handleFailedRead str =
        error . unpack $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]

-- |
lookupRequiredSetting :: Text -> IO Text
lookupRequiredSetting e = T.pack <$> getEnv (T.unpack e)

-- |
lookupOptionalSetting :: Text -> IO (Maybe Text)
lookupOptionalSetting e = fmap T.pack <$> lookupEnv (T.unpack e)
