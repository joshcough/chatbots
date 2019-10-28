{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (
    module Config
  , AppT
  , AppT'
  , AppTEnv
  , AppTEnv'
  , App
  , ChatBotM
  , runAppT
  , runAppTAndThrow
  , runAppTInTestAndThrow
  , runAppTInTest
  , runDb
  ) where

import Protolude hiding (fromException)

import Control.Lens ((^.), view)
import Control.Monad (when)
import Control.Monad.Except (ExceptT(..), MonadError(..), liftIO, runExceptT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT(..))
import Data.Aeson ((.=))
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Network.HTTP.Nano (HttpError)
import Web.Rollbar (ToRollbarEvent(..), rollbar)

import Config (Config(..), HasConfig, configPool)
import Error
import Logging
import Util.Utils (tShow)

import Network.HTTP.Nano.Types (HasHttpCfg)
import Web.Rollbar.Types (HasRollbarCfg)

---
---
---

type ChatBotM m = (MonadError ChatBotError m, MonadLoggerJSON m)

type AppT m = AppT' ChatBotError m Config
type AppTEnv' e m r = ReaderT r (ExceptT e (LoggingJSONT m))
type AppTEnv m r = AppTEnv' ChatBotError m r
type AppT' e m r = AppTEnv' e m r

type App = AppT IO

-- | Executes the given computation in AppT, logging to stdout with log level configured in the
--   context (via `HasLoggingCfg`), and sending errors to Rollbar (using `HasRollbarCfg`).
runAppT :: forall err r a.
     (FromException err, ClassifiedError err, ToRollbarEvent err, Show err, HasHttpCfg r, HasRollbarCfg r, HasLoggingCfg r)
  => AppT' err IO r a
  -> r
  -> IO (Either err a)
runAppT = runAppT' $ \err -> do
    $(logError) "Uncaught app error" ["error" .= tShow err]
    when (isUnexpected err) (rollbar $ toRollbarEvent err)

runAppTAndThrow :: (HasHttpCfg r, HasRollbarCfg r, HasLoggingCfg r) => r -> AppTEnv IO r a -> IO a
runAppTAndThrow r app = runAppT app r >>= either throwChatBotError return

runAppTInTestAndThrow :: HasLoggingCfg r => r -> AppTEnv IO r a -> IO a
runAppTInTestAndThrow config app' = runAppTInTest app' config >>= either throwChatBotError return

throwChatBotError :: Show b => AppError b -> IO a
throwChatBotError (AppUnexpectedException e) = throwIO e
throwChatBotError ae = throwIO . fmap (ErrorCall . show) $ ae

-- | Runs without rollbar
runAppTInTest ::
     (HasLoggingCfg r, FromException err, Show err)
  => AppT' err IO r a
  -> r
  -> IO (Either err a)
runAppTInTest = runAppT' $ \err -> $(logError) "Uncaught app error" ["error" .= tShow err]

-- |
runAppT' ::
     (FromException err, HasLoggingCfg r)
  => (err -> AppT' HttpError IO r ())
  -> AppT' err IO r a
  -> r
  -> IO (Either err a)
runAppT' onError action context = do
    let handleErrorCallingRollbar :: Either HttpError () -> IO ()
        handleErrorCallingRollbar = either print (const $ return ())
    res <- run level sourceVersion context action `catch` (pure . Left . fromException)
    either (\e -> run level sourceVersion context (onError e) >>= handleErrorCallingRollbar) (const $ pure ()) res
    pure res
    where
        level = context ^. (loggingCfg . logLevel)
        sourceVersion = context ^. (loggingCfg . logSourceVersion)

run :: LogLevel
     -> Maybe SourceVersion
     -> r
     -> ReaderT r (ExceptT e (LoggingJSONT m)) a
     -> m (Either e a)
run level sourceVersion context f =
    (runStdoutLoggingJSONT level sourceVersion . runExceptT) (runReaderT f context)

-- |
runDb :: (HasConfig c, MonadReader c m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = view configPool >>= liftIO . runSqlPool query
