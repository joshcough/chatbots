{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types (
    module Config
  , AppT
  , AppT'
  , App
  , ProverlaysM
  , runAppT
  , runAppTInTest
  , runDb
  ) where

import           Control.Lens                         (view, (^.))
import           Control.Monad                        (when)
import           Control.Monad.Except                 (ExceptT(..), MonadError(..), liftIO, runExceptT)
import           Control.Monad.Reader                 (MonadIO, MonadReader, ReaderT(..))
import           Data.Aeson                           ((.=))
import           Database.Persist.Sql                 (SqlPersistT, runSqlPool)
import           Network.HTTP.Nano                    (HttpError)
import           Web.Rollbar                          (ToRollbarEvent(..), rollbar)

import           Config                               (Config(..), configPool)
import           Error
import           Logging
import           Util.Utils                           (tShow)

---
---
---

type ProverlaysM m = (MonadError ProverlaysError m, MonadLoggerJSON m)

type AppT m = AppT' ProverlaysError m
type AppT' e m = ReaderT Config (ExceptT e (LoggingJSONT m))

type App = AppT IO

-- | Executes the given computation in AppT, logging to stdout with log level configured in the
--   context (via `HasLoggingCfg`), and sending errors to Rollbar (using `HasRollbarCfg`).
runAppT :: forall err a. (ClassifiedError err, ToRollbarEvent err, Show err) => AppT' err IO a -> Config -> IO (Either err a)
runAppT = runAppT' $ \err -> do
    $(logError) "Uncaught app error" ["error" .= tShow err]
    when (isUnexpected err) (rollbar $ toRollbarEvent err)

-- | Runs without rollbar
runAppTInTest :: forall err a. (ClassifiedError err, ToRollbarEvent err, Show err) => AppT' err IO a -> Config -> IO (Either err a)
runAppTInTest = runAppT' $ \err -> $(logError) "Uncaught app error" ["error" .= tShow err]

-- |
runAppT' :: forall err a. (ClassifiedError err, ToRollbarEvent err, Show err) =>
       (err -> AppT' HttpError IO ())
    -> AppT' err IO a
    -> Config
    -> IO (Either err a)
runAppT' onError action context = do
    let run :: forall e r. AppT' e IO r -> IO (Either e r)
        run f = (runStdoutLoggingJSONT level sourceVersion . runExceptT) (runReaderT f context)
        handleErrorCallingRollbar :: Either HttpError () -> IO ()
        handleErrorCallingRollbar = either print (const $ return ())
    res <- run action
    either (\e -> run (onError e) >>= handleErrorCallingRollbar) (const $ pure ()) res
    pure res
    where
        level = context ^. (loggingCfg . logLevel)
        sourceVersion = context ^. (loggingCfg . logSourceVersion)

-- |
runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = view configPool >>= liftIO . runSqlPool query
