{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config (
    AwsConfig(..)
  , Config(..)
  , Environment(..)
  , acquireConfig
  , HasConfig(..)
  ) where

import Protolude
import Prelude (userError)

import           Control.Concurrent.Chan              (newChan)
import           Control.Lens                         ((<&>), set)
import           Control.Lens.TH                      (makeClassy)
import           Control.Monad.Logger                 (runNoLoggingT)
import           Control.Monad.Trans.AWS              (Credentials( Discover ), LogLevel( Debug ), envLogger, newEnv, newLogger)
import qualified Control.Monad.Trans.AWS              as AWS
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text, unpack)
import           Database.Persist.Postgresql          (ConnectionPool, createPostgresqlPool)
import           Database.PostgreSQL.Simple.Internal  (postgreSQLConnectionString)
import           Database.PostgreSQL.Simple.URL       (parseDatabaseUrl)
import           Network.HTTP.Nano                    (HasHttpCfg(..), HttpCfg(..), tlsManager)
import           Network.Wai.Handler.Warp             (Port)
import           Servant.Auth.Server                  (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings) --, generateKey)
import           System.IO                            (stdout)
import           Web.Rollbar                          (RollbarCfg(..), HasRollbarCfg(..))
import qualified Web.Rollbar                          as RB

import           ChatBot.Config                       (ChatBotConfig(..), ChatBotExecutionConfig(..), configFromEnv)
import           Logging                              (HasLoggingCfg, LoggingCfg)
import qualified Logging
import qualified KeyGen                               as KG
import qualified Settings                             as S

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | The AWS Config for our application
data AwsConfig = AwsConfig
  { _awsConfigS3RootUrl            :: Text
  , _awsConfigProverlaysBucketName :: Text
  , _awsConfigProverlaysBucketUrl  :: Text
  , _awsConfigEnv                  :: AWS.Env
  }
makeClassy ''AwsConfig

-- | The Config for our application
data Config = Config
    { _configPool :: ConnectionPool
    , _configEnv :: Environment
    , _configPort :: Port
    , _configHttp :: HttpCfg
    , _configCookies :: CookieSettings
    , _configJWT :: JWTSettings
    , _configAwsEnv :: AwsConfig
    , _configRollbar :: RollbarCfg
    , _configLogging :: LoggingCfg
    , _configChatBot :: ChatBotConfig
    , _configChatBotExecution :: ChatBotExecutionConfig
    }

makeClassy ''Config

instance HasHttpCfg Config where
    httpCfg = configHttp

instance HasRollbarCfg Config where
    rollbarCfg = configRollbar

instance HasLoggingCfg Config where
    loggingCfg = configLogging

---
---
---

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
    _configPort             <- S.lookupReadableSetting "PORT" 8081
    _configEnv              <- S.lookupReadableSetting "ENV" Development
    _configPool             <- acquirePool _configEnv
    _configHttp             <- mkHttp
    let _configCookies      = defaultCookieSettings
    _configJWT              <- acquireJWT _configEnv
    _configAwsEnv           <- acquireAwsConfig
    _configRollbar          <- mkRollbar
    _configLogging          <- mkLoggingCfg
    _configChatBot          <- configFromEnv
    _configChatBotExecution <- acquireChatBotExecutionConfig
    pure Config {..}

-- |
acquirePool :: Environment -> IO ConnectionPool
acquirePool env = flip makePool env =<< S.lookupRequiredSetting "DATABASE_URL"


-- |
acquireJWT :: Environment -> IO JWTSettings
acquireJWT env = defaultJWTSettings <$> mkJWT' env
    where
    mkJWT' Production = KG.readProdKey
    mkJWT' Development = KG.readDevKey
    mkJWT' Test = KG.readTestKey


-- | Allocates resources for AwsConfig
acquireAwsConfig :: IO AwsConfig
acquireAwsConfig = do
    _awsConfigS3RootUrl               <- S.lookupTextSetting "AWS_S3_ROOT_URL" "https://s3.amazonaws.com/"
    _awsConfigProverlaysBucketName    <- S.lookupTextSetting "PROVERLAYS_BUCKET" "proverlays"
    let _awsConfigProverlaysBucketUrl = _awsConfigS3RootUrl <> _awsConfigProverlaysBucketName <> "/"
    _awsConfigEnv                     <- do lgr <- newLogger Debug stdout
                                            newEnv Discover <&> set envLogger lgr
    return AwsConfig {..}

-- |
acquireChatBotExecutionConfig :: IO ChatBotExecutionConfig
acquireChatBotExecutionConfig = do
    _cbecOutputChan <- newChan
    _cbecInputChan <- newChan
    return ChatBotExecutionConfig{..}

-- | rollbar
mkRollbar :: IO RollbarCfg
mkRollbar = RollbarCfg  <$> (RB.AccessToken <$> S.lookupRequiredSetting "ROLLBAR_TOKEN")
                        <*> (RB.Environment <$> S.lookupRequiredSetting "ROLLBAR_ENVIRONMENT")
                        <*> (fmap . fmap) RB.Host (S.lookupOptionalSetting "ROLLBAR_HOST")
                        <*> (fmap . fmap) RB.CodeVersion (S.lookupOptionalSetting "SOURCE_VERSION")
                        <*> S.lookupReadableSetting "ROLLBAR_MUTE" False

-- |
mkHttp :: IO HttpCfg
mkHttp = HttpCfg <$> tlsManager

-- |
mkLoggingCfg :: IO LoggingCfg
mkLoggingCfg = Logging.fromEnv

-- | This function creates a 'ConnectionPool' for the given environment.
makePool :: Text -> Environment -> IO ConnectionPool
makePool dbUrl env = case postgreSQLConnectionString <$> parseDatabaseUrl (unpack dbUrl) of
    Nothing  -> throwIO (userError "DATABASE_URL malformed.")
    Just url -> runNoLoggingT $ createPostgresqlPool url (envPool env)

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8
