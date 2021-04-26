{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config (
    Config(..)
  , ConfigAndConnection(..)
  , Environment(..)
  , acquireConfig
  , acquireConfigWithConnStr
  , HasConfig(..)
  ) where

import           Prelude                             (userError)
import           Protolude

import           Control.Lens.TH                     (makeClassy)
import           Control.Monad.Logger                (runNoLoggingT)
import           Data.Text                           (Text, unpack)
import           Database.Persist.Postgresql         (ConnectionPool, createPostgresqlPool)
import           Database.PostgreSQL.Simple.Internal (postgreSQLConnectionString)
import           Database.PostgreSQL.Simple.URL      (parseDatabaseUrl)
import           Network.HTTP.Nano                   (HasHttpCfg (..), HttpCfg (..), tlsManager)
import           Network.Wai.Handler.Warp            (Port)
import qualified Network.WebSockets                  as WS
import           Servant.Auth.Server                 (CookieSettings, JWTSettings,
                                                      defaultCookieSettings, defaultJWTSettings)
import           Web.Rollbar                         (HasRollbarCfg (..), RollbarCfg (..))
import qualified Web.Rollbar                         as RB

import           ChatBot.Config                      (ChatBotConfig (..),
                                                      ChatBotExecutionConfig (..),
                                                      acquireChatBotExecutionConfig, configFromEnv)
import qualified KeyGen                              as KG
import           Logging                             (HasLoggingCfg (..), LoggingCfg)
import qualified Logging
import qualified Settings                            as S

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | The Config for our application
data Config = Config
  { _configHost :: Text
  , _configPort :: Port
  , _configConnStr :: Text
  , _configPool :: ConnectionPool
  , _configEnv :: Environment
  , _configHttp :: HttpCfg
  , _configCookies :: CookieSettings
  , _configJWT :: JWTSettings
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

data ConfigAndConnection = ConfigAndConnection
  { _configAndConnectionConfig :: Config
  , _configAndConnectionConn :: WS.Connection
  }

makeClassy ''ConfigAndConnection

instance HasRollbarCfg ConfigAndConnection where
  rollbarCfg = configAndConnectionConfig . configRollbar

instance HasHttpCfg ConfigAndConnection where
  httpCfg = configAndConnectionConfig . configHttp

instance HasLoggingCfg ConfigAndConnection where
  loggingCfg = configAndConnectionConfig . loggingCfg

instance HasConfig ConfigAndConnection where
  config = configAndConnectionConfig


---
---
---

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = acquireConfigWithConnStr =<< S.lookupRequiredSetting "DATABASE_URL"

acquireConfigWithConnStr :: Text -> IO Config
acquireConfigWithConnStr _configConnStr = do
  _configHost <- S.lookupTextSetting "HOSTNAME" "localhost"
  _configPort <- S.lookupReadableSetting "PORT" 8081
  _configEnv <- S.lookupReadableSetting "ENV" Development
  _configPool <- makePool _configConnStr _configEnv
  _configHttp <- mkHttp
  let _configCookies = defaultCookieSettings
  _configJWT <- acquireJWT _configEnv
  _configRollbar <- mkRollbar
  _configLogging <- mkLoggingCfg
  _configChatBot <- configFromEnv
  _configChatBotExecution <- acquireChatBotExecutionConfig
  pure Config { .. }

-- |
acquireJWT :: Environment -> IO JWTSettings
acquireJWT env = defaultJWTSettings <$> mkJWT' env
 where
  mkJWT' Production = KG.readProdKey
  mkJWT' Development = KG.readDevKey
  mkJWT' Test = KG.readTestKey

-- | rollbar
mkRollbar :: IO RollbarCfg
mkRollbar =
  RollbarCfg
    <$> (RB.AccessToken <$> S.lookupRequiredSetting "ROLLBAR_TOKEN")
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
  Nothing -> throwIO (userError "DATABASE_URL malformed.")
  Just url -> runNoLoggingT $ createPostgresqlPool url (envPool env)

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8
