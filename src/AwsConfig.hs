{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module AwsConfig (
    AwsConfig(..)
  , acquireAwsConfig
  ) where

import Protolude
import Control.Lens ((<&>), set)
import Control.Lens.TH (makeClassy)
import Control.Monad.Trans.AWS (Credentials(Discover), LogLevel(Debug), envLogger, newEnv, newLogger)
import qualified Control.Monad.Trans.AWS as AWS
import Data.Monoid ((<>))
import System.IO (stdout)
import qualified Settings as S

-- | The AWS Config for our application
data AwsConfig = AwsConfig
  { _awsConfigS3RootUrl  :: Text
  , _awsConfigBucketName :: Text
  , _awsConfigBucketUrl  :: Text
  , _awsConfigEnv        :: AWS.Env
  }
makeClassy ''AwsConfig

-- | Allocates resources for AwsConfig
acquireAwsConfig :: IO AwsConfig
acquireAwsConfig = do
    _awsConfigS3RootUrl     <- S.lookupTextSetting "AWS_S3_ROOT_URL" "https://s3.amazonaws.com/"
    _awsConfigBucketName    <- S.lookupTextSetting "PROVERLAYS_BUCKET" "proverlays"
    let _awsConfigBucketUrl = _awsConfigS3RootUrl <> _awsConfigBucketName <> "/"
    _awsConfigEnv           <- do lgr <- newLogger Debug stdout
                                  newEnv Discover <&> set envLogger lgr
    return AwsConfig {..}
