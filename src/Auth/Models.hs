{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Auth.Models (
    CreateUser(..)
  , Login(..)
  , Token(..)
  , User(..)
  , UserToken(..)
  ) where

import           Protolude

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Int            (Int64)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Servant.Auth.Server
import           Web.HttpApiData     (FromHttpApiData (..))

data Login = Login {
    loginEmail    :: Text
  , loginPassword :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data User = User {
    userId    :: Int64
  , userName  :: Text
  , userEmail :: Text
  , userAdmin :: Bool
} deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

data CreateUser = CreateUser {
    createUserName     :: Text
  , createUserEmail    :: Text
  , createUserPassword :: Text
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype Token = Token { getToken :: Text }
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype UserToken = UserToken { userTokenToken :: Token }
   deriving stock (Eq, Ord, Read, Show, Generic)
   deriving anyclass (FromJSON, ToJSON)

instance FromHttpApiData UserToken where
  parseUrlPiece = pure . UserToken . Token
