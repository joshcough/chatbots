{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Auth.Models (
    CreateUser(..)
  , Login(..)
  , MiniUser(..)
  , Token(..)
  , TokenLogin(..)
  , User(..)
  , UserData(..)
  , UserToken(..)
  , minify
  ) where

import Protolude

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Int               (Int64)
import           Data.Text              (Text)
import           Data.UUID              (UUID)
import           GHC.Generics           (Generic)
import           Servant.Auth.Server
import           Web.HttpApiData        (FromHttpApiData(..))

data Login = Login {
    loginEmail    :: Text
  , loginPassword :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype TokenLogin = TokenLogin {
    tokenLoginToken :: UUID
} deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data User = User {
    userId       :: Int64
  , userName     :: Text
  , userEmail    :: Text
} deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data MiniUser = MiniUser {
    miniUserId     :: Int64
  , miniUserName   :: Text
} deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

minify :: User -> MiniUser
minify User{..} = MiniUser userId userName

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

class UserData a where
    getUserId :: a -> Int64

instance UserData User where
    getUserId = userId

instance UserData MiniUser where
    getUserId = miniUserId
