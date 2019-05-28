{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatBot.Models
  (
    ChatMessage(..)
  ) where

import Protolude

import           Control.Monad        (mzero)
import           Data.Aeson           (Value(..), ToJSON(..), FromJSON(..))
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Irc.Identifier       (Identifier, idText, mkId)
import           Irc.RawIrcMsg        (RawIrcMsg(..), TagEntry(..))
import           Irc.UserInfo         (UserInfo(..))

data ChatMessage = ChatMessage {
    cmUser :: Text
  , cmChannel :: Text
  , cmBody :: Text
  , cmRawMessage :: RawIrcMsg
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

instance FromJSON Identifier where
    parseJSON (String s) = pure $ mkId s
    parseJSON _ = mzero
instance ToJSON Identifier where
    toJSON i = String $ idText i

deriving instance Generic TagEntry
deriving instance FromJSON TagEntry
deriving instance ToJSON TagEntry

deriving instance Generic UserInfo
deriving instance FromJSON UserInfo
deriving instance ToJSON UserInfo

deriving instance Generic RawIrcMsg
deriving instance FromJSON RawIrcMsg
deriving instance ToJSON RawIrcMsg
