{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatBot.Models
  (
    ChatMessage(..)
  , ChatUser(..)
  , ChannelName(..)
  , Command(..)
  , Question(..)
  , Quote(..)
  ) where

import Protolude

import Control.Lens.TH (makeClassy)
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Irc.Identifier (Identifier, idText, mkId)
import Irc.RawIrcMsg (RawIrcMsg(..), TagEntry(..))
import Irc.UserInfo (UserInfo(..))
import Web.HttpApiData (FromHttpApiData(..))

newtype ChannelName = ChannelName { _unChannelName :: Text }
    deriving stock (Eq, Ord, Read, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

makeClassy ''ChannelName

instance FromHttpApiData ChannelName where
    parseUrlPiece = pure . ChannelName

data ChatUser = ChatUser {
   cuUserName :: UserInfo
 , cuMod :: Bool
 , cuSubscriber :: Bool
} deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChatMessage = ChatMessage {
    cmUser :: ChatUser
  , cmChannel :: ChannelName
  , cmBody :: Text
  , cmRawMessage :: RawIrcMsg
} deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

data Command = Command {
    commandChannel :: ChannelName
  , commandName :: Text
  , commandBody :: Text
} deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Quote = Quote {
    quoteChannel :: ChannelName
  , quoteBody :: Text
  , quoteQid :: Int
} deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Question = Question {
    questionChannel :: ChannelName
  , questionBody :: Text
  , questionQid :: Int
} deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
