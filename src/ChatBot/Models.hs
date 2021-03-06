{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatBot.Models
  (
    ChatMessage'(..)
  , ChatMessage
  , ChatUser(..)
  , ChatUserName(..)
  , ChannelName, mkChannelName, getChannelName, getChannelNameHashed
  , Command(..)
  , Quote(..)
  , Stream(..)
  , trollabotUser
  ) where

import           Protolude

import           Control.Lens.TH        (makeClassy)
import           Control.Monad          (mzero)
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Sql   (PersistFieldSql (..), SqlType (..))
import           Database.Persist.Types (PersistValue (..))
import           GHC.Generics           (Generic)
import           Irc.Identifier         (Identifier, idText, mkId)
import           Irc.RawIrcMsg          (RawIrcMsg (..), TagEntry (..))
import           Irc.UserInfo           (UserInfo (..))
import           Web.HttpApiData        (FromHttpApiData (..))

newtype ChannelName = ChannelName { _unChannelName :: Text }
    deriving stock (Eq, Ord, Read, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

makeClassy ''ChannelName

mkChannelName :: Text -> ChannelName
mkChannelName t = ChannelName $ if T.head t == '#' then T.drop 1 t else t

getChannelName :: ChannelName -> Text
getChannelName = _unChannelName

getChannelNameHashed :: ChannelName -> Text
getChannelNameHashed c = "#" <> getChannelName c

data Stream = Stream
  { _streamId :: Int64
  , _streamChannelName :: ChannelName
  }
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

makeClassy ''Stream

instance FromHttpApiData ChannelName where
  parseUrlPiece = pure . ChannelName

data ChatUser = ChatUser
  { cuUserName :: ChatUserName
  , cuMod :: Bool
  , cuSubscriber :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

trollabotUser :: ChatUserName
trollabotUser = ChatUserName "trollabot"

newtype ChatUserName = ChatUserName {
  cunName :: Text
} deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance PersistFieldSql ChatUserName where
  sqlType _ = SqlString

instance PersistField ChatUserName where
  toPersistValue (ChatUserName u) = PersistText u
  fromPersistValue (PersistText t) = pure $ ChatUserName t
  fromPersistValue v = Left $ "wrong type for ChatUserName: " <> show v

data ChatMessage' a = ChatMessage
  { cmUser :: ChatUser
  , cmChannel :: ChannelName
  , cmBody :: Text
  , cmRawMessage :: a
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (FromJSON, ToJSON)

type ChatMessage = ChatMessage' RawIrcMsg

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

data Command = Command
  { commandChannel :: ChannelName
  , commandName :: Text
  , commandBody :: Text
  }
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Quote = Quote
  { quoteChannel :: ChannelName
  , quoteBody :: Text
  , quoteUser :: ChatUserName
  , quoteQid :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
