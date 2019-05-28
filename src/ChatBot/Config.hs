{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}

module ChatBot.Config
  (
    ChannelName(..)
  , ChatBotConfig(..)
  , ChatBotExecutionConfig(..)
  , ChatBotFrontendMessage(..)
  , configFromFile
  ) where

import           Control.Concurrent.Chan (Chan)
import           Control.Lens.TH         (makeClassy)
import           Data.Aeson              (ToJSON, FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy    as B
import           Data.Text               (Text)
import           Irc.RawIrcMsg           (RawIrcMsg)
import           GHC.Generics            (Generic)
import           Web.HttpApiData         (FromHttpApiData(..))

newtype ChannelName = ChannelName { _unChannelName :: Text }
    deriving (Eq, Generic, Ord, Show, ToJSON, FromJSON)

makeClassy ''ChannelName

instance FromHttpApiData ChannelName where
    parseUrlPiece = pure . ChannelName

data ChatBotConfig = ChatBotConfig
  { _cbConfigNick :: Text
  , _cbConfigPass :: Text
  , _cbConfigChannels :: [Text]
  } deriving (Eq, Generic, Show, ToJSON, FromJSON)

makeClassy ''ChatBotConfig

data ChatBotFrontendMessage = ConnectTo ChannelName | DisconnectFrom ChannelName
    deriving (Eq, Generic, Ord, Show, ToJSON, FromJSON)

data ChatBotExecutionConfig = ChatBotExecutionConfig {
    _cbecOutputChan :: Chan RawIrcMsg
  , _cbecInputChan :: Chan ChatBotFrontendMessage
}

makeClassy ''ChatBotExecutionConfig

configFromFile :: FilePath -> IO ChatBotConfig
configFromFile filePath = do
    configFileContents <- B.readFile filePath
    either (ioError . userError) return (eitherDecode configFileContents)
