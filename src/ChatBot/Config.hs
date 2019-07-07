{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}

module ChatBot.Config
  (
    ChannelName(..)
  , ChatBotConfig(..)
  , ChatBotExecutionConfig(..)
  , configFromFile
  ) where

import Protolude
import Prelude (userError)

import           Control.Concurrent.Chan (Chan)
import           Control.Lens.TH         (makeClassy)
import           Data.Aeson              (ToJSON, FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy    as B
import           Data.Text               (Text)
import           Irc.RawIrcMsg           (RawIrcMsg)
import           GHC.Generics            (Generic)

import ChatBot.Models (ChannelName(..))

data ChatBotConfig = ChatBotConfig
  { _cbConfigNick :: Text
  , _cbConfigPass :: Text
  , _cbConfigChannels :: [Text]
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

makeClassy ''ChatBotConfig

newtype ChatBotExecutionConfig = ChatBotExecutionConfig {
    _cbecOutputChan :: Chan RawIrcMsg
}

makeClassy ''ChatBotExecutionConfig

configFromFile :: FilePath -> IO ChatBotConfig
configFromFile filePath = do
    configFileContents <- B.readFile filePath
    either (ioError . userError) return (eitherDecode configFileContents)
