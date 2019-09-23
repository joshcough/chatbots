{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}

module ChatBot.Config
  (
    ChannelName(..)
  , ChatBotConfig(..)
  , ChatBotExecutionConfig(..)
  , ChatBotFrontendMessage(..)
  , configFromEnv
  , configFromFile
  ) where

import Prelude (userError)
import Protolude

import Control.Concurrent.Chan (Chan)
import Control.Lens.TH (makeClassy)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import GHC.Generics (Generic)
import Irc.RawIrcMsg (RawIrcMsg)
import qualified Settings as S

import ChatBot.Models (ChannelName(..))

data ChatBotConfig = ChatBotConfig
  { _cbConfigNick :: Text
  , _cbConfigPass :: Text
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

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

-- |
configFromEnv :: IO ChatBotConfig
configFromEnv = do
    _cbConfigNick <- S.lookupRequiredSetting "CHATBOT_NICK"
    _cbConfigPass <- S.lookupRequiredSetting "CHATBOT_PASS"
    return ChatBotConfig{..}