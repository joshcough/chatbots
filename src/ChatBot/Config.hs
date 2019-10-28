{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module ChatBot.Config
  (
    ChannelName, mkChannelName, getChannelName
  , ChatBotConfig(..)
  , ChatBotExecutionConfig(..)
  , ChatBotFrontendMessage(..)
  , acquireChatBotExecutionConfig
  , configFromEnv
  , configFromFile
  ) where

import           Prelude                      (userError)
import           Protolude

import           Control.Concurrent.Chan      (Chan)
import           Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO)
import           Control.Lens.TH              (makeClassy)
import           Data.Aeson                   (FromJSON, ToJSON, Value,
                                               eitherDecode)
import qualified Data.ByteString.Lazy         as B
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import qualified Settings                     as S

import           ChatBot.Models               (ChannelName, ChatMessage',
                                               getChannelName, mkChannelName)

data ChatBotConfig = ChatBotConfig
  { _cbConfigNick :: Text
  , _cbConfigPass :: Text
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

makeClassy ''ChatBotConfig

data ChatBotFrontendMessage
  = ConnectTo ChannelName
  | DisconnectFrom ChannelName
  deriving (Eq, Generic, Ord, Show, ToJSON, FromJSON)

data ChatBotExecutionConfig = ChatBotExecutionConfig {
    _cbecOutputChan :: TChan (ChatMessage' Value)
  , _cbecInputChan  :: Chan ChatBotFrontendMessage
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
  return ChatBotConfig { .. }

-- |
acquireChatBotExecutionConfig :: IO ChatBotExecutionConfig
acquireChatBotExecutionConfig = ChatBotExecutionConfig <$> newBroadcastTChanIO <*> newChan
