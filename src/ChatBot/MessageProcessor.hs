{-# LANGUAGE UndecidableInstances #-}

module ChatBot.MessageProcessor
  ( MessageProcessor(..)
  , Sender(..)
  , authorize
  ) where

import Protolude

import Control.Concurrent.Chan (writeChan)
import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Irc.Commands (ircCapReq, ircJoin, ircNick, ircPass, ircPing, ircPong, ircPrivmsg)
import qualified Irc.Identifier as Irc
import Irc.Message (IrcMsg(..), cookIrcMsg)
import Irc.RawIrcMsg (RawIrcMsg(..), renderRawIrcMsg)
import qualified Irc.UserInfo as Irc
import qualified Network.WebSockets as WS
import Text.Trifecta (Result(..), parseString, whiteSpace)

import ChatBot.Commands (BotCommand(..), Response(..), builtinCommands, getCommandFromDb)
import ChatBot.Config (ChatBotConfig(..), ChatBotExecutionConfig(..))
import ChatBot.Models (ChannelName(..), ChatMessage(..))
import Config (Config(..), ConfigAndConnection(..), HasConfig(..))
import Error (ChatBotError)
import Types (AppTEnv')

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text.IO as T
import Control.Lens (view)
import ChatBot.Storage (CommandsDb, QuotesDb)

type App = AppTEnv' ChatBotError IO ConfigAndConnection

class Sender m where
  send :: RawIrcMsg -> m ()

instance Sender App where
  send msg = do
    ConfigAndConnection _ conn <- ask
    liftIO $ WS.sendTextData conn (renderRawIrcMsg msg)


class Monad m => MessageProcessor m where
    processMessage :: RawIrcMsg -> m ()

instance (Monad m, MonadIO m, QuotesDb m, CommandsDb m, MonadReader c m, HasConfig c, Sender m) => MessageProcessor m
    where
    processMessage rawIrcMsg = processMessage' (cookIrcMsg rawIrcMsg)
      where
        processMessage' (Ping xs) = send (ircPong xs)
        processMessage' (Privmsg userInfo channelName msgBody)
          | T.take 1 msgBody == "!" = processUserMessage rawIrcMsg userInfo channelName msgBody
        processMessage' Privmsg{} = return () -- just a regular user message.
        processMessage' _ = do
          liftIO $ T.putStr "couldn't process message: "
          liftIO $ T.putStrLn . cs $ encodePretty rawIrcMsg

processUserMessage ::
    (MonadIO m, QuotesDb m, CommandsDb m, MonadReader c m, HasConfig c, Sender m) =>
    RawIrcMsg -> Irc.UserInfo -> Irc.Identifier -> Text -> m ()
processUserMessage rawIrcMsg userInfo channelName msgBody = do
  conf <- view config
  let outputChan = _cbecOutputChan $ _configChatBotExecution conf
  case _msgParams rawIrcMsg of
    [_, _] -> do
      let cn = ChannelName $ Irc.idText channelName
      let m = ChatMessage (Irc.userName userInfo) cn msgBody rawIrcMsg
      response <- findAndRunCommand m
      case response of
        RespondWith t -> say cn t
        Nada -> return ()
          -- T.putStrLn . cs $ encodePretty rawIrcMsg
      liftIO $ writeChan outputChan rawIrcMsg
    params -> putStr $ "<Unhandled params>: " ++ show params

findAndRunCommand :: (QuotesDb m, CommandsDb m, MonadReader c m, HasConfig c) => ChatMessage -> m Response
findAndRunCommand (ChatMessage _ channel input _) =
  let (name, rest) = T.breakOn " " input
   in case Map.lookup name builtinCommands
        -- default command
            of
        Just (BotCommand args body) ->
          case parseString (optional whiteSpace >> args) mempty (cs rest) of
            Success a -> body channel a
            Failure _ -> return $ RespondWith "Sorry, I don't understand that."
        -- not a default command, look in the db for it.
        Nothing -> f <$> getCommandFromDb channel name
          where f (Just body) = RespondWith body
                f Nothing = RespondWith "I couldn't find that command."

say :: (MonadIO m, MonadReader c m, HasConfig c, Sender m) => ChannelName -> Text -> m ()
say (ChannelName twitchChannel) msg = do
  conf <- view config
  send $ ircPrivmsg twitchChannel msg
  let outputChan = _cbecOutputChan $ _configChatBotExecution conf
  liftIO $ writeChan outputChan $ ircPrivmsg twitchChannel msg

authorize :: (MonadIO m, MonadReader c m, HasConfig c, Sender m) => m ()
authorize = do
  chatBotConf <- view configChatBot
  send (ircPass $ _cbConfigPass chatBotConf)
  send (ircNick $ _cbConfigNick chatBotConf)
  send (ircCapReq ["twitch.tv/membership"])
  send (ircCapReq ["twitch.tv/commands"])
  send (ircCapReq ["twitch.tv/tags"])
  forM_ (_cbConfigChannels chatBotConf) $ \c -> do
    send (ircJoin c Nothing)
    say (ChannelName c) "Hello!"
  send (ircPing ["ping"])
