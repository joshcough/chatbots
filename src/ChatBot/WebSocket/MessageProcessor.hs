{-# LANGUAGE UndecidableInstances #-}

module ChatBot.WebSocket.MessageProcessor
  ( MessageProcessor(..)
  , Sender(..)
  , authorize
  , frontendListener
  ) where

import Protolude

import ChatBot.Config (ChatBotConfig(..), ChatBotExecutionConfig(..), ChatBotFrontendMessage(..))
import ChatBot.Models (ChannelName(..), ChatMessage(..))
import ChatBot.Storage (CommandsDb, QuotesDb)
import ChatBot.WebSocket.Commands (BotCommand(..), Response(..), builtinCommands, getCommandFromDb)
import Config (Config(..), HasConfig(..))
import Control.Concurrent.Chan (writeChan)
import Control.Lens (view)
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Irc.Commands (ircCapReq, ircJoin, ircNick, ircPart, ircPass, ircPing, ircPong, ircPrivmsg)
import qualified Irc.Identifier as Irc
import Irc.Identifier (mkId)
import Irc.Message (IrcMsg(..), cookIrcMsg)
import Irc.RawIrcMsg (RawIrcMsg(..))
import qualified Irc.UserInfo as Irc
import Logging (MonadLoggerJSON, (.=), logDebug)
import Text.Trifecta (Result(..), parseString, whiteSpace)

class Sender m where
  send :: RawIrcMsg -> m ()

class Monad m => MessageProcessor m where
    processMessage :: RawIrcMsg -> m ()

instance (Monad m, MonadLoggerJSON m, MonadIO m, QuotesDb m, CommandsDb m, MonadReader c m, HasConfig c, Sender m) => MessageProcessor m
    where
    processMessage rawIrcMsg = processMessage' (cookIrcMsg rawIrcMsg)
      where
        processMessage' (Ping xs) = send (ircPong xs)
        processMessage' (Pong _) = return ()
        processMessage' (Privmsg userInfo channelName msgBody)
          | T.take 1 msgBody == "!" = processUserMessage rawIrcMsg userInfo channelName msgBody
        processMessage' Privmsg{} = return () -- just a regular user message.
        processMessage' (UnknownMsg _) = return () -- probably a USERSTATE msg or something, don't care.
        processMessage' _ = $(logDebug) "couldn't process message" ["msg" .= rawIrcMsg]

processUserMessage ::
    (MonadIO m, MonadLoggerJSON m, QuotesDb m, CommandsDb m, MonadReader c m, HasConfig c, Sender m) =>
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
        Nada -> return () -- T.putStrLn . cs $ encodePretty rawIrcMsg
      liftIO $ writeChan outputChan rawIrcMsg
    params -> $(logDebug) "Unhandled params" ["msg" .= params]

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
  send (ircPing ["ping"])

frontendListener :: (MonadIO m, MonadReader c m, HasConfig c, Sender m) => m ()
frontendListener = do
    conf <- view config
    let inputChan = _cbecInputChan $ _configChatBotExecution conf
    forever $ liftIO (readChan inputChan) >>= processChatBotFrontendMessage
    where
    -- TODO: these probably should be lifted to top level for testing.
    processChatBotFrontendMessage (ConnectTo c) = connectTo c
    processChatBotFrontendMessage (DisconnectFrom c) = disconnectFrom c
    connectTo cn@(ChannelName c) = send (ircJoin ("#" <> c) Nothing) >> say cn "Hello!"
    disconnectFrom cn@(ChannelName c) = say cn "Goodbye!" >> send (ircPart (mkId ("#" <> c)) "")