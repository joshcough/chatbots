{-# LANGUAGE UndecidableInstances #-}

module ChatBot.WebSocket.MessageProcessor
  ( MessageProcessor(..)
  , MessageImporter(..)
  , Sender(..)
  , authorize
  , frontendListener
  , connectTo
  , disconnectFrom
  , say
  ) where

import Protolude

import ChatBot.Config (ChatBotConfig(..), ChatBotExecutionConfig(..), ChatBotFrontendMessage(..))
import ChatBot.Models
  ( ChannelName
  , ChatMessage
  , ChatMessage'(..)
  , ChatUser(..)
  , ChatUserName(..)
  , getChannelName
  , getChannelNameHashed
  , mkChannelName
  , trollabotUser
  )
import ChatBot.Storage (CommandsDb, QuestionsDb, QuotesDb(..))
import ChatBot.WebSocket.Commands (BotCommand(..), Permission(..), Permission(..), Response(..), builtinCommands) --, getCommandFromDb)
import Config (Config(..), HasConfig(..))
import Control.Concurrent.STM.TChan (writeTChan)
import Control.Lens (view)
import Data.Aeson (ToJSON(..))
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Irc.Commands (ircCapReq, ircJoin, ircNick, ircPart, ircPass, ircPing, ircPong, ircPrivmsg)
import qualified Irc.Identifier as Irc
import Irc.Identifier (mkId)
import Irc.Message (IrcMsg(..), cookIrcMsg)
import Irc.RawIrcMsg (RawIrcMsg(..), TagEntry(..))
import qualified Irc.UserInfo as Irc
import Logging (MonadLoggerJSON, (.=), logDebug, logError)
import Text.Trifecta (Result(..), parseString, whiteSpace)

class Sender m where
  send :: RawIrcMsg -> m ()

class Monad m => MessageProcessor m where
    processMessage :: RawIrcMsg -> m ()

class Monad m => MessageImporter m where
    processImportMessage :: ChannelName -> RawIrcMsg -> m ()

type Db m = (QuestionsDb m, QuotesDb m, CommandsDb m)

instance (Monad m, MonadLoggerJSON m, MonadIO m, Db m, MonadReader c m, HasConfig c, Sender m) => MessageProcessor m
    where
    processMessage rawIrcMsg = processMessage' (cookIrcMsg rawIrcMsg)
      where
        processMessage' (Ping xs) = send (ircPong xs)
        processMessage' (Pong _) = return ()
        processMessage' (Privmsg userInfo channelName msgBody) =
          processPrivMessage rawIrcMsg userInfo channelName msgBody
        processMessage' (UnknownMsg _) = return () -- probably a USERSTATE msg or something, don't care.
        -- if we somehow get "parted" from a channel, log and try to reconnect.
        processMessage' (Part user channel reason) =
          whenM (isBot user) $ do
            $(logError) "PARTED!" ["channel" .= channel, "reason" .= reason, "user" .= user]
            connectTo $ mkChannelName $ Irc.idText channel
        -- if we somehow "quit", log, and try to rejoin entirely.
        processMessage' (Quit user reason) =
          whenM (isBot user) $ do
            $(logError) "QUIT!" ["reason" .= reason, "user" .= user]
            authorize
        -- we somehow got kicked from a channel. for now, just log it.
        processMessage' (Kick user channel kicker reason) =
          whenM (isBot user) $
            $(logError) "GOT KICKED!" ["channel" .= channel, "kicker" .= kicker, "reason" .= reason, "user" .= user]
        processMessage' _ = $(logDebug) "couldn't process message" ["msg" .= rawIrcMsg]

isBot :: (MonadReader c m, HasConfig c) => Irc.UserInfo -> m Bool
isBot (Irc.UserInfo nick name _) = do
  chatBotConf <- view configChatBot
  let x = _cbConfigNick chatBotConf
  return $ x == Irc.idText nick || x == name

processPrivMessage ::
    (MonadIO m, MonadLoggerJSON m, Db m, MonadReader c m, HasConfig c, Sender m) =>
    RawIrcMsg -> Irc.UserInfo -> Irc.Identifier -> Text -> m ()
processPrivMessage rawIrcMsg userInfo channelName msgBody = do
  let chatMessage = createChatMessage rawIrcMsg userInfo channelName msgBody
  when (T.take 1 msgBody == "!" ) $ processUserCommand chatMessage (_msgParams rawIrcMsg)
  sendToWebSocket chatMessage

processUserCommand ::
    (MonadIO m, MonadLoggerJSON m, Db m, MonadReader c m, HasConfig c, Sender m) =>
    ChatMessage -> [Text] -> m ()
processUserCommand chatMessage msgParams =
  case msgParams of
    [_, _] -> do
      $(logDebug) "processUserMessage" ["chatMessage" .= chatMessage]
      findAndRunCommand chatMessage >>= \case
        RespondWith t -> say (cmChannel chatMessage) t
        Nada -> return () -- not a command that we know about, so do nothing. we could log though...
    params -> $(logDebug) "Unhandled params" ["msg" .= params]

createChatMessage :: RawIrcMsg -> Irc.UserInfo -> Irc.Identifier -> Text -> ChatMessage
createChatMessage rawIrcMsg userInfo channelName msgBody = ChatMessage user cn msgBody rawIrcMsg
  where
  cn = mkChannelName $ Irc.idText channelName
  tags = _msgTags rawIrcMsg
  findBoolTag t = TagEntry t "1" `elem` tags
  un = Irc.idText $ Irc.userNick userInfo
  un' = ChatUserName $ if un == "" then Irc.userName userInfo else un
  user = ChatUser un' (findBoolTag "mod") (findBoolTag "subscriber")

findAndRunCommand :: (Db m, MonadReader c m, HasConfig c) => ChatMessage -> m Response
findAndRunCommand cm =
  let (name, rest) = T.breakOn " " $ cmBody cm
  in case Map.lookup name builtinCommands of
    -- builtin command
    Just bc -> runBotCommand (cmUser cm) bc cm rest
    -- not a default command, look in the db for it.
    Nothing -> pure Nada -- f <$> getCommandFromDb channel name
--          where f (Just body) = RespondWith body
--                f Nothing = Nada

runBotCommand :: Monad m => ChatUser -> BotCommand m -> ChatMessage -> Text -> m Response
runBotCommand cu (BotCommand permission args body) cm rest =
  case permission of
    -- must check if user is streamer too, because streamer is not a mod for some reason.
    ModOnly | cuMod cu || isStreamer cu cm -> go
    Anyone -> go
    _ -> pure Nada
  where
  go = case parseString (optional whiteSpace >> args) mempty (cs rest) of
    Success a -> body (cmChannel cm) cu a
    Failure _ -> return $ RespondWith "Sorry, I don't understand that."

isStreamer :: ChatUser -> ChatMessage -> Bool
isStreamer (ChatUser (ChatUserName name) _ _) (ChatMessage _ cn _ _) = getChannelName cn == name

say :: (MonadIO m,  MonadReader c m, HasConfig c, MonadLoggerJSON m, Sender m) => ChannelName -> Text -> m ()
say twitchChannel msg = do
  $(logDebug) "sending message" ["chan" .= twitchChannel, "msg" .= msg]
  let rawIrcMsg = ircPrivmsg (getChannelNameHashed twitchChannel) msg
  -- send message to twitch
  send $ ircPrivmsg (getChannelNameHashed twitchChannel) msg
  -- write it to websocket too
  nick <- _cbConfigNick . _configChatBot <$> view config
  let botInfo = Irc.UserInfo (Irc.mkId nick) nick ""
  sendToWebSocket $ createChatMessage rawIrcMsg botInfo (mkId $ getChannelName twitchChannel) msg

authorize :: (MonadIO m, MonadReader c m, HasConfig c, Sender m) => m ()
authorize = do
  chatBotConf <- view configChatBot
  send (ircPass $ _cbConfigPass chatBotConf)
  send (ircNick $ _cbConfigNick chatBotConf)
  send (ircCapReq ["twitch.tv/membership"])
  send (ircCapReq ["twitch.tv/commands"])
  send (ircCapReq ["twitch.tv/tags"])
  send (ircPing ["ping"])

frontendListener :: (MonadIO m, MonadLoggerJSON m, MonadReader c m, HasConfig c, Sender m) => m ()
frontendListener = do
  conf <- view config
  let inputChan = _cbecInputChan $ _configChatBotExecution conf
  forever $ liftIO (readChan inputChan) >>= processChatBotFrontendMessage
  where
  -- TODO: these probably should be lifted to top level for testing.
  processChatBotFrontendMessage (ConnectTo c) = connectTo c
  processChatBotFrontendMessage (DisconnectFrom c) = disconnectFrom c

connectTo :: (MonadIO m,  MonadReader c m, HasConfig c, MonadLoggerJSON m, Sender m) => ChannelName -> m ()
connectTo cn = send (ircJoin (getChannelNameHashed cn) Nothing) >> say cn "Hello!"

disconnectFrom :: (MonadIO m,  MonadReader c m, HasConfig c, MonadLoggerJSON m, Sender m) => ChannelName -> m ()
disconnectFrom cn = say cn "Goodbye!" >> send (ircPart (mkId $ getChannelNameHashed cn) "")

instance (Monad m, MonadLoggerJSON m, Db m, MonadReader c m, Sender m) => MessageImporter m
  where
  processImportMessage chan rawIrcMsg = processImportMessage' (cookIrcMsg rawIrcMsg)
    where
    processImportMessage' (Ping xs) = send (ircPong xs)
    processImportMessage' (Privmsg userInfo channelName msgBody) = do
      $(logDebug) "got message from user" ["userInfo" .= userInfo
                                          ,"channelName" .= channelName
                                          ,"msgBody" .= msgBody
                                          ,"msg" .= rawIrcMsg
                                          ]
      when (Irc.userName userInfo == "Nightbot") $ do
        q <- insertQuote chan trollabotUser msgBody
        $(logDebug) "added quote" ["quote" .= q]
    processImportMessage' _ = pure ()

sendToWebSocket :: (MonadLoggerJSON m, MonadReader c m, HasConfig c, MonadIO m) => ChatMessage -> m ()
sendToWebSocket chatMessage = do
  $(logDebug) "writing to output chan" ["chatMessage" .= chatMessage]
  outputChan <- _cbecOutputChan . _configChatBotExecution <$> view config
  liftIO $ atomically $ writeTChan outputChan (toJSON <$> chatMessage)
