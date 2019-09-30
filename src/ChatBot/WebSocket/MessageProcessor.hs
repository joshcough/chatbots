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
import ChatBot.Models (ChannelName(..), ChatMessage(..), ChatUser(..))
import ChatBot.Storage (CommandsDb, QuestionsDb, QuotesDb(..))
import ChatBot.WebSocket.Commands (BotCommand(..), Permission(..), Response(..), Permission(..), builtinCommands) --, getCommandFromDb)
import Config (Config(..), HasConfig(..))
--import Control.Concurrent.Chan (writeChan)
import Control.Lens (view)
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
import Logging (MonadLoggerJSON, (.=), logDebug)
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
        processMessage' (Privmsg userInfo channelName msgBody)
          | T.take 1 msgBody == "!" = processUserMessage rawIrcMsg userInfo channelName msgBody
        processMessage' Privmsg{} = return () -- just a regular user message.
        processMessage' (UnknownMsg _) = return () -- probably a USERSTATE msg or something, don't care.
        processMessage' _ = $(logDebug) "couldn't process message" ["msg" .= rawIrcMsg]

processUserMessage ::
    (MonadIO m, MonadLoggerJSON m, Db m, MonadReader c m, HasConfig c, Sender m) =>
    RawIrcMsg -> Irc.UserInfo -> Irc.Identifier -> Text -> m ()
processUserMessage rawIrcMsg userInfo channelName msgBody =
  case _msgParams rawIrcMsg of
    [_, _] -> do
      let chatMessage = createChatMessage rawIrcMsg userInfo channelName msgBody
      findAndRunCommand chatMessage >>= \case
        RespondWith t -> say (cmChannel chatMessage) t
        Nada -> return () -- T.putStrLn . cs $ encodePretty rawIrcMsg
--      outputChan <- _cbecOutputChan . _configChatBotExecution <$> view config
--      liftIO $ writeChan outputChan rawIrcMsg
    params -> $(logDebug) "Unhandled params" ["msg" .= params]

createChatMessage :: RawIrcMsg -> Irc.UserInfo -> Irc.Identifier -> Text -> ChatMessage
createChatMessage rawIrcMsg userInfo channelName msgBody = ChatMessage user cn msgBody rawIrcMsg
  where
  cn = ChannelName $ Irc.idText channelName
  tags = _msgTags rawIrcMsg
  findBoolTag t = TagEntry t "1" `elem` tags
  user = ChatUser userInfo (findBoolTag "mod") (findBoolTag "subscriber")

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
    ModOnly | cuMod cu -> go
    Anyone -> go
    _ -> pure Nada
  where
  go = case parseString (optional whiteSpace >> args) mempty (cs rest) of
    Success a -> body (cmChannel cm) a
    Failure _ -> return $ RespondWith "Sorry, I don't understand that."

say :: (MonadIO m, MonadLoggerJSON m, Sender m) => ChannelName -> Text -> m ()
say (ChannelName twitchChannel) msg = do
--  conf <- view config
  $(logDebug) "sending message" ["chan" .= twitchChannel, "msg" .= msg]
  send $ ircPrivmsg twitchChannel msg
--  let outputChan = _cbecOutputChan $ _configChatBotExecution conf
--  liftIO $ writeChan outputChan $ ircPrivmsg twitchChannel msg

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

connectTo :: (Sender m, MonadIO m, MonadLoggerJSON m) => ChannelName -> m ()
connectTo cn@(ChannelName c) = send (ircJoin ("#" <> c) Nothing) >> say cn "Hello!"

disconnectFrom :: (MonadIO m, MonadLoggerJSON m, Sender m) => ChannelName -> m ()
disconnectFrom cn@(ChannelName c) = say cn "Goodbye!" >> send (ircPart (mkId ("#" <> c)) "")

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
            q <- insertQuote chan msgBody
            $(logDebug) "added quote" ["quote" .= q]
        processImportMessage' _ = pure ()