{-# LANGUAGE TemplateHaskell #-}

module ChatBot.ChatBotWS
  ( runBot
  ) where

import Protolude
import Prelude (fail, String) -- TODO: kill me

import           Control.Concurrent.Chan (writeChan)
import           Control.Lens.TH         (makeClassy)
import           Control.Monad           (forever, forM_)
import qualified Data.Map                as Map
import           Data.String.Conversions (cs)
import           Network.Socket          (withSocketsDo)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Irc.Commands            (ircCapReq, ircJoin, ircNick, ircPass, ircPing, ircPong, ircPrivmsg)
import qualified Irc.Identifier          as Irc
import           Irc.Message             (IrcMsg(..), cookIrcMsg)
import           Irc.RawIrcMsg           (RawIrcMsg(..), parseRawIrcMsg, renderRawIrcMsg)
import qualified Irc.UserInfo            as Irc
import qualified Network.WebSockets      as WS
import           Text.Trifecta           (Result(..), parseString, whiteSpace)

import           ChatBot.Commands        (Command(..), Response(..), defaultCommands)
import           ChatBot.Config          (ChatBotConfig(..), ChatBotExecutionConfig(..))
import           ChatBot.Models          (ChatMessage(..), ChannelName(..))
import           Config                  (Config(..), HasConfig(..))
import           Error                   (ChatBotError)
import           Types                   (AppTEnv', runAppToIO)
import Logging (HasLoggingCfg(..))

import qualified Data.Text.IO              as T
import           Data.Aeson.Encode.Pretty       (encodePretty)


data ConfigAndConnection = ConfigAndConnection {
   _configAndConnectionConfig :: Config
 , _configAndConnectionConn :: WS.Connection
 }

makeClassy ''ConfigAndConnection

twitchIrcUrl :: String
twitchIrcUrl = "irc-ws.chat.twitch.tv"

runBot :: Config -> IO ()
runBot conf = withSocketsDo $
    WS.runClient twitchIrcUrl 80 "/" $ \conn ->
        runAppToIO (ConfigAndConnection conf conn) app

instance HasLoggingCfg ConfigAndConnection
    where loggingCfg = configAndConnectionConfig . loggingCfg

instance HasConfig ConfigAndConnection
    where config = configAndConnectionConfig

type App = AppTEnv' ChatBotError IO ConfigAndConnection

app :: App ()
app = authorize >> twitchListener

twitchListener :: App ()
twitchListener = forever $ do
    ConfigAndConnection _ conn <- ask
    msg <- liftIO $ T.strip <$> WS.receiveData conn
    liftIO $ print msg
    -- todo: throw actual error here.
    maybe (fail "Server sent invalid message!")
          processMessage
          (parseRawIrcMsg msg)

processMessage :: RawIrcMsg -> App ()
processMessage rawIrcMsg = processMessage' (cookIrcMsg rawIrcMsg)
    where
    processMessage' (Ping xs) = do
        ConfigAndConnection _ conn <- ask
        send conn (ircPong xs)
    processMessage' (Privmsg userInfo channelName msgBody) =
        processUserMessage rawIrcMsg userInfo channelName msgBody
    processMessage' _ = do
        liftIO $ T.putStr "couldn't process message: "
        liftIO $ T.putStrLn . cs $ encodePretty rawIrcMsg

processUserMessage :: RawIrcMsg -> Irc.UserInfo -> Irc.Identifier -> Text -> App ()
processUserMessage rawIrcMsg userInfo channelName msgBody = do
  ConfigAndConnection conf _ <- ask
  let outputChan = _cbecOutputChan $ _configChatBotExecution conf
  case _msgParams rawIrcMsg of
      [_, _] -> do
          let cn = ChannelName $ Irc.idText channelName
          let m = ChatMessage (Irc.userName userInfo) cn msgBody rawIrcMsg
          response <- dispatch m
          case response of
             RespondWith t -> say cn t
             Nada  -> return ()
          -- T.putStrLn . cs $ encodePretty rawIrcMsg
          liftIO $ writeChan outputChan rawIrcMsg
      params -> putStr $ "<Unhandled params>: " ++ show params

dispatch :: ChatMessage -> App Response
dispatch (ChatMessage _ channel input _) =
    case Map.lookup name defaultCommands of
        Just (Command args body) ->
            case parseString (optional whiteSpace >> args) mempty (cs rest) of
                Success a -> body channel a
                Failure _ -> return Nada
        Nothing -> return Nada
    where
    (name, rest) = T.breakOn " " input

say :: ChannelName -> Text -> App ()
say (ChannelName twitchChannel) msg = do
    ConfigAndConnection conf conn <- ask
    send conn $ ircPrivmsg twitchChannel msg
    let outputChan = _cbecOutputChan $ _configChatBotExecution conf
    liftIO $ writeChan outputChan $ ircPrivmsg twitchChannel msg

authorize :: App ()
authorize = do
    ConfigAndConnection conf conn <- ask
    let chatBotConf = _configChatBot conf
    send conn (ircPass $ _cbConfigPass chatBotConf)
    send conn (ircNick $ _cbConfigNick chatBotConf)
    send conn (ircCapReq ["twitch.tv/membership"])
    send conn (ircCapReq ["twitch.tv/commands"])
    send conn (ircCapReq ["twitch.tv/tags"])
    forM_ (_cbConfigChannels chatBotConf) $ \c -> send conn (ircJoin c Nothing)
    send conn (ircPing ["ping"])

class Sender c m where
  send :: c -> RawIrcMsg -> m ()

instance MonadIO m => Sender WS.Connection m where
    send conn msg = liftIO $ WS.sendTextData conn (renderRawIrcMsg msg)
