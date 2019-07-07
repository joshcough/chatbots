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
import           Text.Trifecta           (Result(..), parseString, string, whiteSpace)

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
    processMessage' (Ping xs) = sendMsg (ircPong xs)
    processMessage' (Privmsg userInfo channelName msgBody) = do
        ConfigAndConnection conf _ <- ask
        let outputChan = _cbecOutputChan $ _configChatBotExecution conf
        case _msgParams rawIrcMsg of
            [_, _] -> do
                let m = ChatMessage (Irc.userName userInfo)
                                    (ChannelName $ Irc.idText channelName)
                                    msgBody
                                    rawIrcMsg
                dispatch m
                -- T.putStrLn . cs $ encodePretty rawIrcMsg
                liftIO $ writeChan outputChan rawIrcMsg
            params -> putStr $ "<Unhandled params>: " ++ show params
    processMessage' _ = do
        liftIO $ T.putStr "couldn't process message: "
        liftIO $ T.putStrLn . cs $ encodePretty rawIrcMsg

dispatch :: ChatMessage -> App ()
dispatch msg = forM_ (Map.toList defaultCommands) (runCommand msg)

runCommand :: ChatMessage -> (Text, Command App) -> App ()
runCommand (ChatMessage _ channel input _) (name, Command args body) =
    case parseString p mempty (cs input) of
        Success a -> body channel a >>= \case
            RespondWith t -> say channel t
            Nada  -> return ()
        Failure _ -> return ()
    where p = string (cs $ "!" <> name) >> optional whiteSpace >> args

say :: ChannelName -> Text -> App ()
say (ChannelName twitchChannel) msg = do
    ConfigAndConnection conf _ <- ask
    sendMsg $ ircPrivmsg twitchChannel msg
    let outputChan = _cbecOutputChan $ _configChatBotExecution conf
    liftIO $ writeChan outputChan $ ircPrivmsg twitchChannel msg

authorize :: App ()
authorize = do
    ConfigAndConnection conf _ <- ask
    let chatBotConf = _configChatBot conf
    sendMsg (ircPass $ _cbConfigPass chatBotConf)
    sendMsg (ircNick $ _cbConfigNick chatBotConf)
    sendMsg (ircCapReq ["twitch.tv/membership"])
    sendMsg (ircCapReq ["twitch.tv/commands"])
    sendMsg (ircCapReq ["twitch.tv/tags"])
    forM_ (_cbConfigChannels chatBotConf) $ \c -> sendMsg (ircJoin c Nothing)
    sendMsg (ircPing ["ping"])

sendMsg :: RawIrcMsg -> App ()
sendMsg msg = do
    ConfigAndConnection _ conn <- ask
    -- T.putStrLn . cs $ encodePretty msg
    liftIO $ WS.sendTextData conn (renderRawIrcMsg msg)
