module ChatBot.ChatBotWS
  ( runBot
  ) where

import Protolude
import Prelude (fail, String) -- TODO: kill me

import           Control.Concurrent.Chan (writeChan)
import           Control.Monad           (forever, forM_)
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Network.Socket          (withSocketsDo)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Irc.Commands            (ircCapReq, ircJoin, ircNick, ircPass, ircPing, ircPong, ircPrivmsg)
import qualified Irc.Identifier          as Irc
import           Irc.Message             (IrcMsg(..), cookIrcMsg)
import           Irc.RawIrcMsg           (RawIrcMsg(..), parseRawIrcMsg, renderRawIrcMsg)
import qualified Irc.UserInfo            as Irc
import           Network.HTTP            (getRequest, getResponseBody, simpleHTTP)
import qualified Network.WebSockets      as WS
import           Text.Trifecta

import           ChatBot.Config          (ChatBotConfig(..), ChatBotExecutionConfig(..))
import           ChatBot.Models          (ChatMessage(..))
import           ChatBot.Parsers         (anything, slurp, url)
import           Config                  (Config(..))
import           Types                   (App, runAppToIO)

-- import qualified Data.Text.IO              as T
-- import           Data.Aeson.Encode.Pretty       (encodePretty)

twitchIrcUrl :: String
twitchIrcUrl = "irc-ws.chat.twitch.tv"

runBot :: Config -> IO ()
runBot conf = withSocketsDo $
    WS.runClient twitchIrcUrl 80 "/" (runAppToIO conf . app)

---
---
---

commands :: MonadIO m => [Command m]
commands = [ Command "hi"    anything $ const $ pure $ RespondWith "hello!"
           , Command "bye"   anything $ const $ pure $ RespondWith "bye!"
           , Command "echo"  slurp    $ pure . RespondWith
           , Command "echo!" slurp    $ pure . RespondWith . \t -> t <> "!"
           , Command "url" url $ fmap (RespondWith . cs . take 100) . fetchUrl
           ]

data Response = RespondWith Text | Nada
data Command m = forall a . Command Text (Parser a) (a -> m Response)

fetchUrl :: MonadIO m => ConvertibleStrings a String => a -> m String
fetchUrl u = liftIO $ simpleHTTP (getRequest $ cs u) >>= getResponseBody

app :: WS.Connection -> App ()
app conn = do
    authorize conn
    twitchListener conn

twitchListener :: WS.Connection -> App ()
twitchListener conn = forever $ do
    msg <- liftIO $ T.strip <$> WS.receiveData conn
    liftIO $ print msg
    -- todo: throw actual error here.
    maybe (fail "Server sent invalid message!")
          (processMessage conn)
          (parseRawIrcMsg msg)

processMessage :: WS.Connection -> RawIrcMsg -> App ()
processMessage conn rawIrcMsg = processMessage' (cookIrcMsg rawIrcMsg)
    where
    processMessage' (Ping xs) = sendMsg conn (ircPong xs)
    processMessage' (Privmsg userInfo channelName msgBody) = do
        conf <- ask
        let outputChan = _cbecOutputChan $ _configChatBotExecution conf
        case _msgParams rawIrcMsg of
            [_, _] -> do
                let m = ChatMessage (Irc.userName userInfo)
                                    (Irc.idText channelName)
                                    msgBody
                                    rawIrcMsg
                dispatch conn m
                -- T.putStrLn . cs $ encodePretty rawIrcMsg
                liftIO $ writeChan outputChan rawIrcMsg
            params -> putStr $ "<Unhandled params>: " ++ show params
    processMessage' _ = return ()
        -- T.putStrLn . cs $ encodePretty rawIrcMsg
        -- writeChan outputChan rawIrcMsg

dispatch :: WS.Connection -> ChatMessage -> App ()
dispatch conn msg = forM_ commands (runCommand conn msg)

runCommand :: WS.Connection -> ChatMessage -> Command App -> App ()
runCommand conn (ChatMessage _ channel input _) (Command name args body) =
    case parseString p mempty (cs input) of
        Success a -> body a >>= \case
            RespondWith t -> say channel t conn
            Nada  -> return ()
        Failure _ -> return ()
    where p = string (cs $ "!" <> name) >> optional whiteSpace >> args

say :: Text -> Text -> WS.Connection -> App ()
say twitchChannel msg conn = do
    conf <- ask
    sendMsg conn $ ircPrivmsg twitchChannel msg
    let outputChan = _cbecOutputChan $ _configChatBotExecution conf
    liftIO $ writeChan outputChan $ ircPrivmsg twitchChannel msg

authorize :: WS.Connection -> App ()
authorize conn = do
    chatBotConf <- _configChatBot <$> ask
    sendMsg conn (ircPass $ _cbConfigPass chatBotConf)
    sendMsg conn (ircNick $ _cbConfigNick chatBotConf)
    sendMsg conn (ircCapReq ["twitch.tv/membership"])
    sendMsg conn (ircCapReq ["twitch.tv/commands"])
    sendMsg conn (ircCapReq ["twitch.tv/tags"])
    forM_ (_cbConfigChannels chatBotConf) $ \c -> sendMsg conn (ircJoin c Nothing)
    sendMsg conn (ircPing ["ping"])

sendMsg :: MonadIO m => WS.Connection -> RawIrcMsg -> m ()
sendMsg conn msg =
    -- T.putStrLn . cs $ encodePretty msg
    liftIO $ WS.sendTextData conn (renderRawIrcMsg msg)


{-
!addQuote something stupid i said
=> "add quote 22: something stupid i said"
!quote22
=> "something stupid i said"
!quote5
=> "im lazy"
!randomQuote
=> ...
!uptime
-}
