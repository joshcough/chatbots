module ChatBot.ChatBotWS
  ( runBot
  , main
  ) where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (newChan, readChan, writeChan)
import           Control.Monad           (forever, void, forM_)
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Network.Socket          (withSocketsDo)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Irc.Commands            (ircCapReq, ircJoin, ircNick, ircPart, ircPass, ircPing, ircPong, ircPrivmsg)
import           Irc.Identifier          (mkId)
import qualified Irc.Identifier          as Irc
import           Irc.Message             (IrcMsg(..), cookIrcMsg)
import           Irc.RawIrcMsg           (RawIrcMsg(..), parseRawIrcMsg, renderRawIrcMsg)
import qualified Irc.UserInfo            as Irc
import           Network.HTTP            (getRequest, getResponseBody, simpleHTTP)
import qualified Network.WebSockets      as WS
import           Text.Trifecta

import           ChatBot.Config          (ChannelName(..), ChatBotConfig(..), ChatBotExecutionConfig(..), ChatBotFrontendMessage(..), configFromFile)
import           ChatBot.Models          (ChatMessage(..))
import           ChatBot.Parsers         (anything, slurp, url, (~~))

-- import qualified Data.Text.IO              as T
-- import           Data.Aeson.Encode.Pretty       (encodePretty)

main :: IO ()
main = do
    outputChan <- newChan
    inputChan <- newChan
    runBot (ChatBotExecutionConfig outputChan inputChan) =<< configFromFile "chatbot.json"

twitchIrcUrl :: String
twitchIrcUrl = "irc-ws.chat.twitch.tv"

runBot :: ChatBotExecutionConfig -> ChatBotConfig -> IO ()
runBot eConf conf = withSocketsDo $
    WS.runClient twitchIrcUrl 80 "/" (app eConf conf)

---
---
---

commands :: [Command]
commands = [ Command "hi"    anything $ const $ pure $ RespondWith "hello!"
           , Command "bye"   anything $ const $ pure $ RespondWith "bye!"
           , Command "echo"  slurp    $ pure . RespondWith
           , Command "echo!" slurp    $ pure . RespondWith . \t -> t <> "!"
           , Command "url" url $ fmap (RespondWith . cs . take 100) . fetchUrl
           , Command "mult" (integer ~~ integer) $ \(i, j) ->
                pure $ RespondWith . cs . show $ i * j
           ]

data Response = RespondWith Text | Nada
data Command = forall a. Command Text (Parser a) (a -> IO Response)

fetchUrl :: ConvertibleStrings a String => a -> IO String
fetchUrl u = simpleHTTP (getRequest $ cs u) >>= getResponseBody

app :: ChatBotExecutionConfig -> ChatBotConfig -> WS.ClientApp ()
app (ChatBotExecutionConfig outputChan inputChan) conf conn = do
    void . forkIO $ authorize
    void . forkIO $ frontendListener
    twitchListener
    where

    frontendListener :: IO ()
    frontendListener = forever $ readChan inputChan >>= \case
        ConnectTo (ChannelName c) -> sendMsg conn (ircJoin c Nothing)
        DisconnectFrom (ChannelName c) -> sendMsg conn (ircPart (mkId c) "")

    twitchListener :: IO ()
    twitchListener = forever $ do
        msg <- T.strip <$> WS.receiveData conn
        print msg
        maybe (fail "Server sent invalid message!")
              processMessage
              (parseRawIrcMsg msg)

    authorize :: IO ()
    authorize = do
        sendMsg conn (ircPass $ _cbConfigPass conf)
        sendMsg conn (ircNick $ _cbConfigNick conf)
        sendMsg conn (ircCapReq ["twitch.tv/membership"])
        sendMsg conn (ircCapReq ["twitch.tv/commands"])
        sendMsg conn (ircCapReq ["twitch.tv/tags"])
        forM_ (_cbConfigChannels conf) $ \c -> sendMsg conn (ircJoin c Nothing)
        sendMsg conn (ircPing ["ping"])

    processMessage rawIrcMsg = processMessage' (cookIrcMsg rawIrcMsg)
        where
        processMessage' (Ping xs) = sendMsg conn (ircPong xs)
        processMessage' (Privmsg userInfo channelName msgBody) =
            case _msgParams rawIrcMsg of
                [_, _] -> do
                    let m = ChatMessage (Irc.userName userInfo)
                                        (Irc.idText channelName)
                                        msgBody
                                        rawIrcMsg
                    dispatch m
                    -- T.putStrLn . cs $ encodePretty rawIrcMsg
                    writeChan outputChan rawIrcMsg
                params -> putStr $ "<Unhandled params>: " ++ show params
        processMessage' _ =
            -- T.putStrLn . cs $ encodePretty rawIrcMsg
            writeChan outputChan rawIrcMsg

        dispatch :: ChatMessage -> IO ()
        dispatch msg = forM_ commands (runCommand msg)

        runCommand :: ChatMessage -> Command -> IO ()
        runCommand (ChatMessage _ channel input _) (Command name args body) =
            case parseString p mempty (cs input) of
                Success a -> body a >>= \case
                    RespondWith t -> say channel t
                    Nada  -> return ()
                Failure _ -> return ()
            where p = string (cs $ "!" <> name) >> optional whiteSpace >> args

        say :: Text -> Text -> IO ()
        say twitchChannel msg = do
            sendMsg conn $ ircPrivmsg twitchChannel msg
            writeChan outputChan $ ircPrivmsg twitchChannel msg

sendMsg :: WS.Connection -> RawIrcMsg -> IO ()
sendMsg conn msg =
    -- T.putStrLn . cs $ encodePretty msg
    WS.sendTextData conn (renderRawIrcMsg msg)


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
