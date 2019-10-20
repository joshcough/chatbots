module ChatBot.Storage (
    ChatBotDB
  , CommandsDb(..)
  , QuestionsDb(..)
  , QuotesDb(..)
  ) where

import Protolude hiding (from)

import Control.Monad.Except (MonadIO)
import Control.Monad.Fail (MonadFail)
import Data.List ((!!), nub, sort)
import Database.Esqueleto
import Database.Persist.Postgresql (insert)
import qualified Database.Persist.Postgresql as P
import Logging ((.=), logDebug)
import System.Random (randomRIO)
import Types (AppT', runDb)

import ChatBot.Config (ChannelName(..))
import ChatBot.DatabaseModels (DbCommand(..), DbQuestion(..), DbQuote(..), EntityField(..))
import ChatBot.Models (ChatUserName(..), Command(..), Question(..), Quote(..))
import Config (HasConfig)

type ChatBotDB m = (CommandsDb m, QuotesDb m)

class Monad m => CommandsDb m where
    insertCommand :: ChannelName -> Text -> Text -> m ()
    getCommand :: ChannelName -> Text -> m (Maybe Command)
    deleteCommand :: ChannelName -> Text -> m ()
    getCommands :: ChannelName -> m [Command]

class Monad m => QuotesDb m where
    getQuoteStreams :: m [ChannelName]
    insertQuote :: ChannelName -> ChatUserName -> Text -> m Quote
    getQuote :: ChannelName -> Int -> m (Maybe Quote)
    deleteQuote :: ChannelName -> Int -> m ()
    getQuotes :: ChannelName -> m [Quote]
    getRandomQuote :: ChannelName -> m (Maybe Quote)

class Monad m => QuestionsDb m where
    getQuestionsStreams :: m [ChannelName]
    insertQuestion :: ChannelName -> Text -> m Question
    getQuestion :: ChannelName -> Int -> m (Maybe Question)
    deleteQuestion :: ChannelName -> Int -> m ()
    getQuestions :: ChannelName -> m [Question]

instance (HasConfig c, MonadIO m) => CommandsDb (AppT' e m c) where
    insertCommand (ChannelName channel) commandName commandText = do
        $(logDebug) "insertCommand" ["channel" .= channel, "name" .= commandName, "text" .= commandText]
        runDb $ insert $ DbCommand channel commandName commandText
        return ()
    getCommand (ChannelName channel) commandName = do
        $(logDebug) "getCommand" ["channel" .= channel, "name" .= commandName]
        fmap (fmap dbCommandToCommand) . runDb $
            selectFirst [DbCommandChannel P.==. channel, DbCommandName P.==. commandName] []
    deleteCommand (ChannelName channel) commandName = do
        $(logDebug) "deleteCommand" ["channel" .= channel, "name" .= commandName]
        runDb $ delete $ from $ \command ->
            where_ (
                (command ^. DbCommandChannel) ==. val channel
                &&.
                (command ^. DbCommandName) ==. val commandName
             )
    getCommands (ChannelName channel) = do
        $(logDebug) "getCommands" ["channel" .= channel]
        fmap (fmap dbCommandToCommand) . runDb $ select $ from $ \command -> do
            where_ $ (command ^. DbCommandChannel) ==. val channel
            pure command

instance (HasConfig c, MonadIO m) => QuotesDb (AppT' e m c) where
    getQuoteStreams = do
        $(logDebug) "getStreams" []
        fmap (sort . nub . fmap dbQuoteToChannel) . runDb $ select $ from pure
    insertQuote c@(ChannelName channel) user quoteText = do
        $(logDebug) "insertQuote" ["channel" .= channel, "user" .= user, "quoteText" .= quoteText]
        runDb $ do
            qid <- nextQuoteId c
            _ <- insert $ DbQuote channel quoteText user qid
            pure $ Quote c quoteText user qid
    getQuote (ChannelName channel) qid = do
        $(logDebug) "getQuote" ["channel" .= channel, "qid" .= qid]
        -- select * from quote where channel = channel and qid = qid
        fmap (fmap dbQuoteToQuote) . runDb $ selectFirst [DbQuoteChannel P.==. channel, DbQuoteQid P.==. qid] []
    deleteQuote (ChannelName channel) qid = do
        $(logDebug) "deleteQuote" ["channel" .= channel, "qid" .= qid]
        -- delete from quote where channel = channel and qid = qid
        runDb $ delete $ from $ \quote ->
            where_ (
                (quote ^. DbQuoteChannel) ==. val channel
                &&.
                (quote ^. DbQuoteQid) ==. val qid
             )
    getQuotes (ChannelName channel) = do
        $(logDebug) "getQuotes" ["channel" .= channel]
        fmap (fmap dbQuoteToQuote) . runDb $ select $ from $ \quote -> do
            where_ $ (quote ^. DbQuoteChannel) ==. val channel
            pure quote

    getRandomQuote c@(ChannelName channel) = do
        $(logDebug) "getRandomQuote" ["channel" .= channel]
        runDb (quoteIds c) >>= pick >>= \case
          Nothing -> return Nothing
          Just qid -> getQuote c qid

nextQuoteId :: (MonadFail m, MonadIO m) => ChannelName -> SqlPersistT m Int
nextQuoteId (ChannelName channel) = do
    [Value mn] <- select $ from $ \quote -> do
        where_ $ (quote ^. DbQuoteChannel) ==. val channel
        return (max_ $ quote ^. DbQuoteQid)
    pure $ maybe 1 (+1) mn

pick :: MonadIO m => [a] -> m (Maybe a)
pick [] = return Nothing
pick xs = liftIO $ Just . (xs !!) <$> randomRIO (0, length xs - 1)

quoteIds :: (MonadIO m) => ChannelName -> SqlPersistT m [Int]
quoteIds (ChannelName channel) =
    fmap (fmap unValue) $ select $ from $ \quote -> do
        where_ $ (quote ^. DbQuoteChannel) ==. val channel
        return (quote ^. DbQuoteQid)

instance (HasConfig c, MonadIO m) => QuestionsDb (AppT' e m c) where
    getQuestionsStreams = do
         $(logDebug) "getQuestionsStreams" []
         fmap (sort . nub . fmap dbQuestionToChannel) . runDb $ select $ from pure
    insertQuestion c@(ChannelName channel) questionText = do
        $(logDebug) "insertQuestion" ["channel" .= channel, "questionText" .= questionText]
        -- insert into questions values (
        --   channel, question, select count(*) + 1 from questions where channel = channel
        -- )
        runDb $ do
            qid <- fmap ((+1) . length) $ select $ from $ \question -> do
                     where_ $ (question ^. DbQuestionChannel) ==. val channel
                     return question
            _ <- insert $ DbQuestion channel questionText qid
            pure $ Question c questionText qid
    getQuestion (ChannelName channel) qid = do
        $(logDebug) "getQuestion" ["channel" .= channel, "qid" .= qid]
        -- select * from question where channel = channel and qid = qid
        fmap (fmap dbQuestionToQuestion) . runDb $ selectFirst [DbQuestionChannel P.==. channel, DbQuestionQid P.==. qid] []
    deleteQuestion (ChannelName channel) qid = do
        $(logDebug) "deleteQuestion" ["channel" .= channel, "qid" .= qid]
        -- delete from question where channel = channel and qid = qid
        runDb $ delete $ from $ \question ->
            where_ (
                (question ^. DbQuestionChannel) ==. val channel
                &&.
                (question ^. DbQuestionQid) ==. val qid
             )
    getQuestions (ChannelName channel) = do
        $(logDebug) "getQuestions" ["channel" .= channel]
        fmap (fmap dbQuestionToQuestion) . runDb $ select $ from $ \question -> do
            where_ $ (question ^. DbQuestionChannel) ==. val channel
            pure question

dbCommandToCommand :: Entity DbCommand -> Command
dbCommandToCommand (Entity _ (DbCommand chan name body)) = Command (ChannelName chan) name body

dbQuestionToQuestion :: Entity DbQuestion -> Question
dbQuestionToQuestion (Entity _ (DbQuestion chan name qid)) = Question (ChannelName chan) name qid

dbQuoteToQuote :: Entity DbQuote -> Quote
dbQuoteToQuote (Entity _ (DbQuote chan name user qid)) = Quote (ChannelName chan) name user qid

dbQuestionToChannel :: Entity DbQuestion -> ChannelName
dbQuestionToChannel (Entity _ (DbQuestion chan _ _)) = ChannelName chan

dbQuoteToChannel :: Entity DbQuote -> ChannelName
dbQuoteToChannel (Entity _ (DbQuote chan _ _ _)) = ChannelName chan
