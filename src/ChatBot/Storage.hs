module ChatBot.Storage (
    ChatBotDB
  , CommandsDb(..)
  , QuestionsDb(..)
  , QuotesDb(..)
  ) where

import Protolude hiding (from)

import Control.Monad.Except (MonadIO)
import Database.Esqueleto
import Data.List (nub, sort)
import Database.Persist.Postgresql (insert)
import qualified Database.Persist.Postgresql as P
import Logging (logDebug, (.=))
import Types (AppT', runDb)

import ChatBot.Config (ChannelName(..))
import ChatBot.DatabaseModels (DbCommand(..), DbQuestion(..), DbQuote(..), EntityField(..))
import ChatBot.Models (Command(..), Question(..), Quote(..))
import Config (HasConfig)

type ChatBotDB m = (CommandsDb m, QuotesDb m)

class Monad m => CommandsDb m where
    insertCommand :: ChannelName -> Text -> Text -> m ()
    getCommand :: ChannelName -> Text -> m (Maybe Command)
    deleteCommand :: ChannelName -> Text -> m ()
    getCommands :: ChannelName -> m [Command]

class Monad m => QuotesDb m where
    getQuoteStreams :: m [ChannelName]
    insertQuote :: ChannelName -> Text -> m Quote
    getQuote :: ChannelName -> Int -> m (Maybe Quote)
    deleteQuote :: ChannelName -> Int -> m ()
    getQuotes :: ChannelName -> m [Quote]

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
    insertQuote c@(ChannelName channel) quoteText = do
        $(logDebug) "insertQuote" ["channel" .= channel, "quoteText" .= quoteText]
        -- insert into quotes values (
        --   channel, quote, select count(*) + 1 from quotes where channel = channel
        -- )
        runDb $ do
            qid <- fmap ((+1) . length) $ select $ from $ \quote -> do
                     where_ $ (quote ^. DbQuoteChannel) ==. val channel
                     return quote
            _ <- insert $ DbQuote channel quoteText qid
            pure $ Quote c quoteText qid
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
dbQuoteToQuote (Entity _ (DbQuote chan name qid)) = Quote (ChannelName chan) name qid

dbQuestionToChannel :: Entity DbQuestion -> ChannelName
dbQuestionToChannel (Entity _ (DbQuestion chan _ _)) = ChannelName chan

dbQuoteToChannel :: Entity DbQuote -> ChannelName
dbQuoteToChannel (Entity _ (DbQuote chan _ _)) = ChannelName chan
