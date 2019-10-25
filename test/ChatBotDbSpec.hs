module ChatBotDbSpec (spec) where

import Protolude
import Test.Hspec

import ChatBot.Config (ChannelName, mkChannelName)
import ChatBot.Models (Question(..), Quote(..), trollabotUser)
import ChatBot.Storage (QuestionsDb(..), QuotesDb(..))
import Helpers
import Types (runAppTInTestAndThrow)

art :: ChannelName
art = mkChannelName "artoftroll"

daut :: ChannelName
daut = mkChannelName "daut"

spec :: Spec
spec = withDB $ do
    describe "Questions" $ do
        it "can insert question" $ \(_, config) -> do
            qid <- runAppTInTestAndThrow config $
                insertQuestion' art "i am so good at this game?"
            qid `shouldBe` 1

        it "can insert many questions in same channel" $ \(_, config) -> do
            (q1, q2) <- runAppTInTestAndThrow config $ do
                q1 <- insertQuestion' art "look what i can do?"
                q2 <- insertQuestion' art "bam?"
                pure (q1, q2)
            (q1, q2) `shouldBe` (1, 2)

        it "can insert many questions in different channels" $ \(_, config) -> do
            (q1, q2, q3, q4) <- runAppTInTestAndThrow config $ do
                q1 <- insertQuestion' art "look what i can do - art?"
                q2 <- insertQuestion' art "bam - art?"
                q3 <- insertQuestion' daut "look what i can do - daut?"
                q4 <- insertQuestion' daut "bam - daut?"
                pure (q1, q2, q3, q4)
            (q1, q2, q3, q4) `shouldBe` (1, 2, 1, 2)

    describe "Quotes" $ do
        it "can insert quote" $ \(_, config) -> do
            qid <- runAppTInTestAndThrow config $
                insertQuote' art "i am so good at this game"
            qid `shouldBe` 1

        it "can insert many quotes in same channel" $ \(_, config) -> do
            (q1, q2) <- runAppTInTestAndThrow config $ do
                q1 <- insertQuote' art "look what i can do"
                q2 <- insertQuote' art "bam"
                pure (q1, q2)
            (q1, q2) `shouldBe` (1, 2)

        it "can delete quotes from a channel (and still insert after)" $ \(_, config) -> do
            qs <- runAppTInTestAndThrow config $ do
                q1 <- insertQuote' art "look what i can do"
                q2 <- insertQuote' art "bam"
                _ <- insertQuote' art "bam again"
                deleteQuote art q1
                deleteQuote art q2
                _ <- insertQuote' art "bam again!"
                getQuotes art
            qs `shouldBe` [Quote art "bam again" trollabotUser 3, Quote art "bam again!" trollabotUser 4]

        it "can insert many quotes in different channels" $ \(_, config) -> do
            (q1, q2, q3, q4) <- runAppTInTestAndThrow config $ do
                q1 <- insertQuote' art "look what i can do - art"
                q2 <- insertQuote' art "bam - art"
                q3 <- insertQuote' daut "look what i can do - daut"
                q4 <- insertQuote' daut "bam - daut"
                pure (q1, q2, q3, q4)
            (q1, q2, q3, q4) `shouldBe` (1, 2, 1, 2)

insertQuestion' :: QuestionsDb f => ChannelName -> Text -> f Int
insertQuestion' c = fmap questionQid . insertQuestion c

insertQuote' :: QuotesDb f => ChannelName -> Text -> f Int
insertQuote' c = fmap quoteQid . insertQuote c trollabotUser
