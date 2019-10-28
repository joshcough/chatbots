module ChatBotDbSpec (spec) where

import           Protolude
import           Test.Hspec

import           ChatBot.Config  (ChannelName, mkChannelName)
import           ChatBot.Models  (Quote (..), Stream (..), trollabotUser)
import           ChatBot.Storage (QuotesDb (..), StreamsDb (..))
import           Config          (Config)
import           Helpers
import           Types           (AppTEnv, runAppTInTestAndThrow)

art :: ChannelName
art = mkChannelName "artoftroll"

daut :: ChannelName
daut = mkChannelName "daut"

spec :: Spec
spec = withDB $ describe "Quotes" $ do
  it "can insert quote" $ \c -> withStreams c $ do
    qid <- runAction c $ insertQuote' art "i am so good at this game"
    qid `shouldBe` 1

  it "can insert many quotes in same channel" $ \c -> withStreams c $ do
    (q1, q2) <- runAction c $ do
      q1 <- insertQuote' art "look what i can do"
      q2 <- insertQuote' art "bam"
      pure (q1, q2)
    (q1, q2) `shouldBe` (1, 2)

  it "can delete quotes from a channel (and still insert after)" $ \c -> withStreams c $ do
    qs <- runAction c $ do
      q1 <- insertQuote' art "look what i can do"
      q2 <- insertQuote' art "bam"
      _ <- insertQuote' art "bam again"
      deleteQuote art q1
      deleteQuote art q2
      _ <- insertQuote' art "bam again!"
      getQuotes art
    qs `shouldBe` [Quote art "bam again" trollabotUser 3, Quote art "bam again!" trollabotUser 4]

  it "can insert many quotes in different channels" $ \c -> withStreams c $ do
    (q1, q2, q3, q4) <- runAction c $ do
      q1 <- insertQuote' art "look what i can do - art"
      q2 <- insertQuote' art "bam - art"
      q3 <- insertQuote' daut "look what i can do - daut"
      q4 <- insertQuote' daut "bam - daut"
      pure (q1, q2, q3, q4)
    (q1, q2, q3, q4) `shouldBe` (1, 2, 1, 2)


runAction :: (IO (), Config) -> AppTEnv IO Config a -> IO a
runAction (_, config) = runAppTInTestAndThrow config

withStreams :: (IO (), Config) -> IO () -> IO ()
withStreams (_, config) action = do
  _ <- runAppTInTestAndThrow config $ insertStream' art >> insertStream' daut
  action


insertQuote' :: QuotesDb f => ChannelName -> Text -> f Int
insertQuote' c = fmap quoteQid . insertQuote c trollabotUser

insertStream' :: StreamsDb f => ChannelName -> f Int64
insertStream' = fmap _streamId . insertStream
