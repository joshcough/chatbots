module ChatBot.WebSocket.Parsers where

import           Protolude

import           Data.String.Conversions (cs)
import           Data.Text               (pack)
import           Text.Trifecta

(~~) :: Parser a -> Parser b -> Parser (a, b)
a ~~ b = (,) <$> a <*> b

slurp :: DeltaParsing m => m Text
slurp = cs <$> restOfLine

anything :: DeltaParsing m => m ()
anything = () <$ restOfLine

url :: DeltaParsing m => m Text
url = slurp

number :: DeltaParsing m => m Int
number = fromIntegral <$> integer

commandName :: DeltaParsing m => m Text
commandName = do
  _ <- string "!"
  name <- pack <$> many alphaNum
  return $ "!" <> name
