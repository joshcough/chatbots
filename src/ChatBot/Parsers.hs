module ChatBot.Parsers where

import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Text.Trifecta

(~~) :: Parser a -> Parser b -> Parser (a, b)
a ~~ b = do
    a' <- a
    b' <- b
    return (a', b')

slurp :: DeltaParsing m => m Text
slurp = cs <$> restOfLine

anything :: DeltaParsing m => m ()
anything = () <$ restOfLine

url :: DeltaParsing m => m Text
url = slurp
