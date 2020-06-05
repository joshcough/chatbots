module ChatBot.IrcMsg where

import Prelude

import Data.Array ((:), drop, many, some, mapMaybe, snoc, sortWith, fromFoldable, take)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Functor ((<$))
import Data.Int (fromString)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, optional)
import Data.Newtype (class Newtype, unwrap)
import Data.String (contains, joinWith, length, replaceAll, singleton, split)
import Data.String as String
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..), fst, snd)
import Prim (Array, Boolean(..), Char, Int, String)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Generic (genericDecodeJson, genericEncodeJson)

import Control.Alternative ((<|>))
--import Data.List (List(..))
import Data.String.CodeUnits as SCU
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as P
import Text.Parsing.Parser.Token as P
import Text.Parsing.Parser.Combinators as P

newtype UserInfo
  = UserInfo
      { userName :: String
      , userNick :: String
      , userHost :: String
      }

derive instance eqUserInfo :: Eq UserInfo
derive instance ordUserInfo :: Ord UserInfo
derive instance genericUserInfo :: Generic UserInfo _
derive instance newtypeUserInfo :: Newtype UserInfo _

instance decodeUserInfo :: DecodeJson UserInfo where
    decodeJson = genericDecodeJson
instance encodeUserInfo :: EncodeJson UserInfo where
    encodeJson = genericEncodeJson

----------------------------------------------------------------------------------
_UserInfo :: Iso' UserInfo { userName :: String
                           , userNick :: String
                           , userHost :: String }
_UserInfo = _Newtype
----------------------------------------------------------------------------------

type TagEntry = Tuple String String

newtype RawIrcMsg
  = RawIrcMsg
      { _msgCommand :: String
      , _msgTags    :: Array TagEntry
      , _msgParams  :: Array String
      , _msgPrefix  :: Maybe UserInfo
      }

derive newtype instance showUserInfo :: Show UserInfo
derive newtype instance showIrcMsg :: Show RawIrcMsg
derive instance eqIrcMsg :: Eq RawIrcMsg
derive instance ordIrcMsg :: Ord RawIrcMsg
derive instance genericIrcMsg :: Generic RawIrcMsg _
derive instance newtypeIrcMsg :: Newtype RawIrcMsg _

instance decodeIrcMsg :: DecodeJson RawIrcMsg where
    decodeJson = genericDecodeJson
instance encodeIrcMsg :: EncodeJson RawIrcMsg where
    encodeJson = genericEncodeJson

----------------------------------------------------------------------------------
_IrcMsg :: Iso' RawIrcMsg { _msgCommand :: String
                          , _msgTags    :: Array (Tuple String String)
                          , _msgParams  :: Array String
                          , _msgPrefix  :: Maybe UserInfo }
_IrcMsg = _Newtype
----------------------------------------------------------------------------------

type EmoteId = Int
newtype EmotePos = EmotePos { emoteId :: EmoteId, startIndex :: Int, endIndex :: Int }

emoteLink :: EmoteId -> String
emoteLink id = "https://static-cdn.jtvnw.net/emoticons/v1/" <> show id <> "/1.0"

type Tags = {
   badgeInfo :: Maybe String
 , badges :: Array String
 , color :: Maybe String
 , displayName :: Maybe String
 , emotesPositions :: Array EmotePos
 , flags :: Array String
 , tagId :: Maybe String
 , mod :: Boolean
 , roomId :: Maybe String
 , subscriber :: Boolean
 , tmiSentTs :: Maybe String
 , turbo :: Boolean
 , userId :: Maybe String
 , userType :: Maybe String
}

parseMsgParams :: RawIrcMsg -> String
parseMsgParams (RawIrcMsg r) = f r._msgParams
  where
  f [_, msg] = msg
  f _ = ""

-- Example: "1531159:0-11,13-24,46-57/1671268:26-34,36-44"
parseEmotesPositions :: String -> Array EmotePos
parseEmotesPositions es = sortWith getStart $ split (Pattern "/") es >>= parseEmote
  where
  getStart :: EmotePos -> Int
  getStart (EmotePos r) = r.startIndex
  parseEmote :: String -> Array EmotePos
  parseEmote s = case split (Pattern ":") s of
    [id, positions] -> mapMaybe (parseSinglePosition id) $ split (Pattern ",") positions
    _ -> []
  parseSinglePosition :: String -> String -> Maybe EmotePos
  parseSinglePosition id pos = case split (Pattern "-") pos of
    [start, end] -> do
       id' <- fromString id
       start' <- fromString start
       end' <- fromString end
       pure $ EmotePos { emoteId: id', startIndex: start', endIndex: end' }
    _ -> Nothing

parseBool :: String -> Boolean
parseBool "1" = true
parseBool _ = false

{-
Example tags values:
"_msgTags": [
    [ "badge-info", "subscriber/26" ],
    [ "badges", "broadcaster/1,subscriber/0,premium/1" ],
    [ "color", "#B22222" ],
    [ "display-name", "ArtOfTheTroll" ],
    [ "emotes",  "1671268:26-34,36-44/425618:52-54/1531159:0-11,13-24" ],
    [ "flags", "" ],
    [ "id", "f599c772-b8a6-4e74-b71a-ce858a894488" ],
    [ "mod", "0" ],
    [ "room-id", "156467570" ],
    [ "subscriber", "1" ],
    [ "tmi-sent-ts", "1571791338011" ],
    [ "turbo", "0" ],
    [ "user-id", "156467570" ],
    [ "user-type", "" ]
-}
parseTags :: RawIrcMsg -> Tags
parseTags (RawIrcMsg r) =
  { badgeInfo: get "badge-info"
  , badges: maybe [] (\b -> [b]) $ get "badges"
  , color: get "color"
  , displayName: get "display-name"
  , emotesPositions: maybe [] parseEmotesPositions $ get "emotes"
  , flags: maybe [] (\b -> [b]) $ get "flags"
  , tagId: get "id"
  , mod: maybe false parseBool $ get "mod"
  , roomId: get "room-id"
  , subscriber: maybe false parseBool $ get "subscriber"
  , tmiSentTs: get "tmi-sent-ts"
  , turbo: maybe false parseBool $ get "turbo"
  , userId: get "user-id"
  , userType: get "user-type"
  }
  where
  tagsMap = Map.fromFoldable r._msgTags
  get k = Map.lookup k tagsMap

tagsAndSections :: RawIrcMsg -> Tuple Tags (Array (Either String String))
tagsAndSections raw = Tuple tags $ sections (parseMsgParams raw) tags.emotesPositions
  where
  tags = parseTags raw

sections :: String -> Array EmotePos -> Array (Either String String)
sections s = snd <<< foldl f (Tuple s []) <<< sectionLengths (length s)
  where
  f (Tuple s' res) e = Tuple (String.drop (g e) s') (snoc res $ h e)
    where
    g (Left i) = i
    g (Right (Tuple _ i)) = i
    h (Left i) = Left $ String.take i s'
    h (Right (Tuple emoteId _)) = Right $ emoteLink emoteId

sectionLengths :: Int -> Array EmotePos -> Array (Either Int (Tuple EmoteId Int))
sectionLengths end = final <<< foldl f (Tuple 0 [])
  where
  final (Tuple n rs) = if n == end then rs else snoc rs (Left $ end - n)
  f (Tuple currentIndex res) (EmotePos e) =
     let l = Left $ e.startIndex - currentIndex
         r = Right (Tuple e.emoteId $ e.endIndex - e.startIndex + 1)
         next = e.endIndex + 1
     in Tuple next $ if currentIndex == e.startIndex then snoc res r else res <> [l, r]


-- | Construct a new 'RawIrcMsg' without a time or prefix.
rawIrcMsg :: String -> Array String -> RawIrcMsg
rawIrcMsg command params = RawIrcMsg
      { _msgCommand : command
      , _msgTags    : []
      , _msgParams  : params
      , _msgPrefix  : Nothing
      }

-- | PASS command
ircPass :: String -> RawIrcMsg
ircPass password = rawIrcMsg "PASS" [password]

-- | NICK command
ircNick :: String ->  RawIrcMsg
ircNick nick = rawIrcMsg "NICK" [nick]

-- | CAP REQ command
ircCapReq :: Array String -> RawIrcMsg
ircCapReq capabilities = rawIrcMsg "CAP" ["REQ", joinWith " " capabilities]

-- | PING command
ircPing ::
  Array String {- ^ parameters -} ->
  RawIrcMsg
ircPing = rawIrcMsg "PING"

-- | JOIN command
ircJoin ::
  String       {- ^ channel -} ->
  Maybe String {- ^ key     -} ->
  RawIrcMsg
ircJoin chan (Just key) = rawIrcMsg "JOIN" [chan, key]
ircJoin chan Nothing    = rawIrcMsg "JOIN" [chan]

-- | Serialize a structured IRC protocol message back into its wire
-- format. This command adds the required trailing newline.
renderRawIrcMsg :: RawIrcMsg -> String
renderRawIrcMsg (RawIrcMsg m)
   = renderTags m._msgTags
  <> maybe mempty renderPrefix m._msgPrefix
  <> m._msgCommand
  <> " "
  <> buildParams m._msgParams
  <> "\r"
  <> "\n"

  where

  renderTags :: Array TagEntry -> String
  renderTags [] = mempty
  renderTags xs = "@" <> joinWith ";" (map renderTag xs) <> " "

  renderTag :: TagEntry -> String
  renderTag (Tuple key val)
    | String.null val = key
    | otherwise = key <> "=" <> escapeTagVal val

  escapeTagVal :: String -> String
  escapeTagVal =
    replaceAll (Pattern ";") (Replacement "\\:") <<<
    replaceAll (Pattern " ") (Replacement "\\s") <<<
    replaceAll (Pattern "\\") (Replacement "\\\\") <<<
    replaceAll (Pattern "\r") (Replacement "\\r") <<<
    replaceAll (Pattern "\n") (Replacement "\\n")

  renderPrefix :: UserInfo -> String
  renderPrefix u = ":" <> renderUserInfo u <> " "

  buildParams :: Array String -> String
  buildParams = joinWith " "

  -- | Render 'UserInfo' as @nick!username\@hostname@
  renderUserInfo :: UserInfo -> String
  renderUserInfo (UserInfo r)
      = r.userName
     <> (if String.null r.userNick then "" else "!" <> r.userNick)
     <> (if String.null r.userHost then "" else "@" <> r.userHost)

{-
TODO: this was:
-- | Concatenate a list of parameters into a single, space-delimited
-- bytestring. Use a colon for the last parameter if it starts with
-- a colon or contains a space.
buildParams :: [Text] -> Builder
buildParams [x]
  | " " `Text.isInfixOf` x || ":" `Text.isPrefixOf` x || Text.null x
  = Builder.char8 ' ' <> Builder.char8 ':' <> Text.encodeUtf8Builder x
buildParams (x:xs)
  = Builder.char8 ' ' <> Text.encodeUtf8Builder x <> buildParams xs
buildParams [] = mempty
---}
--
---- | Parse a whole IRC message assuming that the trailing
---- newlines have already been removed. This parser will
---- parse valid messages correctly but will also accept some
---- invalid messages. Presumably the server isn't sending
---- invalid messages!
--rawIrcMsgParser :: P.Parser String RawIrcMsg
--rawIrcMsgParser =
--  do tags   <- fromMaybe [] <$> guarded '@' tagsParser
--     prefix <- guarded ':' prefixParser
--     cmd    <- simpleTokenParser
--     params <- paramsParser maxMiddleParams
--     pure $ RawIrcMsg
--       { _msgTags    : tags
--       , _msgPrefix  : prefix
--       , _msgCommand : cmd
--       , _msgParams  : params
--       }
--
---- | RFC 2812 specifies that there can only be up to
---- 14 "middle" parameters, after that the fifteenth is
---- the final parameter and the trailing : is optional!
--maxMiddleParams :: Int
--maxMiddleParams = 14
--
--tagsParser :: P.Parser String (List TagEntry)
--tagsParser = tagParser `P.sepBy1` P.char ';' <* P.whiteSpace
--
--tagParser :: P.Parser String TagEntry
--tagParser = pure $ Tuple "x" $ unescapeTagVal "x"
----  do key <- takeWhile (not <<< flip contains "=; " <<< Pattern <<< singleton)
----     _   <- optional (P.char '=')
----     val <- takeWhile (not <<< flip contains "; " <<< Pattern <<< singleton)
----     pure $ Tuple key (unescapeTagVal val)
--
---- | Parse a rendered 'UserInfo' token.
--prefixParser :: P.Parser String UserInfo
--prefixParser =
--  do tok <- simpleTokenParser
--     pure $ parseUserInfo tok
--
---- | Take the next space-delimited lexeme
--simpleTokenParser :: P.Parser String String
--simpleTokenParser =
--  do xs <- takeWhile1 (\c -> c /= ' ')
--     _ <- P.whiteSpace
--     pure xs
--
---- | When the current input matches the given character parse
---- using the given parser.
--guarded :: forall b. Char -> P.Parser String b -> P.Parser String (Maybe b)
--guarded c p =
--  do success <- optionalChar c
--     if success then Just <$> p else pure Nothing
--
---- | Returns 'True' iff next character in stream matches argument.
--optionalChar :: Char -> P.Parser String Boolean
--optionalChar c = true <$ P.char c <|> pure false
--
--takeWhile :: (Char -> Boolean) -> P.Parser String String
--takeWhile f = do
--  cs <- many $ P.satisfy f
--  pure $ SCU.fromCharArray cs
--
--takeWhile1 :: (Char -> Boolean) -> P.Parser String String
--takeWhile1 f = do
--  cs <- some $ P.satisfy f
--  pure $ SCU.fromCharArray cs
--
--
--unescapeTagVal :: String -> String
--unescapeTagVal = String.fromCodePointArray <<< aux <<< String.toCodePointArray
--
--aux :: Array CodePoint -> Array CodePoint
--aux arr = case take 2 arr of
--  [x, y] | f x y '\\' ':' -> []
--  _ -> []
--
----  ['\\',':']  -> codePointFromChar ';'  : aux (drop 2 arr)
----  ['\\','s']  -> codePointFromChar ' '  : aux (drop 2 arr)
----  ['\\','\\'] -> codePointFromChar '\\' : aux (drop 2 arr)
----  ['\\','r']  -> codePointFromChar '\r' : aux (drop 2 arr)
----  ['\\','n']  -> codePointFromChar '\n' : aux (drop 2 arr)
----  [x,y]       -> x : aux (y : drop 2 arr)
----  [x]         -> [x]
----  []          -> []
--  where
--  f x y c1 c2 = x == codePointFromChar c1 && y == codePointFromChar c2
--
--
---- | Split up a hostmask into a nickname, username, and hostname.
---- The username and hostname might not be defined but are delimited by
---- a @!@ and @\@@ respectively.
--parseUserInfo :: String -> UserInfo
--parseUserInfo x = UserInfo
--  { userNick : nick
--  , userName : String.drop 1 user
--  , userHost : String.drop 1 host
--  }
--  where
--    breakOn c s = case String.indexOf (Pattern c) s of
--      Nothing -> { after: "", before: s}
--      Just n -> String.splitAt n s
--
--    r1 = breakOn ("@") x
--    nickuser = r1.before
--    host = r1.after
--    r2 = breakOn ("!") nickuser
--    nick = r2.before
--    user = r2.after
--
--paramsParser :: Int -> P.Parser String (Array String)
--paramsParser _ = pure []


-- TODO:
{-

-- | Parse the list of parameters in a raw message. The RFC
-- allows for up to 15 parameters.
-- Int arg is: possible middle parameters
paramsParser ::
  Int -> Parser [Text]
paramsParser !n =
  do end <- P.atEnd
     if end
       then return []
       else do isColon <- optionalChar ':'
               if isColon || n == 0
                 then finalParam
                 else middleParam

  where

  finalParam =
    do x <- takeText
       let !x' = Text.copy x
       return [x']

  middleParam =
    do x  <- simpleTokenParser
       xs <- paramsParser (n-1)
       return (x:xs)
-}