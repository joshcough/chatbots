module ChatBot.IrcMsg where

import Prelude

import Data.Array ((:), mapMaybe, snoc, sortWith)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (drop, length, split, take)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)
import Prim (Array, Boolean, Int, String)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Generic (genericDecodeJson, genericEncodeJson)

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
  f (Tuple s' res) e = Tuple (drop (g e) s') (snoc res $ h e)
    where
    g (Left i) = i
    g (Right (Tuple _ i)) = i
    h (Left i) = Left $ take i s'
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
