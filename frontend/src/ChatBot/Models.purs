-- File auto generated by purescript-bridge! --
module ChatBot.Models where

import ChatBot.IrcMsg (RawIrcMsg)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (Boolean, Int, String)
import Data.Argonaut (Json, class DecodeJson, class EncodeJson)
import Data.Argonaut.Generic (genericDecodeJson, genericEncodeJson)

import Prelude

newtype ChannelName
  = ChannelName
      { _unChannelName :: String
      }


derive instance eqChannelName :: Eq ChannelName
derive instance ordChannelName :: Ord ChannelName
derive instance genericChannelName :: Generic ChannelName _
derive instance newtypeChannelName :: Newtype ChannelName _

instance decodeChannelName :: DecodeJson ChannelName where
    decodeJson = genericDecodeJson
instance encodeChannelName :: EncodeJson ChannelName where
    encodeJson = genericEncodeJson

--------------------------------------------------------------------------------
_ChannelName :: Iso' ChannelName { _unChannelName :: String }
_ChannelName = _Newtype

unChannelName :: Lens' ChannelName String
unChannelName = _Newtype <<< prop (SProxy :: SProxy "_unChannelName")
--------------------------------------------------------------------------------
newtype ChatUser
  = ChatUser
      { cuUserName :: ChatUserName
      , cuMod :: Boolean
      , cuSubscriber :: Boolean
      }

derive instance eqChatUser :: Eq ChatUser
derive instance genericChatUser :: Generic ChatUser _
derive instance newtypeChatUser :: Newtype ChatUser _

instance decodeChatUser :: DecodeJson ChatUser where
    decodeJson = genericDecodeJson
instance encodeChatUser :: EncodeJson ChatUser where
    encodeJson = genericEncodeJson

--------------------------------------------------------------------------------
_ChatUser :: Iso' ChatUser { cuUserName :: ChatUserName
                           , cuMod :: Boolean
                           , cuSubscriber :: Boolean }
_ChatUser = _Newtype
--------------------------------------------------------------------------------

newtype ChatMessage
  = ChatMessage
      { cmUser :: ChatUser
      , cmChannel :: ChannelName
      , cmBody :: String
      , cmRawMessage :: RawIrcMsg
      }

derive instance eqChatMessage :: Eq ChatMessage
derive instance genericChatMessage :: Generic ChatMessage _
derive instance newtypeChatMessage :: Newtype ChatMessage _

instance decodeChatMessage :: DecodeJson ChatMessage where
    decodeJson = genericDecodeJson
instance encodeChatMessage :: EncodeJson ChatMessage where
    encodeJson = genericEncodeJson

--------------------------------------------------------------------------------
_ChatMessage :: Iso' ChatMessage { cmUser :: ChatUser
                                 , cmChannel :: ChannelName
                                 , cmBody :: String
                                 , cmRawMessage :: RawIrcMsg }
_ChatMessage = _Newtype
---------------------------------------------------------------------------------}

newtype ChatUserName
  = ChatUserName
      { cunName :: String
      }


derive instance eqChatUserName :: Eq ChatUserName
derive instance ordChatUserName :: Ord ChatUserName
derive instance genericChatUserName :: Generic ChatUserName _
derive instance newtypeChatUserName :: Newtype ChatUserName _

instance decodeChatUserName :: DecodeJson ChatUserName where
    decodeJson = genericDecodeJson
instance encodeChatUserName :: EncodeJson ChatUserName where
    encodeJson = genericEncodeJson

--------------------------------------------------------------------------------
_ChatUserName :: Iso' ChatUserName { cunName :: String }
_ChatUserName = _Newtype
--------------------------------------------------------------------------------
newtype Command
  = Command
      { commandChannel :: ChannelName
      , commandName :: String
      , commandBody :: String
      }


derive instance eqCommand :: Eq Command
derive instance ordCommand :: Ord Command
derive instance genericCommand :: Generic Command _
derive instance newtypeCommand :: Newtype Command _

instance decodeCommand :: DecodeJson Command where
    decodeJson = genericDecodeJson
instance encodeCommand :: EncodeJson Command where
    encodeJson = genericEncodeJson

--------------------------------------------------------------------------------
_Command :: Iso' Command { commandChannel :: ChannelName
                         , commandName :: String
                         , commandBody :: String }
_Command = _Newtype
--------------------------------------------------------------------------------
newtype Question
  = Question
      { questionChannel :: ChannelName
      , questionBody :: String
      , questionQid :: Int
      }


derive instance eqQuestion :: Eq Question
derive instance ordQuestion :: Ord Question
derive instance genericQuestion :: Generic Question _
derive instance newtypeQuestion :: Newtype Question _

instance decodeQuestion :: DecodeJson Question where
    decodeJson = genericDecodeJson
instance encodeQuestion :: EncodeJson Question where
    encodeJson = genericEncodeJson

--------------------------------------------------------------------------------
_Question :: Iso' Question { questionChannel :: ChannelName
                           , questionBody :: String
                           , questionQid :: Int }
_Question = _Newtype
--------------------------------------------------------------------------------
newtype Quote
  = Quote
      { quoteChannel :: ChannelName
      , quoteBody :: String
      , quoteUser :: ChatUserName
      , quoteQid :: Int
      }


derive instance eqQuote :: Eq Quote
derive instance ordQuote :: Ord Quote
derive instance genericQuote :: Generic Quote _
derive instance newtypeQuote :: Newtype Quote _

instance decodeQuote :: DecodeJson Quote where
    decodeJson = genericDecodeJson
instance encodeQuote :: EncodeJson Quote where
    encodeJson = genericEncodeJson

--------------------------------------------------------------------------------
_Quote :: Iso' Quote { quoteChannel :: ChannelName
                     , quoteBody :: String
                     , quoteUser :: ChatUserName
                     , quoteQid :: Int }
_Quote = _Newtype
--------------------------------------------------------------------------------
