-- File auto generated by purescript-bridge! --
module ChatBot.DatabaseModels where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (Int, String)

import Prelude
import Data.Generic.Rep (class Generic)

newtype DbCommand =
    DbCommand {
      dbCommandChannel :: String
    , dbCommandName :: String
    , dbCommandBody :: String
    }

derive instance eqDbCommand :: Eq DbCommand
derive instance ordDbCommand :: Ord DbCommand
derive instance genericDbCommand :: Generic DbCommand _
derive instance newtypeDbCommand :: Newtype DbCommand _

--------------------------------------------------------------------------------
_DbCommand :: Iso' DbCommand { dbCommandChannel :: String, dbCommandName :: String, dbCommandBody :: String}
_DbCommand = _Newtype

--------------------------------------------------------------------------------
newtype DbQuote =
    DbQuote {
      dbQuoteChannel :: String
    , dbQuoteText :: String
    , dbQuoteQid :: Int
    }

derive instance eqDbQuote :: Eq DbQuote
derive instance ordDbQuote :: Ord DbQuote
derive instance genericDbQuote :: Generic DbQuote _
derive instance newtypeDbQuote :: Newtype DbQuote _

--------------------------------------------------------------------------------
_DbQuote :: Iso' DbQuote { dbQuoteChannel :: String, dbQuoteText :: String, dbQuoteQid :: Int}
_DbQuote = _Newtype

--------------------------------------------------------------------------------
