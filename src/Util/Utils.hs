
module Util.Utils (
    average
  , getRecursiveContents
  , deleteJsonPath
  , deleteJsonPath'
  , emptyObject
  , jsonPath
  , jsonPath'
  , jsonPathV
  , setJsonPath
  , setJsonPath'
  , HasJsonValue(..)
  , mapFromList'
  , prettyJsonText
  , strength
  , (^?)
  , tShow
  --
  , setJsonPathWhoa
  , updateJsonPath
  , updateJsonPath'
  , addElementToList
  , addElementToList'
) where

import           Control.Lens                   (Traversal', alaf, (^?), set)
import           Control.Monad                  (forM)
import           Control.Monad.Except           (MonadIO, liftIO)
import           Data.Aeson                     (ToJSON, Value(..), object)
import           Data.Aeson.Encode.Pretty       (encodePretty)
import           Data.Aeson.Lens                (key)
import qualified Data.ByteString.Lazy.Char8     as BSL
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (genericLength, foldl')
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid
import           Data.Text                      (Text, pack, split)
import           Data.Text.Encoding             (decodeUtf8)
import qualified Data.Vector                    as V
import           System.Directory               (doesDirectoryExist, getDirectoryContents)
import           System.FilePath                ((</>))

-- |
average :: Real a => [a] -> Double
average xs = realToFrac (sum xs) / genericLength xs

-- From Real World Haskell, p. 214
-- |
getRecursiveContents :: MonadIO m => FilePath -> m [FilePath]
getRecursiveContents topPath = liftIO $ do
  names <- getDirectoryContents topPath
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

-- |
emptyObject :: Value
emptyObject = object []

-- |
jsonPath :: [Text] -> Traversal' Value Value
jsonPath = alaf Endo foldMap key

-- |
jsonPath' :: Text -> Traversal' Value Value
jsonPath' = jsonPath . split (== '.')

-- |
jsonPathV :: HasJsonValue a => Text -> a -> Maybe Value
jsonPathV path a = getJson a ^? jsonPath' path

-- | WHOA does this work? why is it here?
setJsonPathWhoa :: Text -> Value -> Value -> Value
setJsonPathWhoa path = set (jsonPath' path)

-- |
setJsonPath' :: Text -> Value -> Value -> Value
setJsonPath' path = setJsonPath $ split (== '.') path

-- |
setJsonPath :: [Text] -> Value -> Value -> Value
setJsonPath path newVal val = case path of
    [] -> val
    [p] -> case val of
        (Object o) -> Object $ HM.insert p newVal o
        _ -> Object $ HM.insert p newVal HM.empty
    (p:ps) -> case val of
        (Object o) -> Object $ HM.insert p (setJsonPath ps newVal val') o
            where val' = fromMaybe (Object HM.empty) $ HM.lookup p o
        _ -> Object $ HM.insert p (setJsonPath ps newVal (Object HM.empty)) HM.empty

-- |
addElementToList' :: Text -> Value -> Value -> Value
addElementToList' path = addElementToList $ split (== '.') path

-- |
addElementToList :: [Text] -> Value -> Value -> Value
addElementToList pathToList valToAdd val = updateJsonPath pathToList val $ \case
    Array as -> Array $ as V.++ V.fromList [valToAdd]
    v -> v -- TODO: what to really do here? throw an error?

-- |
updateJsonPath' :: Text -> Value -> (Value -> Value) -> Value
updateJsonPath' path = updateJsonPath $ split (== '.') path

-- |
updateJsonPath :: [Text] -> Value -> (Value -> Value) -> Value
updateJsonPath path val f = case val ^? jsonPath path of
    Just v -> setJsonPath path (f v) val
    Nothing -> val

-- |
deleteJsonPath' :: Text -> Value -> Value
deleteJsonPath' path = deleteJsonPath $ split (== '.') path

-- |
deleteJsonPath :: [Text] -> Value -> Value
deleteJsonPath path val = case path of
    [] -> val
    [p] -> case val of
        (Object o) -> Object $ HM.delete p o
        _ -> val
    (p:ps) -> case val of
        (Object o) -> case HM.lookup p o of
            Nothing -> val
            Just val' -> Object $ HM.insert p (deleteJsonPath ps val') o
        _ -> val

class HasJsonValue a where
    getJson :: a -> Value

-- | Create a Map from a list, but if there are duplicate keys, collect them into a list.
mapFromList' :: Ord k => [(k, a)] -> Map k [a]
mapFromList' = foldl' insert' Map.empty
    where
    insert' m (k, a) = Map.alter f k m
        where f Nothing   = Just [a]
              f (Just as) = Just (a:as)

-- | For some theory about this function, see https://bartoszmilewski.com/2017/02/06/applicative-functors/
strength :: Functor f => (a, f b) -> f (a, b)
strength (a, fb) = (a,) <$> fb

-- |
prettyJsonText :: ToJSON a => a -> Text
prettyJsonText = decodeUtf8 . BSL.toStrict . encodePretty

-- |
tShow :: Show a => a -> Text
tShow = pack . show
