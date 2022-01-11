{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Imports
  ( UTCTime
  , printf
  , Json(..)
  , intercalate
  , when
  , sortOn
  , (&)
  , suchThatMap
  , Text
  , Map
  , Set
  , Generic
  , encode
  , decode
  , Serialize
  , foldlM
  , bool
  , (<=<)
  , lookupEnv
  , IsString
  , doesFileExist
  , Bifunctor(..)
  , Endo(..)
  , Arbitrary(..)
  , choose
  , ToJSON
  , FromJSON
  , Proxy(..)
  , FromJSONKey
  , oneof
  , genericShrink
  , elements
  , subsequences
  , inits
  , ToJSONKey
  , aesonOptions
  , module Prelude
  ) where

import Data.String (IsString)
import Data.Foldable (foldlM)
import Data.Serialize (encode, decode, Serialize)
import Prelude hiding (Ordering)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.Set (Set)
import Data.Map (Map)
import GHC.Generics (Generic, Rep)
import Data.Bool (bool)
import Control.Monad ((<=<), when)
import System.Environment (lookupEnv)
import System.Directory (doesFileExist)
import Data.Function ((&))
import Data.Bifunctor (Bifunctor(..))
import Data.List (sortOn, subsequences, inits, intercalate)
import Data.Monoid (Endo(..))
import Test.QuickCheck (Arbitrary(..), choose, oneof, genericShrink, elements, suchThatMap)
import Text.Printf (printf)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy(..))

newtype Json a = Json { unJson :: a }

instance (Generic a, Aeson.GToJSON' Aeson.Encoding Aeson.Zero (Rep a), Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) => ToJSON (Json a) where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions . unJson
  toJSON = Aeson.genericToJSON Aeson.defaultOptions . unJson

instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => FromJSON (Json a) where
  parseJSON val = Json <$> Aeson.genericParseJSON Aeson.defaultOptions val

aesonOptions :: Aeson.Options
aesonOptions = Aeson.defaultOptions
