{-# LANGUAGE DeriveGeneric, DerivingVia, UndecidableInstances, TypeApplications, ScopedTypeVariables, DuplicateRecordFields #-}

module UnitTests.OptionalFields.Common
  ( module UnitTests.OptionalFields.Common
  , module Data.Aeson
  , module Data.Aeson.TH
  ) where

import Control.Monad (guard)
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

newtype NullableNonEmptyString = NullableNonEmptyString (Maybe String)
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid) via Maybe String

instance ToJSON NullableNonEmptyString where
  toJSON (NullableNonEmptyString x) = toJSON x
  omitOptionalField (NullableNonEmptyString x) = null x

instance FromJSON NullableNonEmptyString where
  parseJSON = fmap nne . parseJSON
  optionalDefault = Just mempty

nne :: String -> NullableNonEmptyString
nne str = case filter (/= ' ') str of
  "" -> NullableNonEmptyString Nothing
  _ -> NullableNonEmptyString (Just str)

obj :: [(Key, Value)] -> Value
obj = Object . KM.fromList

prop :: ToJSON a => String -> a -> (Key, Value)
prop k v = (K.fromString k, toJSON v)

data RecordA = RecordA
  { required :: String
  , optional :: NullableNonEmptyString
  }
  deriving Generic

data RecordB = RecordB
  { required :: String
  , optional :: NullableNonEmptyString
  }
  deriving Generic

encodeCase :: ToJSON a => a -> Value -> IO ()
encodeCase x v = guard $ encode x == encode v

decodeCase :: forall a. (FromJSON a, ToJSON a) => a -> Value -> IO ()
decodeCase x v = guard $ (fmap encode . decode @a . encode) v == Just (encode x)

counterCase :: forall a proxy. (FromJSON a, ToJSON a) => proxy a -> Value -> IO ()
counterCase _ v = guard $ (null . decode @a . encode) v

helloWorldRecA :: RecordA
helloWorldRecA = RecordA "hello" (nne "world")

helloWorldRecB :: RecordB
helloWorldRecB = RecordB "hello" (nne "world")

helloWorldObj :: Value
helloWorldObj = obj
  [ prop "required" "hello"
  , prop "optional" "world"
  ]

helloRecA :: RecordA
helloRecA = RecordA "hello" mempty

helloRecB :: RecordB
helloRecB = RecordB "hello" mempty

helloObj :: Value
helloObj = obj
  [ prop "required" "hello"
  ]

helloNullObj :: Value
helloNullObj = obj
  [ prop "required" "hello"
  , prop "optional" Null
  ]
