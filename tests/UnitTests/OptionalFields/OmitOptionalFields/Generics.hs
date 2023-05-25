{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.OptionalFields.OmitOptionalFields.Generics (omitGenerics) where

import UnitTests.OptionalFields.Common

instance ToJSON RecordA where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance ToJSON RecordB where
  toJSON = genericToJSON defaultOptions { omitNothingFields = False }

omitGenerics :: [IO ()]
omitGenerics =
  [ encodeCase helloWorldRecA helloWorldObj
  , encodeCase helloWorldRecB helloWorldObj
  , encodeCase helloRecA helloObj
  , encodeCase helloRecB helloNullObj
  ]
