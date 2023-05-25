{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.OptionalFields.RequireOptionalFields.Generics (requireGenerics) where

import UnitTests.OptionalFields.Common

instance ToJSON RecordA where
  toJSON = genericToJSON defaultOptions { requireOptionalFields = True }

instance FromJSON RecordA where
  parseJSON = genericParseJSON defaultOptions { requireOptionalFields = True }

instance ToJSON RecordB where
  toJSON = genericToJSON defaultOptions { requireOptionalFields = False }

instance FromJSON RecordB where
  parseJSON = genericParseJSON defaultOptions { requireOptionalFields = False }

requireGenerics :: [IO ()]
requireGenerics =
  [ decodeCase helloWorldRecA helloWorldObj
  , decodeCase helloRecA helloNullObj
  , counterCase (undefined :: proxy RecordA) helloObj
  , decodeCase helloWorldRecB helloWorldObj
  , decodeCase helloRecB helloNullObj
  , decodeCase helloRecB helloObj
  ]
