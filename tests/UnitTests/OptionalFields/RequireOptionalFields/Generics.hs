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

requireGenerics :: TestTree
requireGenerics = testGroup "Require optional fields (Generics)"
  [ testGroup "requireOptionalFields = True"
    [ testCase "Non-null field is valid JSON" $ decodeCase helloWorldRecA helloWorldObj
    , testCase "Null field is valid JSON" $ decodeCase helloRecA helloNullObj
    , testCase "Absent field is not valid JSON" $ counterCase (undefined :: proxy RecordA) helloObj
    ]
  , testGroup "requireOptionalFields = False"
    [ testCase "Non-null field is valid JSON" $ decodeCase helloWorldRecB helloWorldObj
    , testCase "Null field is valid JSON" $ decodeCase helloRecB helloNullObj
    , testCase "Absent field is valid JSON" $ decodeCase helloRecB helloObj
    ]
  ]
