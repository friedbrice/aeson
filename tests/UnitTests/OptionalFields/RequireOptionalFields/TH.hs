{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.OptionalFields.RequireOptionalFields.TH (requireTH) where

import UnitTests.OptionalFields.Common

$(deriveJSON defaultOptions { requireOptionalFields = True } ''RecordA)

$(deriveJSON defaultOptions { requireOptionalFields = False } ''RecordB)

requireTH :: TestTree
requireTH = testGroup "Require optional fields (TH)"
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
