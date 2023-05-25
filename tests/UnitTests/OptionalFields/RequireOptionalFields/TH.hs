
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module UnitTests.OptionalFields.RequireOptionalFields.TH (requireTH) where

import UnitTests.OptionalFields.Common

$(deriveJSON
  defaultOptions { requireOptionalFields = True }
  ''RecordA)

$(deriveJSON
  defaultOptions { requireOptionalFields = False }
  ''RecordB)

requireTH :: [IO ()]
requireTH =
  [ decodeCase helloWorldRecA helloWorldObj
  , decodeCase helloRecA helloNullObj
  , counterCase (undefined :: proxy RecordA) helloObj
  , decodeCase helloWorldRecB helloWorldObj
  , decodeCase helloRecB helloNullObj
  , decodeCase helloRecB helloObj
  ]
