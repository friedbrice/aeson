{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UnitTests.OptionalFields.OmitOptionalFields.TH (omitTH) where

import UnitTests.OptionalFields.Common

$(deriveToJSON
  defaultOptions { omitNothingFields = True }
  ''RecordA)

$(deriveToJSON
  defaultOptions { omitNothingFields = False }
  ''RecordB)

omitTH :: [IO ()]
omitTH =
  [ encodeCase helloWorldRecA helloWorldObj
  , encodeCase helloWorldRecB helloWorldObj
  , encodeCase helloRecA helloObj
  , encodeCase helloRecB helloNullObj
  ]
