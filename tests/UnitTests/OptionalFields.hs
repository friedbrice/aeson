{-# LANGUAGE DerivingVia, DeriveGeneric, UndecidableInstances, KindSignatures, DataKinds, FlexibleInstances, TemplateHaskell #-}

module UnitTests.OptionalFields (optionalFields) where

import UnitTests.OptionalFields.OmitOptionalFields.Generics (omitGenerics)
import UnitTests.OptionalFields.OmitOptionalFields.TH (omitTH)
import UnitTests.OptionalFields.RequireOptionalFields.Generics (requireGenerics)
import UnitTests.OptionalFields.RequireOptionalFields.TH (requireTH)

optionalFields :: [IO ()]
optionalFields = concat [omitGenerics, omitTH, requireGenerics, requireTH]
