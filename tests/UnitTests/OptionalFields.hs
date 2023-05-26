{-# LANGUAGE DerivingVia, DeriveGeneric, UndecidableInstances, KindSignatures, DataKinds, FlexibleInstances, TemplateHaskell #-}

module UnitTests.OptionalFields (optionalFields) where

import UnitTests.OptionalFields.Common
import UnitTests.OptionalFields.Generics (omitGenerics)
import UnitTests.OptionalFields.TH (omitTH)

optionalFields :: [TestTree]
optionalFields = [omitGenerics, omitTH]
