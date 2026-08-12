{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.AbilityNameSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AbilityName" $ do
  Spec.it s "MkAbilityName" $
    Common.assertCodec
      s
      AbilityName.codec
      (AbilityName.MkAbilityName (Text.pack "a"))
      """ "a" """

  Spec.it s "MkAbilityName, a second name" $
    Common.assertCodec
      s
      AbilityName.codec
      (AbilityName.MkAbilityName (Text.pack "b"))
      """ "b" """

  Spec.it s "has a schema" $
    Common.assertHasSchema s AbilityName.codec

  Spec.it s "rejects a non-string" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ 1 """) >>= Codec.decode AbilityName.codec))
      "expected a decode failure"
