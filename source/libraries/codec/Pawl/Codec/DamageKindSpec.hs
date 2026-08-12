{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DamageKindSpec where

import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageKind as DamageKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamageKind" $ do
  Spec.it s "Combat" $
    Common.assertCodec
      s
      DamageKind.codec
      DamageKind.Combat
      """ {"type":"Combat"} """
  Spec.it s "Noncombat" $
    Common.assertCodec
      s
      DamageKind.codec
      DamageKind.Noncombat
      """ {"type":"Noncombat"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s DamageKind.codec
