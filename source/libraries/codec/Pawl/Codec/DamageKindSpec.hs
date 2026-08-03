module Pawl.Codec.DamageKindSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageKind as DamageKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamageKind" $ do
  Spec.it s "Combat" $
    Common.assertJsonCodec
      s
      DamageKind.toJson
      DamageKind.fromJson
      DamageKind.Combat
      "{\"type\":\"Combat\"}"
  Spec.it s "Noncombat" $
    Common.assertJsonCodec
      s
      DamageKind.toJson
      DamageKind.fromJson
      DamageKind.Noncombat
      "{\"type\":\"Noncombat\"}"
