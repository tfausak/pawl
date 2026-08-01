module Pawl.Codec.DamagePatternSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamagePattern" $ do
  Spec.it s "a named kind" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern (Just DamageKind.Combat))
      "{\"whichKind\":{\"type\":\"Combat\"}}"
  Spec.it s "no kind (matches any)" $
    Common.assertJsonCodec
      s
      DamagePattern.toJson
      DamagePattern.fromJson
      (DamagePattern.MkDamagePattern Nothing)
      "{\"whichKind\":null}"
