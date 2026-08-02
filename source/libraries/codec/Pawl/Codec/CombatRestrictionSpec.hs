module Pawl.Codec.CombatRestrictionSpec where

import qualified Pawl.Codec.CombatRestriction as CombatRestriction
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CombatRestriction as CombatRestriction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CombatRestriction" $ do
  -- CR 508.1c: Pacifism's first half, "Enchanted creature can't attack".
  Spec.it s "CantAttack carries its Affected" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantAttack Affected.Attached)
      "{\"type\":\"CantAttack\",\"value\":{\"type\":\"Attached\"}}"
  -- CR 509.1b: Pacifism's second half, "... or block".
  Spec.it s "CantBlock carries its Affected" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantBlock Affected.Attached)
      "{\"type\":\"CantBlock\",\"value\":{\"type\":\"Attached\"}}"
