module Pawl.Codec.BlockRequirementSpec where

import qualified Pawl.Codec.BlockRequirement as BlockRequirement
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.BlockRequirement as BlockRequirement

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  -- Lure's shape (CR 303.4m): the enchanted creature IS the attacker every
  -- creature able to block must block.
  Spec.describe s "Pawl.Codec.BlockRequirement" . Spec.it s "MkBlockRequirement" $
    Common.assertJsonCodec
      s
      BlockRequirement.toJson
      BlockRequirement.fromJson
      (BlockRequirement.MkBlockRequirement Affected.Attached)
      "{\"attacker\":{\"type\":\"Attached\"}}"
