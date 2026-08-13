module Pawl.Codec.CombatRestriction where

import qualified Pawl.Codec.AffectedUnless as AffectedUnless
import qualified Pawl.Codec.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Codec.LimitUnless as LimitUnless
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CombatRestriction as CombatRestriction

-- | TAGGED, where each arm's payload is an object with named keys: this type is
-- a sum over which declaration the restriction forbids and in what shape.
--
-- The tag's payload is an OBJECT rather than the Affected itself, because every
-- arm carries at least two things and the last is CR 508.1c's "unless" gate.
-- Named keys and not a positional array, for Pawl.Codec.Condition's reason:
-- "affected" and "unless" cannot be swapped by accident. The subject-carrying
-- arms share one record, the SIZE-BOUNDING ones share another, which spells its
-- first key "limit" instead of "affected", and the PAIRWISE one has its own,
-- which adds "blockers" -- so the tag plus the key set is the whole of what
-- distinguishes them.
--
-- The wire format is unchanged by the conversion to a bundle: those three
-- payloads were already named objects, and giving each a record only supplied
-- the name their schema definitions needed.
codec :: Codec.Codec CombatRestriction.CombatRestriction
codec =
  Arm.tagged
    encode
    [ Arm.payload "CantAttack" AffectedUnless.codec CombatRestriction.CantAttack,
      Arm.payload "CantBlock" AffectedUnless.codec CombatRestriction.CantBlock,
      Arm.payload "CantBeBlockedBy" CantBeBlockedBy.codec CombatRestriction.CantBeBlockedBy,
      Arm.payload "CantAttackAlone" AffectedUnless.codec CombatRestriction.CantAttackAlone,
      Arm.payload "CantAttackMoreThan" LimitUnless.codec CombatRestriction.CantAttackMoreThan,
      Arm.payload "CantBlockMoreThan" LimitUnless.codec CombatRestriction.CantBlockMoreThan
    ]
  where
    encode cr = case cr of
      CombatRestriction.CantAttack x -> Common.tagged "CantAttack" . Just $ Codec.encode AffectedUnless.codec x
      CombatRestriction.CantBlock x -> Common.tagged "CantBlock" . Just $ Codec.encode AffectedUnless.codec x
      CombatRestriction.CantBeBlockedBy x -> Common.tagged "CantBeBlockedBy" . Just $ Codec.encode CantBeBlockedBy.codec x
      CombatRestriction.CantAttackAlone x -> Common.tagged "CantAttackAlone" . Just $ Codec.encode AffectedUnless.codec x
      CombatRestriction.CantAttackMoreThan x -> Common.tagged "CantAttackMoreThan" . Just $ Codec.encode LimitUnless.codec x
      CombatRestriction.CantBlockMoreThan x -> Common.tagged "CantBlockMoreThan" . Just $ Codec.encode LimitUnless.codec x
