module Pawl.Codec.CombatRestriction where

import qualified Pawl.Codec.AffectedUnless as AffectedUnless
import qualified Pawl.Codec.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Codec.LimitUnless as LimitUnless
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
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
    [ Arm.payload "CantAttack" AffectedUnless.codec CombatRestriction.CantAttack (\x -> case x of CombatRestriction.CantAttack y -> Just y; _ -> Nothing),
      Arm.payload "CantBlock" AffectedUnless.codec CombatRestriction.CantBlock (\x -> case x of CombatRestriction.CantBlock y -> Just y; _ -> Nothing),
      Arm.payload "CantBeBlockedBy" CantBeBlockedBy.codec CombatRestriction.CantBeBlockedBy (\x -> case x of CombatRestriction.CantBeBlockedBy y -> Just y; _ -> Nothing),
      Arm.payload "CantAttackAlone" AffectedUnless.codec CombatRestriction.CantAttackAlone (\x -> case x of CombatRestriction.CantAttackAlone y -> Just y; _ -> Nothing),
      Arm.payload "CantAttackMoreThan" LimitUnless.codec CombatRestriction.CantAttackMoreThan (\x -> case x of CombatRestriction.CantAttackMoreThan y -> Just y; _ -> Nothing),
      Arm.payload "CantBlockMoreThan" LimitUnless.codec CombatRestriction.CantBlockMoreThan (\x -> case x of CombatRestriction.CantBlockMoreThan y -> Just y; _ -> Nothing)
    ]
