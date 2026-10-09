module Pawl.Types.PlayerScope where

import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A SET of players, named relative to a perspective: the player-side analogue of
-- Pawl.Types.Affected, and much smaller because CR 109.5 fixes what "you" means.
-- Membership is Pawl.Engine.PlayerEffect.inScope and the set is the fold over it,
-- Pawl.Engine.PlayerEffect.playersInScope -- one reading, two shapes.
--
-- The carriers below differ only in where the perspective comes from, and the
-- first two take CR 109.5's "you":
--
--   * Pawl.Types.PlayerStaticAbility / ActivePlayerEffect -- which players a
--     Pawl.Types.PlayerEffect applies to, against the effect's CONTROLLER.
--   * Pawl.Types.ZoneScope.Scoped -- whose copy of a per-player zone a target
--     slot, a resolution-time sweep or a resolution-time choice reaches (CR
--     400.1), for the readings CR 109.5 answers. That type's OTHER arm names a
--     slot, which is why it is not Pawl.Types.PlayerRef and why every one of
--     those carriers takes a ZoneScope rather than this type: the two
--     PlayerEffect carriers above have no slots for an InSlot arm to be resolved
--     against.
--   * Pawl.Types.PlayerEffect.CantBeTargetedBy -- whose spells and abilities
--     cannot target the player the effect applies to (CR 702.18a's shroud, CR
--     702.11c's hexproof). Its perspective is that PROTECTED player, which is
--     not CR 109.5's "you": the two coincide only because both cards in the pool
--     say "You have ...".
--   * Pawl.Types.ChoosePlayer -- which players a resolving "choose a player"
--     offers (Stadium Vendors), against the resolving controller; the one
--     carrier that is an offer rather than an axis of application.
--   * Pawl.Types.PlayerEffect.CantSearchLibraries, Pawl.Types.AimedPlayers and
--     Pawl.Types.CantAttackPlayer carry it too, each against its own perspective.
--
-- The scope is always resolved DYNAMICALLY on both PlayerEffect carriers. CR
-- 611.2c freezes a stored effect's OBJECT set -- which is what every stored
-- ContinuousEffect does (Affected.TheseObjects) -- but its third sentence carves
-- out an effect that modifies the rules rather than any object, which is this
-- axis. So there is no stored-set analogue of TheseObjects here: this is the same
-- type on the printed and the stored carrier. Freeze it and Silence, which
-- resolves with no opponent spell on the stack, does literally nothing.
data PlayerScope
  = -- | The players in this relation to the perspective (CR 109.5, 102.3).
    Related PlayerRelation.PlayerRelation
  | -- | Damping Engine's "a player who controls more permanents than each other
    -- player". At most ONE player, and often none: the comparison is STRICT, so a
    -- tie for the lead puts nobody in scope.
    --
    -- PERSPECTIVE-FREE, like AnyPlayer: the sentence names no "you" for a
    -- carrier to supply, so Pawl.Engine.PlayerEffect.playersInScope answers it
    -- with an absent perspective. Its membership is a fact about the BOARD
    -- rather than about the two players being compared, which is why inScope
    -- takes a GameState at all.
    ControllingMostPermanents
  deriving (Eq, Ord, Show)
