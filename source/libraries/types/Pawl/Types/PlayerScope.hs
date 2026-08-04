module Pawl.Types.PlayerScope where

-- | A SET of players, named relative to a perspective: the player-side analogue of
-- Pawl.Types.Affected, and much smaller because CR 109.5 fixes what "you" means.
-- Membership is Pawl.Engine.PlayerEffect.inScope and the set is the fold over it,
-- Pawl.Engine.PlayerEffect.playersInScope -- one reading, two shapes.
--
-- THREE carriers. They differ only in where the perspective comes from, and the
-- first two take CR 109.5's "you":
--
--   * Pawl.Types.PlayerStaticAbility / ActivePlayerEffect -- which players a
--     Pawl.Types.PlayerEffect applies to, against the effect's CONTROLLER.
--   * Pawl.Types.Pool.CardsInGraveyard -- whose graveyards a target slot draws
--     from (CR 400.1). That carrier is why this is not Pawl.Types.PlayerRef,
--     whose InSlot arm names a target slot CR 601.2c has not filled yet.
--   * Pawl.Types.PlayerEffect.CantBeTargetedBy -- whose spells and abilities
--     cannot target the player the effect applies to (CR 702.18a's shroud, CR
--     702.11c's hexproof). Its perspective is that PROTECTED player, which is
--     not CR 109.5's "you": the two coincide only because both cards in the pool
--     say "You have ...".
--
-- The scope is always resolved DYNAMICALLY on both PlayerEffect carriers. CR
-- 611.2c freezes a stored effect's OBJECT set -- which is what every stored
-- ContinuousEffect does (Affected.TheseObjects) -- but its third sentence carves
-- out an effect that modifies the rules rather than any object, which is this
-- axis. So there is no stored-set analogue of TheseObjects here: this is the same
-- type on the printed and the stored carrier. Freeze it and Silence, which
-- resolves with no opponent spell on the stack, does literally nothing.
data PlayerScope
  = -- | CR 109.5: the effect's controller -- or, on the CantBeTargetedBy
    -- carrier, the protected player the scope is anchored to. "The one player
    -- the perspective names", whichever carrier supplied it.
    You
  | -- | Every other player. Not a two-player shortcut: CR 806.1 has a
    -- free-for-all's players compete as individuals, so every other player is an
    -- opponent by construction, and CR 102.2 says the same for two players --
    -- `pid /= controller` is the one predicate that serves both. CR 102.3 is the
    -- ONE reading this is wrong for -- a teammate is not an opponent -- and pawl
    -- has no teams to express (#175).
    Opponents
  | -- | Every player, the controller included ("including your own", Thalia's own
    -- ruling).
    EachPlayer
  deriving (Eq, Ord, Show)
