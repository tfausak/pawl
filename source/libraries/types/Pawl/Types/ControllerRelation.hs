module Pawl.Types.ControllerRelation where

-- | CR 614.1 / 109.5: whose object a replacement's pattern admits, relative to the
-- controller of the effect's SOURCE (that is what "you" means on a permanent's
-- static ability). Hardened Scales says "a creature you control" (Yours); Rest in
-- Peace's redirect has no controller clause at all (Anyones).
--
-- Judged in ONE place, Pawl.Engine.Replacement.relationHolds, whatever the
-- pattern reads it for -- a player, an object's controller, or an object's
-- owner (CR 400.3, for a zone change). Each reader supplies its own two players
-- and nothing else.
data ControllerRelation
  = Yours
  | Anyones
  | -- | CR 102.2 / 102.3: "an opponent" -- Leyline of the Void's "an opponent's
    -- graveyard". Pawl.TeamSpec's "CR 102.3 a teammate's card is not put into an
    -- opponent's graveyard" is what proves a teammate is not one.
    Opponents
  | -- | CR 303.4b: the player the source enchants -- Wheel of Sun and Moon's
    -- "enchanted player's graveyard".
    EnchantedPlayers
  deriving (Bounded, Enum, Eq, Ord, Show)
