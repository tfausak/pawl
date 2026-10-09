module Pawl.Types.ControllerRelation where

import qualified Data.Set as Set
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

-- | CR 614.1 / 109.5: whose object a replacement's pattern admits, relative to the
-- controller of the effect's SOURCE (that is what "you" means on a permanent's
-- static ability). Hardened Scales says "a creature you control" (Related You);
-- Rest in Peace's redirect has no controller clause at all (Related AnyPlayer).
--
-- Judged in ONE place, Pawl.Engine.Replacement.relationHolds, whatever the
-- pattern reads it for -- a player, an object's controller, or an object's
-- owner (CR 400.3, for a zone change). Each reader supplies its own two players
-- and nothing else. Pawl.TeamSpec's "CR 102.3 a teammate's card is not put into
-- an opponent's graveyard" proves Related Opponent is CR 102.3's.
data ControllerRelation
  = -- | The player in this relation to the source's controller (CR 109.5, 102.3).
    Related PlayerRelation.PlayerRelation
  | -- | CR 303.4b: the player the source enchants -- Wheel of Sun and Moon's
    -- "enchanted player's graveyard".
    EnchantedPlayers
  | -- | CR 601.2c / 608.2b: the players a slot of the INSTALLING resolution names
    -- -- Plagiarize's "if target player would draw". Pawl.Engine.Resolve.Effect
    -- bakes it into Among as the row is installed; unbaked, it admits nobody.
    InSlot SlotName.SlotName
  | -- | The players an InSlot named when its row was installed. Runtime-only: a
    -- card cannot name a PlayerId, and Pawl.EffectLintSpec keeps the pool from
    -- authoring one.
    Among (Set.Set PlayerId.PlayerId)
  deriving (Eq, Ord, Show)
