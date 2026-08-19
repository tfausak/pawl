module Pawl.Types.ManaUnit where

import qualified Data.Set as Set
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ProductionTag as ProductionTag

-- | One unit of mana in a pool.
--
-- THREE axes, and the third is not one of the two collections below.
-- Pawl.Types.ManaRetention is a DURATION something must end (CR 514.2), not a
-- fact about how the mana was made, and it comes from the wording of the effect
-- that added the mana rather than from any property of its source -- which is
-- why it is a field of its own and not a production tag.
--
-- Grows TWO separate collections, and conflating them would be a mistake.
-- Production-time tags are the CLOSED half -- snow-ness, "this activation caused
-- you to lose life" -- observable facts about the production event that the
-- engine determines with no card knowledge. Spending restrictions are the OPEN
-- half: Cavern of Souls' "spend this mana only to cast a creature spell of the
-- chosen type" is a predicate over spells, so payment must ask a classification
-- and must NEVER case on the restriction itself. Only the first is here
-- (Pawl.Types.ProductionTag); spending restrictions are #252.
--
-- Deliberately no source ObjectId. Snow cares about a PROPERTY of the source, not
-- its identity, and a reference would dangle by construction: mana outlives its
-- source, and CR 400.7 mints a fresh id on every zone change. Properties are
-- stamped at production time, by whichever of the two producers is adding the
-- mana: Pawl.Engine.Mana.manaOptionsOfGiven for a mana ability paid inline (CR
-- 605.3b), and Pawl.Engine.Resolve's Effect.AddMana arm for an ability that
-- resolves off the stack (CR 605.1b). Both read them from the same decider,
-- Pawl.Engine.Mana.productionTagsGiven.
data ManaUnit = MkManaUnit
  { manaType :: ManaType.ManaType,
    tags :: Set.Set ProductionTag.ProductionTag,
    -- | CR 106.4: whether the player loses this mana as a step or phase ends.
    -- Stamped by the same two producers as `tags`, but read off the ADDITION
    -- (Pawl.Types.ManaAddition) rather than off the source: a mana ability paid
    -- inline states no retention, so that path always adds Ordinary.
    retention :: ManaRetention.ManaRetention
  }
  deriving (Eq, Ord, Show)
