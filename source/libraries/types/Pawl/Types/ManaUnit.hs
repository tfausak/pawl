module Pawl.Types.ManaUnit where

import qualified Data.Set as Set
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ProductionTag as ProductionTag

-- | One unit of mana in a pool.
--
-- FOUR axes, and only the first is a fact about how the mana was made.
-- Pawl.Types.ProductionTag is the CLOSED half -- snow-ness, "this activation
-- caused you to lose life" -- observable facts about the production event that
-- the engine determines with no card knowledge.
--
-- Pawl.Types.ManaRetention is a DURATION something must end (CR 514.2, CR
-- 500.5a) rather
-- than such a fact, and it comes from the wording of the effect that added the
-- mana rather than from any property of its source -- which is why it is a field
-- of its own and not a production tag.
--
-- The RESTRICTION is the OPEN half, and conflating it with the tags would be a
-- mistake: Geosurge's "spend this mana only to cast artifact or creature spells"
-- is a predicate over the spell being paid for, not a fact about this mana, so
-- payment evaluates it through the one generic matcher
-- (Pawl.Engine.Filter.matches) and never cases on what it says.
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
    -- Read off the ADDITION (Pawl.Types.ManaAddition) rather than off the
    -- source. Not implemented: the inline producer reading it --
    -- Pawl.Engine.Mana.manaOptionsOfGiven stamps Ordinary whatever the
    -- instruction says, so only Pawl.Engine.Resolve's arm honours a retaining
    -- clause today (#1808).
    retention :: ManaRetention.ManaRetention,
    -- | CR 106.6: what this mana may be spent on -- Nothing for mana that may
    -- be spent on anything, which is almost every mana. @Just f@ reads "spend
    -- this mana only to cast a spell matching @f@", so a payment that is not a
    -- CAST cannot use it at all (Pawl.Engine.Mana.spendableAmong).
    --
    -- Stamped off the ADDITION (Pawl.Types.ManaAddition) rather than off the
    -- source, for CR 106.6a's reason: the restriction belongs to the spell or
    -- ability that produced the mana and so applies to every mana it produced.
    -- BOTH producers read it -- Pawl.Engine.Mana.manaOptionsOfGiven for a mana
    -- ability paid inline (Mishra's Workshop) and Pawl.Engine.Resolve's
    -- Effect.AddMana arm for one that resolves off the stack (Geosurge).
    --
    -- Not implemented: CR 106.6's other two shapes -- an additional effect on
    -- the spell the mana is spent on (Cavern of Souls' "can't be countered"),
    -- and a delayed triggered ability that triggers when the mana is spent
    -- (#1977). Not implemented either: the restriction whose subject is an
    -- ACTIVATION rather than a cast, Dalakos, Crafter of Wonders' "or activate
    -- abilities of artifacts" (#1975).
    restriction :: Maybe (Filter.Filter Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
