module Pawl.Types.TargetSlot where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotCount as SlotCount
import qualified Pawl.Types.TargetCount as TargetCount

-- | What a target slot may hold: a closed Pool of candidate recipients (CR 115),
-- narrowed by an open Filter (Nothing = the whole pool, e.g. bare "target
-- creature"). This retires the whole hand-carved family of colour- and
-- type-restricted target constructors (#40): each is now one data value.
--
-- CR 601.2c's "another" is not a third field: it is a Filter.Not inside the
-- Filter, which is what makes the exclusion agree with the Pool's own recipient
-- tagging. A separate field was applied by deleting a Recipient.ToObject, which
-- never matched the ToCreature tags a Creatures pool produces, so "another
-- target creature" did not exclude itself.
--
-- WHAT it excludes is what the printed word points at, and the two readings are
-- different atoms: Filter.IsSource for the ability's own source (Flensing
-- Raptor's "another target creature you control"), and Filter.IsBound for a
-- SIBLING slot of the same announcement (Fall of the Hammer's second slot,
-- "another target creature"). Rule 601.2c makes
-- sharing between two instances of "target" the default, so the second is a
-- restriction the card writes rather than one the machinery imposes;
-- Pawl.Engine.Target.jointlyJudged is where an announcement carrying one is
-- judged whole.
--
-- HOW MANY the slot takes is the `count` field (CR 601.2c), which covers CR
-- 115.6's "up to one", every larger count, "any number of target ..." -- that
-- one by naming no maximum -- and CR 601.2b's announced X. On the slot and not
-- on the mode, because a card makes the call per slot -- Explosive Entry's
-- artifact and creature slots are separately optional.
--
-- `filter` shadows the Prelude's, for the reason Pawl.Types.Count's does.
data TargetSlot = MkTargetSlot
  { pool :: Pool.Pool,
    filter :: Maybe (Filter.Filter Keyword.Keyword),
    count :: SlotCount.SlotCount,
    -- | CR 202.3 / 601.2c: the COMPUTED number this slot's Filter compares a
    -- candidate's mana value against -- Celestine, the Living Saint's "creature
    -- card with mana value X or less ... where X is the amount of life you gained
    -- this turn". Filter.ManaValueAtMostAmount is the atom that reads it, and
    -- Pawl.Engine.Target.slotContext is where it is evaluated and handed over.
    --
    -- HERE rather than in the Filter arm, which is where it belongs on the face of
    -- it: Pawl.Types.Quantity imports Pawl.Types.Count, which imports this
    -- module's Filter, so an arm carrying a Quantity closes a module cycle. The
    -- repo's way out of such a cycle is parametric polymorphism -- Filter is
    -- already parametric in its keyword for exactly that reason -- but a second
    -- parameter does not reach here: Pawl.Types.Keyword itself holds a
    -- `Filter.Filter Keyword` (Protection, Landwalk, Hexproof) and imports no
    -- Quantity, so a quantity parameter on Filter would make Keyword hold one,
    -- and Quantity imports Keyword. Breaking THAT cycle means a quantity
    -- parameter on Keyword too, and on KeywordFamily behind it. A field on the
    -- slot costs one record field and reaches every bound the pool prints.
    --
    -- ON THE SLOT rather than one nullary atom per printed bound (a
    -- PowerLessThanSource-shaped ManaValueAtMostLifeGained), because Betor,
    -- Ancestor's Voice prints the bound against life LOST this turn beside the
    -- gained one: the atom-per-bound shape owes a new atom, a new
    -- Pawl.Engine.Filter.Context field and a new position lint for each, where a
    -- slot naming its own Quantity owes nothing.
    --
    -- Nothing on every slot but the ones that print such a bound, which is all but
    -- a handful: the atom is vacuously False against a Nothing here, and
    -- Pawl.CardSpec's position lint is what keeps a card from writing one into a
    -- slot that names no amount.
    --
    -- A bound that reads a SLOT is answered off the ANNOUNCEMENT rather than off
    -- CR 113.7's source, which carries no such binding: Pawl.Engine.Target's
    -- slotContext hands the environment to Pawl.Engine.Quantity through
    -- Filter.Context's boundAmounts. Both announcements reach it -- CR 603.2's
    -- trigger event (Venerable Warsinger's "where X is the amount of damage this
    -- creature dealt to that player", proved by Pawl.TargetSpec's "CR 603.2 whole
    -- card: the bound is the damage the event carried") and CR 601.2b's announced
    -- X on a spell (Stir the Grave's "mana value X or less", proved by that
    -- module's "CR 601.2c whole card: the bound is the X the caster announced").
    amount :: Maybe Quantity.Quantity
  }
  deriving (Eq, Ord, Show)

-- The ordinary "target creature" slot: exactly one recipient. Named because
-- almost every slot in the engine and the corpus is one, so writing the count out
-- at each would bury the handful that are not.
required :: Pool.Pool -> Maybe (Filter.Filter Keyword.Keyword) -> TargetSlot
required p f = MkTargetSlot p f (SlotCount.Printed TargetCount.one) Nothing

-- CR 115.6 / 601.2c's "up to N targets": a slot the caster may fill any number of
-- times up to N, the empty answer included.
upTo :: Natural.Natural -> Pool.Pool -> Maybe (Filter.Filter Keyword.Keyword) -> TargetSlot
upTo n p f = MkTargetSlot p f (SlotCount.Printed (TargetCount.upTo n)) Nothing

-- CR 601.2c's "any number of target ...": the same slot with no printed ceiling,
-- so the board's candidates are the only bound.
anyNumber :: Pool.Pool -> Maybe (Filter.Filter Keyword.Keyword) -> TargetSlot
anyNumber p f = MkTargetSlot p f (SlotCount.Printed TargetCount.anyNumber) Nothing

-- CR 601.2c with CR 601.2b: a slot taking exactly the X the caster announced
-- ("each of X target creatures", Rot-Curse Rakshasa).
announcedX :: Pool.Pool -> Maybe (Filter.Filter Keyword.Keyword) -> TargetSlot
announcedX p f = MkTargetSlot p f SlotCount.AnnouncedX Nothing

-- CR 601.2c: the same slot with the computed bound its Filter compares against --
-- see `amount` above. Separate from the three builders so that the slots that
-- name no amount, which is almost every slot, stay spelled as they were.
withAmount :: Quantity.Quantity -> TargetSlot -> TargetSlot
withAmount q slot = slot {amount = Just q}
