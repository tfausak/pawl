module Pawl.Types.CostBasis where

import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.SlotName as SlotName

-- | CR 118.6's "a cost ... based on the mana cost of an object": the mana part
-- of a Pawl.Types.PayGate's cost, described rather than printed -- Flash's
-- "unless you pay its mana cost reduced by {2}".
--
-- The gate's own Pawl.Types.Cost then states NO mana part of its own, which is
-- what a card describing its cost this way prints; Pawl.CardSpec's
-- cardCostBasisStatesMana holds the corpus to it, and CR 118.6 is what makes a
-- stated cost with no mana part the unpayable one this replaces.
--
-- A SLOT and not a whole Pawl.Types.ObjectRef: every printing of this family
-- says "it" about something an earlier clause of the same resolution named, and
-- one object is all "its mana cost" can be about -- a slot holding two names no
-- one cost, which is why Pawl.Engine.Resolve reports the read at
-- SlotArity.One.
--
-- Pawl.Types.MakeForetold.manaCostReducedBy is the same derivation from the
-- other provenance -- CR 702.143d's "its foretell cost is its mana cost reduced
-- by {2}" -- and Pawl.Engine.Cost.reducedManaCost is the one procedure both
-- reach. What that one settles when the card is exiled, this one settles as the
-- gate is offered, there being no earlier moment: CR 118.12 puts the payment at
-- resolution.
data CostBasis = MkCostBasis
  { -- | The object whose mana cost this is, by the slot an earlier clause bound
    -- it at (CR 400.7j) -- Flash's "it" is the creature its first clause put
    -- onto the battlefield.
    --
    -- Read through the projection (CR 613, CR 706.2), so a permanent that is a
    -- copy of another card owes the COPIED mana cost: Pawl.CostSpec's "CR 706.2
    -- a Clone put by Flash is bought at the copied mana cost" is what proves
    -- it.
    --
    -- An object with no mana cost -- a land (CR 202.1b) -- leaves the gate
    -- unpayable, which is rule 118.6's second sentence stated outright. So does
    -- a slot naming nothing.
    slot :: SlotName.SlotName,
    -- | CR 118.7's reduction applied to that mana cost -- Flash's {2}, taken off
    -- by Pawl.Engine.Cost.reducedManaCost exactly as every other reduction is
    -- (CR 118.7a-g).
    --
    -- Not a Maybe: the empty ManaCost IS {0} (CR 118.5), so "its mana cost"
    -- unreduced has one spelling here rather than two.
    reducedBy :: ManaCost.ManaCost
  }
  deriving (Eq, Ord, Show)
