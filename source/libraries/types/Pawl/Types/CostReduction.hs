module Pawl.Types.CostReduction where

import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Quantity as Quantity

-- | CR 601.2f: a reduction a spell's OWN printed text applies to its own cost --
-- Thrasta, Tempest's Roar's "This spell costs {3} less to cast for each other
-- spell cast this turn".
--
-- The SELF-scoped sibling of Pawl.Types.ReduceSpellCost, which is a battlefield
-- permanent's static ability discounting OTHER players' spells (Sapphire
-- Medallion). Neither carrier can hold the other's sentence. That one is a CR
-- 613.11 continuous effect gathered off the battlefield
-- (Pawl.Engine.PlayerEffect.printedRows walks it), and a spell reducing its own
-- cost is not on the battlefield when the reduction applies; this one names no
-- spells to match against, because the only spell it reduces is the one it is
-- printed on, and it carries a Quantity where that one carries a literal amount.
--
-- Read straight off the card by Pawl.Engine.Cost.spellAdjustments and NOT
-- through the projection -- Pawl.Types.Face.castingPermissions' precedent, for
-- its reason: the ability is consulted while the object is in a hand or on the
-- stack, neither of which Pawl.Engine.Projection.gather reaches (#160). CR
-- 113.6d is the rule that makes an ability modifying what its own object costs
-- to cast function on the stack.
data CostReduction = MkCostReduction
  { -- | What ONE of the things counted takes off -- Thrasta's {3}.
    --
    -- A ManaCost and not a number, for Pawl.Types.ReduceSpellCost's reason: CR
    -- 118.7 reduces a cost by mana of a stated type, and
    -- Pawl.Engine.Cost.applyAdjustments already reads a reduction's generic and
    -- typed halves apart. No printing of this sentence names a type, so every
    -- one written here is generic; the field is where a typed one would go.
    amount :: ManaCost.ManaCost,
    -- | How many times 'amount' comes off -- Thrasta's "for each other spell
    -- cast this turn", which is a Count over Scope.InHistory
    -- EventShape.SpellCast.
    --
    -- A Quantity rather than a Natural, which is the whole reason this type
    -- exists: a fixed self-reduction is expressible as a Literal, and a scaling
    -- one is not expressible any other way. Evaluated at CR 601.2f, against the
    -- state as it stands there and never against an earlier snapshot -- see
    -- Pawl.Engine.Cost.selfReductions.
    --
    -- Thrasta's "OTHER" needs no exclusion here and gets none: CR 601.2i is what
    -- makes a spell cast, and it comes after CR 601.2f, so the spell being
    -- totalled has filed no GameEvent.SpellCast of its own for the count to pick
    -- up.
    perEach :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
