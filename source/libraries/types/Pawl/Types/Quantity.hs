module Pawl.Types.Quantity where

import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.SlotName as SlotName

-- | A number that may not be a literal number.
--
-- Deliberately NOT called Characteristic: CR 109.3 already uses that word for an
-- object's whole characteristic set (name, mana cost, color, type line, rules
-- text, abilities, power, toughness, loyalty, defense, …). This is just a value.
--
-- Grows further: Half (Little Girl), Infinite (Mox Lotus). Star, Plus and Count
-- landed at P3b. Plus is binary and recursive so composition covers the awkward
-- printed values without new cases: 1+* is Plus (Literal 1) Star.
--
-- Deliberately NO Num instance. "Numeric tower" names the problem domain, not a
-- class hierarchy. Num would be lawless and partial once Star and Infinite exist
-- (signum Star? negate Infinite?), which collides with the no-partial-functions
-- rule, and fromInteger would silently erase the very distinction this type
-- exists to draw. Combining is explicit named functions.
data Quantity
  = Literal Integer
  | -- | CR 202.3: an object's mana value, computed from its mana cost. A
    -- computed quantity (Opalescence's "base P/T equal to its mana value"),
    -- evaluated against the affected object.
    ManaValue
  | -- | CR 208.1: the power of the object this quantity is evaluated against --
    -- for an effect, its SOURCE (Ghitu Fire-Eater's "damage equal to its
    -- power"). The projected value, not the printed one, so a pumped source
    -- deals what it currently has.
    --
    -- Sibling of ManaValue, and read the same way: against the `oid` the caller
    -- supplies, never against a target. What makes it different is that power is
    -- PROJECTED (CR 613 layer 7) where mana value is computed from the printed
    -- cost, so this arm goes through the injected ViewOf rather than
    -- Game.cardOf -- which is also what lets a caller substitute CR 608.2h last
    -- known information for a source that is gone.
    --
    -- No Toughness sibling: nothing in the pool reads one, and an unused arm is
    -- the speculative construction the project forbids.
    Power
  | -- | CR 601.2b: X -- a value the caster chose while casting, read from the
    -- object's binding environment (Pawl.Engine.Binding.variableX). One-shot only: a
    -- continuous effect must FREEZE this to a Literal when stored (Projection.hs
    -- note), which no M4a card exercises.
    X
  | -- | A number an EARLIER effect of the same resolution bound into a slot -- Bane
    -- of Progress' "for each permanent destroyed this way", where the sweep's own
    -- Destroy writes how many it actually destroyed and this reads it back. Read
    -- from the binding environment of whichever object the evaluation is aimed at
    -- -- for Pawl.Engine.Resolve, the effect's SOURCE, which is the same object the
    -- producing effect wrote to (Pawl.Engine.Binding.amountOf, the channel X uses).
    --
    -- Named InSlot rather than Bound because PlayerRef.InSlot and ObjectRef.InSlot
    -- already spell "read the binding at this slot" that way; a third reference
    -- type reading a slot uses the same word.
    --
    -- SEPARATE from X above rather than X being `InSlot Binding.variableX`, though
    -- the two arms read the identical field. X is CR 601.2b's value a PLAYER chose
    -- while casting, and being X is what the D4 "reads X iff the cost declares {X}"
    -- lint (Resolve.readsX) is stated over, as well as the one slot read that
    -- Resolve.slotsOf must NOT report; this is a value the ENGINE derived
    -- mid-resolution, which neither lint may see. Collapsing them would restate
    -- both as comparisons against one privileged slot name. Not retiring X's
    -- reserved-slot shim in favour of this arm is #14.
    --
    -- Nothing when the slot holds no amount -- the producing effect has not run
    -- yet, or bound nothing. Not 0: "how many were destroyed" is unanswered, not
    -- answered with zero, and Resolve's arms already no-op on an unevaluable
    -- quantity.
    InSlot SlotName.SlotName
  | -- | CR 208.2 / 208.2a: the star printed in a power/toughness box, standing for
    -- a value a characteristic-defining ability defines. NOTATION, not a value:
    -- Quantity.evaluate returns Nothing for it. Projection.baseCharacteristics
    -- resolves it at the seed by substituting Card.characteristicPT, so a Star
    -- is meant to never survive into a projection -- but that is a CONVENTION the
    -- card data must honour, not something this type guarantees. Two cases reach
    -- the evaluator anyway, both benign:
    --
    --   * a card whose OWN characteristicPT itself contained a Star -- the seed
    --     substitutes Star for the printed characteristicPT without
    --     re-descending into the replacement, and the codec accepts Star in any
    --     Quantity position. The star reaches the CDA's evaluator, which is
    --     Quantity.determine, so CR 208.2a makes the undeterminable value 0.
    --     Hypothetical: no card in the pool does this; the Pawl.CardSpec
    --     `cardOffends` lint family is where that check would live if it is ever
    --     added.
    --   * a card with printed Star power/toughness and NO characteristicPT at
    --     all, so there is nothing for the seed to substitute and the Star
    --     reaches evaluate directly, at the seed, where CR 208.2a's substitution
    --     does not apply (there is no CDA). Real: Primal Plasma (P5) is exactly this --
    --     its star gets a value from an as-enters REPLACEMENT (CR 208.2b), not a
    --     CDA. See Pawl.Engine.Projection's doc comment above `baseColorsOf` for the
    --     consequence in full (#76).
    Star
  | -- | CR 208.2: composition, so a printed 1+* needs no constructor of its own.
    Plus Quantity Quantity
  | -- | A quantity that counts game state (CR 208.2a, CR 608.2h). See
    -- Pawl.Types.Count for why the payload is its own type.
    --
    -- This is where the knot is tied: a Count's Aggregation may itself name a
    -- per-object Quantity (Aggregation.Greatest), so the payload is
    -- `Count Quantity` and the recursion lives in the DATA rather than in a
    -- module cycle. Structural, not a recursive call -- evaluating a Greatest
    -- descends into a strictly smaller Quantity, so the fold terminates.
    Count (Count.Count Quantity)
  deriving (Eq, Ord, Show)
