module Pawl.Types.Quantity where

import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.ManaCount as ManaCount
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName

-- | A number that may not be a literal number.
--
-- Deliberately NOT called Characteristic: CR 109.3 already uses that word for an
-- object's whole characteristic set (name, mana cost, color, type line, rules
-- text, abilities, power, toughness, loyalty, defense, …). This is just a value.
--
-- Grows further: Half (Little Girl), Infinite (Mox Lotus). Plus is binary and
-- recursive so composition covers the awkward printed values without new cases:
-- 1+* is Plus (Literal 1) Star.
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
    -- supplies, never against a target. Power is PROJECTED (CR 613 layer 7)
    -- where mana value is computed from the printed cost, so this arm goes
    -- through the injected ViewOf -- which is also what lets a caller substitute
    -- CR 608.2h last known information for a source that is gone.
    --
    -- No Toughness sibling: nothing in the pool reads one, and an unused arm is
    -- the speculative construction the project forbids.
    Power
  | -- | A number an EARLIER effect of the same resolution bound into a slot --
    -- Bane of Progress' "for each permanent destroyed this way". Read from the
    -- binding environment of whichever object the evaluation is aimed at; for
    -- Pawl.Engine.Resolve, the effect's SOURCE, which is the same object the
    -- producing effect wrote to.
    --
    -- Named InSlot rather than Bound because PlayerRef.InSlot and
    -- ObjectRef.InSlot already spell "read the binding at this slot" that way.
    --
    -- CR 601.2b's X is THIS arm, at Pawl.Engine.Binding.variableX: #14 retired
    -- the separate constructor once the two were reading the identical field.
    -- What was special about X was never the reading but the LINTS around it --
    -- "reads X iff the cost declares {X}", and the one slot Quantity.slots must
    -- not report, since casting fills it and no card declares it. Those now name
    -- the reserved slot instead of a constructor, which is where the exception
    -- belonged: it is a fact about that slot, not about how a number is read.
    --
    -- Nothing when the slot holds no amount -- the producing effect has not run
    -- yet, or bound nothing. Not 0: "how many were destroyed" is unanswered, not
    -- answered with zero.
    InSlot SlotName.SlotName
  | -- | CR 208.2 / 208.2a: the star printed in a power/toughness box, standing for
    -- a value a characteristic-defining ability defines. NOTATION, not a value:
    -- Quantity.evaluate returns Nothing for it. Projection.baseCharacteristics
    -- resolves it at the seed by substituting Face.characteristicPT, so a Star
    -- is meant to never survive into a projection -- but that is a CONVENTION the
    -- card data must honour, not something this type guarantees. Two cases reach
    -- the evaluator anyway, both benign:
    --
    --   * a card whose OWN characteristicPT itself contained a Star. The seed
    --     substitutes without re-descending into the replacement, so the star
    --     reaches the CDA's evaluator and CR 208.2a makes the undeterminable
    --     value 0. Hypothetical: no card in the pool does this.
    --   * a card with printed Star power/toughness and NO characteristicPT at
    --     all, so there is nothing for the seed to substitute and the Star
    --     reaches evaluate directly, where CR 208.2a's substitution does not
    --     apply (there is no CDA). Real: Primal Plasma, whose star gets its value
    --     from an as-enters REPLACEMENT (CR 208.2b) rather than a CDA (#76).
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
  | -- | A quantity that counts a MANA POOL (CR 106.4) -- Omnath, Locus of Mana's
    -- "for each unspent green mana you have". See Pawl.Types.ManaCount for why
    -- the pool cannot be a Count's Scope and why the payload is its own type
    -- rather than a second shape of Count.
    --
    -- Not part of the knot Count above ties: a ManaCount holds no Quantity, so
    -- this arm is a LEAF and the recursion this type has does not reach it.
    ManaCount ManaCount.ManaCount
  | -- | CR 119.1: a player's life total -- Serra Avatar's "equal to your life
    -- total". A player's own scalar, which is why it is NOT a Count: CR 400.1
    -- scopes a Count over a ZONE and its Aggregation folds over the objects
    -- there, while a life total is one number attached to a player and no
    -- population at all.
    --
    -- The PlayerRef says WHOSE, the same payload Effect.LoseLife and
    -- Effect.GainLife already carry to say whose life changes; this is the read
    -- direction of that pair. CR 109.5's "you" is PlayerRef.Relative
    -- PlayerRelation.You, resolved against the evaluation context -- for a
    -- characteristic-defining ability that is the object's OWN controller (CR
    -- 604.3a(3)), which is what makes Serra Avatar track its controller's life
    -- rather than its source's.
    --
    -- A LEAF, like ManaCount above and unlike Count: it holds no Quantity, so
    -- the recursion this type has does not reach it.
    LifeTotal PlayerRef.PlayerRef
  | -- | CR 702.179: a player's speed -- what CR 702.179e's "max speed" and CR
    -- 702.178a's "as long as your speed is 4" ask about. LifeTotal's sibling in
    -- every respect: one scalar attached to a player rather than a population in
    -- a zone, so it is not a Count, and it carries the same PlayerRef to say
    -- whose.
    --
    -- CR 702.179f is applied by the EVALUATOR, not by this type: a player with no
    -- speed at all (Player.speed of Nothing, CR 702.179b) evaluates to 0 here,
    -- because the rule says their speed IS 0 "for the purpose of an effect that
    -- refers to speed" -- and this constructor is exactly such an effect's
    -- reading. Nothing would be "unanswered", which is a different claim and the
    -- wrong one.
    --
    -- A LEAF, like LifeTotal and ManaCount: it holds no Quantity.
    Speed PlayerRef.PlayerRef
  deriving (Eq, Ord, Show)
