module Pawl.Types.Quantity where

import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.ManaCount as ManaCount
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
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
    --     from an as-enters REPLACEMENT (CR 208.2b). On the battlefield that
    --     replacement has always run before anything reads the power, so the
    --     evaluator sees a number; OFF it, CR 208.2b's last sentence makes the
    --     answer 0, and Projection.printedPower is where that 0 is written --
    --     deliberately not here, since this arm answers for the projection seed,
    --     where a surviving star is a hole rather than a zero.
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
  | -- | CR 725.1: is that player the monarch? 1 if so and 0 if not -- Dawnglade
    -- Regent's "as long as you're the monarch". A DESIGNATION read as a number,
    -- which is what lets Pawl.Types.Condition ask about it at all: that type's
    -- vocabulary is numeric and has exactly one constructor, and Kird Ape's "as
    -- long as you control a Forest" is already a count compared against 1, so a
    -- yes/no is the same comparison with a 0/1 on the measured side.
    --
    -- LifeTotal's and Speed's sibling: one fact attached to a player rather than
    -- a population in a zone, so it is not a Count -- CR 400.1 scopes a Count
    -- over a zone and its Aggregation folds over the objects there, and CR 725.1
    -- makes the monarch a game-wide player designation with no object and no
    -- zone. Nor is it a Filter: a Filter is asked per CANDIDATE OBJECT, and there
    -- is no object here to ask about.
    --
    -- NO MONARCH AT ALL evaluates to 0, not to Nothing, and CR 725.5 is the rule
    -- that says so: "if the result of a continuous effect generated by a static
    -- ability is determined based on who is currently the monarch, but there is
    -- no monarch in the game as that effect begins to apply, that effect does
    -- nothing until a player becomes the monarch". The rule gives the answer, so
    -- the answer is not unanswered -- the same argument Speed above makes for CR
    -- 702.179f's absent speed. Nothing would claim the question could not be put,
    -- which is a different and wrong claim.
    --
    -- A LEAF, like LifeTotal, Speed and ManaCount: it holds no Quantity.
    IsMonarch PlayerRef.PlayerRef
  | -- | CR 122.1: how many counters of a kind a PLAYER has -- CR 728.1's "a
    -- number of cards equal to the number of rad counters they have", and the
    -- shape Ezuri, Claw of Progress' "where X is the number of experience
    -- counters you have" asks for.
    --
    -- LifeTotal's and Speed's sibling: one number attached to a player rather
    -- than a population in a zone, so it is not a Count -- CR 400.1 scopes a
    -- Count over a zone and its Aggregation folds over the objects there, and a
    -- player's counter store is neither. The PlayerRef says whose, the
    -- PlayerCounterKind which.
    --
    -- A counter kind ABSENT from Player.counters reads as 0 rather than as
    -- unanswered, which is that field's own stated convention: a player who has
    -- never been given a rad counter has no rad counters, which is a number.
    --
    -- The OBJECT-counter reading (CR 122.1a-e) is ObjectCounters below, which is
    -- a different arm because it must say which OBJECT where this says which
    -- player.
    --
    -- A LEAF, like LifeTotal, Speed and ManaCount: it holds no Quantity.
    PlayerCounters PlayerRef.PlayerRef PlayerCounterKind.PlayerCounterKind
  | -- | CR 122.1: how many counters of a kind are on the OBJECT this quantity is
    -- evaluated against -- Promising Duskmage's "if it had a +1/+1 counter on
    -- it".
    --
    -- Power's sibling rather than PlayerCounters', and for Power's reason: it
    -- reads the `oid` the caller supplies and is answered through the injected
    -- ViewOf, which is what lets a caller substitute CR 608.2h last known
    -- information for an object that is gone. That substitution is the whole
    -- point here -- CR 613.4c folds a +1/+1 counter into layer 7c's power and
    -- toughness, so a projection taken as the object died records the counter's
    -- EFFECT and never the counter, and only a record of the counters themselves
    -- can answer this.
    --
    -- No ObjectRef beside it: like Power, it names no object at all and takes the
    -- one the evaluation is aimed at. Reading another object's counters would be
    -- a Count over a zone, which is a different shape and one nothing in the pool
    -- asks for.
    --
    -- A kind ABSENT from the object's counters reads as 0 rather than as
    -- unanswered, exactly as PlayerCounters above: an object nobody put a counter
    -- on has none, which is a number. Nothing survives only for an object the
    -- view cannot describe at all.
    --
    -- A LEAF: it holds no Quantity.
    ObjectCounters CounterKind.CounterKind
  | -- | CR 508.3b: how many of that player's opponents were DECLARED attacked
    -- this combat phase -- rule 702.121a's "for each opponent you attacked with a
    -- creature this combat".
    --
    -- LifeTotal's and Speed's sibling in shape: one number attached to a player,
    -- so it is not a Count -- CR 400.1 scopes a Count over a zone, and the combat
    -- record is not one. What it reads is Pawl.Types.Combat.declaredAttacked,
    -- which is exactly the rule's question already: that field exists because CR
    -- 508.4 says a creature put onto the battlefield attacking never "attacked",
    -- and melee's own ruling repeats it.
    --
    -- DECLARED targets rather than live attackers, and the same field rather than
    -- Combat.attacked, is what makes the three printed readings come out right: a
    -- creature that has left combat still counts, a creature that entered
    -- attacking never does, and an opponent who has since left the game counts
    -- too -- nothing here asks who is still playing.
    --
    -- Only OfPlayer entries count. CR 506.3 lets a creature attack a planeswalker
    -- or a battle, and rule 702.121a counts OPPONENTS, so attacking an opponent's
    -- planeswalker and nothing else is a bonus of 0.
    --
    -- The PlayerRef says whose opponents, CR 109.5's "you" being
    -- PlayerRef.Relative PlayerRelation.You. WHO attacked is not recorded and does
    -- not need to be: CR 506.2 makes the attacking player the active player, so
    -- one combat phase's record can only be that player's attacks (#175 is where
    -- CR 802 would break that).
    --
    -- A LEAF, like LifeTotal, Speed and IsMonarch: it holds no Quantity.
    OpponentsAttacked PlayerRef.PlayerRef
  deriving (Eq, Ord, Show)
