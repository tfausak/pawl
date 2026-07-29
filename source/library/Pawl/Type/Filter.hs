module Pawl.Type.Filter where

import Pawl.Type.CardType (CardType)
import Pawl.Type.Color (Color)
import Pawl.Type.PlayerRelation (PlayerRelation)
import Pawl.Type.Subtype (Subtype)
import Pawl.Type.Supertype (Supertype)

-- A first-order, non-recursive-in-meaning-but-finitely-recursive-in-structure
-- predicate over one candidate -- an object, or (CR 115.1) a player, since a
-- target may be either -- expressed as data and evaluated by one generic
-- matcher (Pawl.Filter.matches) that never learns which effect produced it. Its
-- atoms case on CHARACTERISTICS (card type, supertype, colour, subtype, power,
-- controller, and for a player candidate its identity) -- and, in IsAttacking's
-- case, on a combat status that is not one (CR 109.3) but that the closed half
-- owns just as squarely -- exactly as the rules
-- already case on a CardType -- casing on a
-- characteristic classification is legitimate; the invariant forbids only casing
-- on an EFFECT's identity, which this type never does.
--
-- Flat, not layered: the atoms and the And/Or/Not combinators are sibling arms of
-- one type, mirroring Pawl.Type.Quantity's flat `Plus Quantity Quantity`
-- alongside its leaf arms. A Simple/combinator split would buy an enforceable
-- normal form only if it also restricted the recursion (CNF/DNF); unrestricted it
-- guarantees nothing the flat type does not.
--
-- `And []` is the trivial predicate -- the identity that matches everything -- so
-- a bare "target creature" (no narrowing) needs no separate "always" arm.
data Filter
  = HasCardType CardType -- CR 205 / 300: the object's card types include this one.
  | HasSupertype Supertype -- CR 205.4: the object's supertypes include this one.
  | HasColor Color -- CR 105.2: the object's colours include this one.
  | HasSubtype Subtype -- CR 205.3: the object's subtypes include this one.
  | PowerAtLeast Integer -- CR 208.1: the object's power is >= this literal.
  | ControlledBy PlayerRelation -- CR 109.5 / 102.2: controller relates thus to the perspective.
  | -- The candidate IS the evaluation's source object. Context-relative in the
    -- same way ControlledBy is: the Filter value carries no object id, and the
    -- answer comes from the Context the caller supplies. `Not IsSource` is how
    -- CR 601.2c's "another" (a target slot) and a continuous effect's own
    -- "each other" card text (an affected set, e.g. Opalescence -- not a rule
    -- number) are both written -- one relation, one spelling, rather than a
    -- parallel Exclusion field on each (#163).
    IsSource
  | -- CR 115.1: the candidate is a PLAYER who relates thus to the perspective --
    -- "target opponent". Context-relative in exactly the way ControlledBy is: it
    -- carries no player id, and the answer comes from the Context the caller
    -- supplies.
    --
    -- Separate from ControlledBy rather than a reuse of it, because they ask
    -- about different things: ControlledBy asks who controls an OBJECT candidate,
    -- and this asks who the candidate IS. CR 109.1's list of what an object is
    -- has no "player" in it, and CR 108.4 gives a controller only to a card
    -- representing a permanent or spell -- so the two are never both answerable
    -- for one candidate.
    IsPlayer PlayerRelation
  | -- CR 508.1k: the candidate is an ATTACKING creature -- a creature declared
    -- as an attacker this combat phase and not since removed from combat
    -- (CR 506.4). "Target attacking creature" (Kill Shot) is the card text this
    -- exists for.
    --
    -- The one atom that reads something CR 109.3 explicitly says is NOT a
    -- characteristic ("any other information about an object isn't a
    -- characteristic"), alongside its examples of tapped-ness and what an Aura
    -- enchants. That is not a breach of the invariant this type's haddock states:
    -- combat status is a RULES concept the closed half already owns (CR 506-511,
    -- Pawl.Type.Combat), so reading it is the same kind of act as reading a card
    -- type. What the invariant forbids is casing on an EFFECT's identity, which
    -- this arm still does not do.
    IsAttacking
  | -- CR 608.2i: the candidate was DECLARED as an attacker earlier this turn --
    -- Relentless Assault's "all creatures that attacked this turn". A look-back
    -- read of the turn-scoped GameEvent log, which is what CR 608.2i sanctions:
    -- "Some effects look back in time and require information about previous
    -- game states and actions rather than considering the current game state."
    -- Never a stamp on the object.
    --
    -- NOT a synonym for IsAttacking, and not expressible in terms of it:
    -- Combat.attackers is wiped by Combat.clearCombat as the end of combat step
    -- ends (CR 511.3), so by the time a postcombat main phase resolves this
    -- spell the combat's attackers are gone from the live record. The event log
    -- is the right footing because it is cleared at turn handoff, which is
    -- exactly the span "this turn" names.
    --
    -- DECLARED, like TriggerCondition.SelfAttacks and for that arm's reason: CR
    -- 508.4 says a creature put onto the battlefield attacking "never attacked",
    -- and only Combat.declareAttackers appends GameEvent.AttackerDeclared.
    --
    -- The fourth atom, after IsAttacking, IsAttachedToCreature and IsToken,
    -- reading something CR 109.3 leaves off the characteristic list. Their
    -- defence covers this one: what happened earlier this turn is a RULES record
    -- the closed half owns outright (CR 608.2i, Pawl.Type.GameEvent), so reading
    -- it is the same kind of act as reading a card type, and casing on an
    -- EFFECT's identity is still what the invariant forbids and still not what
    -- this does.
    AttackedThisTurn
  | -- CR 303.4b / 701.3a: the candidate is ATTACHED to a creature -- "the object
    -- or player an Aura is attached to is called enchanted" -- which is what
    -- Crown of the Ages' "target Aura attached to a creature" narrows by.
    --
    -- The second atom, after IsAttacking, reading something CR 109.3 explicitly
    -- says is not a characteristic -- and it names attachment among its examples
    -- ("what an Aura enchants"). IsAttacking's own defence covers this one word
    -- for word: attachment is a RULES concept the closed half already owns (CR
    -- 301.5, 303.4, 701.3, Object.attachedTo), so reading it is the same kind of
    -- act as reading a card type, and casing on an EFFECT's identity is still
    -- what the invariant forbids and still not what this does.
    --
    -- Nullary and creature-specific rather than a recursive `AttachedTo Filter`
    -- that would compose with the rest of this type: the narrowest atom the one
    -- card in the pool needs. The generalization is #356.
    IsAttachedToCreature
  | -- CR 111.6: "A token isn't a card." Ashaya, Soul of the Wild's "nontoken
    -- creatures you control" is the card text this exists for, and it is spelled
    -- `Not IsToken` -- one relation, one spelling, the way CR 601.2c's "another"
    -- is spelled `Not IsSource` (#163) rather than given a second arm.
    --
    -- The third atom, after IsAttacking and IsAttachedToCreature, reading
    -- something CR 109.3 leaves off the characteristic list. Their defence covers
    -- this one too: what a permanent IS represented by is a RULES concept the
    -- closed half owns outright (CR 111, Pawl.Type.Source), so reading it is the
    -- same kind of act as reading a card type, and casing on an EFFECT's identity
    -- is still what the invariant forbids and still not what this does.
    --
    -- Unlike those two it is not merely uncharacteristic but IMMUTABLE: CR 111.3
    -- makes a token's effect-defined values "functionally equivalent" to printed
    -- ones, so nothing in CR 613 can turn a card into a token or back. That is
    -- what lets Pawl.Projection.filterReads declare it as reading nothing.
    IsToken
  | And [Filter]
  | Or [Filter]
  | Not Filter
  deriving (Eq, Ord, Show)
