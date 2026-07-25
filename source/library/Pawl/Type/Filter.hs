module Pawl.Type.Filter where

import Pawl.Type.CardType (CardType)
import Pawl.Type.Color (Color)
import Pawl.Type.PlayerRelation (PlayerRelation)
import Pawl.Type.Subtype (Subtype)
import Pawl.Type.Supertype (Supertype)

-- A first-order, non-recursive-in-meaning-but-finitely-recursive-in-structure
-- predicate over one object, expressed as data and evaluated by one generic
-- matcher (Pawl.Filter.matches) that never learns which effect produced it. Its
-- atoms case on CHARACTERISTICS (card type, supertype, colour, subtype, power,
-- controller) exactly as the rules already case on a CardType -- casing on a
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
    -- CR 601.2c's "another" (a target slot) and CR 305.2's "each other" (a
    -- continuous effect's affected set) are both written -- one relation, one
    -- spelling, rather than a parallel Exclusion field on each (#163).
    IsSource
  | And [Filter]
  | Or [Filter]
  | Not Filter
  deriving (Eq, Ord, Show)
