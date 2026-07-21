module Pawl.Type.Quantity where

import Pawl.Type.CountSpec (CountSpec)

-- A number that may not be a literal number.
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
--
data Quantity
  = Literal Integer
  | -- CR 202.3: an object's mana value, computed from its mana cost. A
    -- computed quantity (Opalescence's "base P/T equal to its mana value"),
    -- evaluated against the affected object.
    ManaValue
  | -- CR 601.2b: X -- a value the caster chose while casting, read from the
    -- object's binding environment (Pawl.Binding.variableX). One-shot only: a
    -- continuous effect must FREEZE this to a Literal when stored (Projection.hs
    -- note), which no M4a card exercises.
    X
  | -- CR 208.2 / 208.2a: the star printed in a power/toughness box, standing for
    -- a value a characteristic-defining ability defines. NOTATION, not a value:
    -- Quantity.evaluate returns Nothing for it. Projection.baseCharacteristics
    -- resolves it at the seed by substituting Card.characteristicPT, so a Star
    -- is meant to never survive into a projection -- but that is a CONVENTION the
    -- card data must honour, not something this type guarantees: the seed
    -- substitutes Star for the printed characteristicPT without re-descending
    -- into the replacement, and the codec accepts Star in any Quantity position,
    -- so a card whose OWN characteristicPT itself contained a Star would leave
    -- one in the projection. The consequence is benign (evaluate returns Nothing,
    -- setPT keeps the base) and no card in the pool does this; the
    -- Pawl.CardSpec `cardOffends` lint family is where that check would live if
    -- it is ever added.
    Star
  | -- CR 208.2: composition, so a printed 1+* needs no constructor of its own.
    Plus Quantity Quantity
  | -- A quantity that counts game state (CR 208.2a, CR 608.2h). See CountSpec
    -- for why the payload is its own type.
    Count CountSpec
  deriving (Eq, Ord, Show)
