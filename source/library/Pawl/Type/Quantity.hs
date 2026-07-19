module Pawl.Type.Quantity where

-- A number that may not be a literal number.
--
-- Deliberately NOT called Characteristic: CR 109.3 already uses that word for an
-- object's whole characteristic set (name, mana cost, color, type line, rules
-- text, abilities, power, toughness, loyalty, defense, …). This is just a value.
--
-- Grows: Star (a characteristic-defining ability — Tarmogoyf's power), Plus
-- Quantity Quantity (Tarmogoyf's 1+*), Half (Little Girl), Infinite (Mox Lotus),
-- Variable (X). Plus is binary and recursive so composition covers the awkward
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
  deriving (Eq, Ord, Show)
