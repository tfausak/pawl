module Pawl.Types.MorphVariant where

-- | CR 702.37a / 702.37b: WHICH of rule 702.37's two spellings a morph ability
-- is written in. A field on Pawl.Types.Keyword's Morph rather than a second
-- Keyword constructor, and the rule says so outright: CR 702.37b's last sentence
-- is "A megamorph cost is a morph cost", and its first is "Megamorph is a variant
-- of the morph ability".
--
-- A SIBLING constructor would be the same claim written so that every reader
-- could ignore it. Pawl.Engine.Keyword.morphCost and Pawl.Engine.Cast's
-- face-down gate both read morph through a case with a WILDCARD, so a
-- `Megamorph` constructor beside `Morph` would compile clean and answer Nothing
-- for a megamorph card -- uncastable face down, unturnable face up, with no
-- warning anywhere. Widening the one constructor makes every reader of it break
-- loudly instead, which is what the rule's own wording asks for.
--
-- Mega adds exactly one thing to Plain, and it is not about cost: CR 702.37b's
-- second clause, "As this permanent is turned face up, put a +1/+1 counter on it
-- if its megamorph cost was paid to turn it face up" -- a CR 614.1e replacement
-- effect, minted from this field by Pawl.Engine.Keyword.mintedReplacementsFor
-- the way rule 702.136a's riot row is.
data MorphVariant
  = Plain
  | Mega
  deriving (Eq, Ord, Show)
