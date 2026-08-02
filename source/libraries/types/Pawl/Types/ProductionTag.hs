module Pawl.Types.ProductionTag where

-- | A fact about the EVENT that produced one unit of mana, captured as the mana is
-- added and carried by the unit for as long as it exists.
--
-- The CLOSED half of the two collections Pawl.Types.ManaUnit grows; its header
-- says why they are kept apart, and why a unit references a property of its
-- source rather than the source itself. Everything here is a fact the engine
-- determines for itself out of the production event, so casing on this
-- constructor is the same kind of act as casing on a Phase -- never an effect's
-- identity.
data ProductionTag
  = -- | CR 107.4h: "When used in a cost, the snow mana symbol {S} represents a cost
    -- that can be paid with one mana of any type produced by a snow source (see
    -- rule 106.3)."
    --
    -- The tag says the mana WAS so produced; whether the permanent that made it
    -- is still on the battlefield, or is still snow, is no longer asked. That is
    -- the whole reason this is a tag and not a lookup: CR 106.3 makes the source
    -- of an ability's mana the source of that ability, and mana outlives it.
    --
    -- Not a mana type and not a colour, so it sits here rather than in
    -- Pawl.Types.ManaType -- CR 107.4h's last sentence: "Snow is neither a color
    -- nor a type of mana."
    Snow
  deriving (Eq, Ord, Show)
