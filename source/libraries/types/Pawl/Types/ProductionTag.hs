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
  = -- | CR 107.4h: {S} is paid with mana of any type produced by a snow source
    -- (CR 106.3).
    --
    -- The tag says the mana WAS so produced; whether the permanent that made it
    -- is still on the battlefield, or still snow, is no longer asked. That is why
    -- this is a tag and not a lookup -- mana outlives its source.
    --
    -- Neither a mana type nor a colour (CR 107.4h), so it sits here rather than
    -- in Pawl.Types.ManaType.
    Snow
  | -- | "Mana from a Treasure" (Forsworn Paladin): the mana's source (CR 106.3)
    -- was a permanent with the Treasure subtype.
    --
    -- No comprehensive rule coins the phrase, exactly as
    -- Pawl.Types.SpellCast.ordinal's haddock says of "your second spell each
    -- turn" -- it is plain card text over CR 106.3's source, and this says so
    -- rather than manufacturing a citation.
    Treasure
  deriving (Bounded, Enum, Eq, Ord, Show)
