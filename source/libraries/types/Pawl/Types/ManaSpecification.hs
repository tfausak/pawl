module Pawl.Types.ManaSpecification where

-- | CR 106.12a's "or is tapped for mana of a specified type": which mana a tap
-- has to have produced for the ability to trigger. The narrowing half of
-- Pawl.Types.PermanentTappedForMana, beside its subject and its filter.
--
-- A SPECIFICATION and not a Pawl.Types.ManaProduction, which the AddMana opcode
-- carries: that type says how an effect DECIDES the mana it adds, and two of its
-- arms (AnyColor, SnowSymbol) mean nothing asked of mana already in a pool. This
-- one is a predicate over what a resolution produced.
data ManaSpecification
  = -- | CR 106.12a's first half, "is tapped for mana": no narrowing at all, so
    -- any mana the activation produced fires the ability -- Autumn Willow,
    -- Harmony.
    AnyMana
  | -- | CR 607.2d's link, read off the triggered ability's own object: Gauntlet
    -- of Power's "whenever a basic land is tapped for mana of the chosen color",
    -- linked to its "As this artifact enters, choose a color".
    --
    -- A permanent with no colour chosen narrows to nothing rather than to every
    -- colour, Pawl.Engine.Mana.producedTypes' posture for ManaProduction.Chosen
    -- and for its reason: inventing one would be the engine making a player's
    -- choice.
    ChosenColor
  deriving (Bounded, Enum, Eq, Ord, Show)
