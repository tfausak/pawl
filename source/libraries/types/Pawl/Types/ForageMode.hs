module Pawl.Types.ForageMode where

-- | CR 701.61a: which half of "exile three cards from your graveyard or
-- sacrifice a Food" a foraging player takes.
--
-- A named sum rather than a Bool, Pawl.Types.MutateSide's posture, so a
-- transcript reads as the decision it records.
--
-- The choice belongs to the forager and is asked only where BOTH halves can be
-- carried out -- Pawl.Engine.Forage.forage is where that is decided.
data ForageMode
  = -- | CR 701.61a's first half: exile three cards from the forager's graveyard.
    ExileCards
  | -- | CR 701.61a's second half: sacrifice a Food the forager controls.
    SacrificeFood
  deriving (Bounded, Enum, Eq, Ord, Show)
