module Pawl.Types.Activations where

import Numeric.Natural (Natural)
import Pawl.Types.Claim (Claim)

-- | What a payment could get out of ONE mana ability: how many times over it
-- could activate it right now, and what each of those activations spends.
-- Pawl.Engine.Cost.manaActivations is the answer; Pawl.Engine.Mana's supply
-- model is the reader.
--
-- CR 118.3's "fully" is why the resources ride along rather than the count going
-- alone. A count is a fact about one source asked by itself, and the supply model
-- has to add several of them up: two sources whose costs each sacrifice a
-- creature both answer 1 beside one creature, and two whose costs each pay 3 life
-- both answer 2 at 6 life. What stops either pair being counted twice over is
-- the claims and the life.
--
-- The claims and the life are ONE activation's, unscaled, whatever the count --
-- the reader multiplies by however many it takes.
data Activations = MkActivations
  { -- | CR 118.3: how many times in a row this player could pay the cost.
    times :: Natural,
    -- | What one activation takes out of a zone (CR 701.21a's sacrifice, a
    -- discard, an exile from a graveyard).
    claims :: [Claim],
    -- | CR 119.4: the life one activation pays.
    life :: Natural
  }
  deriving (Eq, Ord, Show)
