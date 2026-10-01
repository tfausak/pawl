module Pawl.Types.GraveyardOrder where

-- | A player's standing declaration whether the order of cards put into their
-- graveyard at the same time matters to them (CR 404.3). The player's own
-- setting, like an auto-yield: while it is Indifferent their batches land in
-- the order they moved, unasked, which is the player's choice made in advance
-- rather than the engine's. Indifferent by default; the engine never turns it
-- on, so a client that sees a reader of graveyard order in a deck may.
data GraveyardOrder
  = -- | Any order will do: the batch keeps the order it moved in.
    Indifferent
  | -- | The owner arranges each batch (Prompt.ArrangeGraveyardArrivals).
    Matters
  deriving (Bounded, Enum, Eq, Ord, Show)
