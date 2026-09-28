module Pawl.Types.AnyNumberDiscard where

import qualified Pawl.Types.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.9b / 107.1c's discard: EVERY player the slot names discards any
-- number of the cards in their own hand the payload's Filter matches, up to its
-- ceiling where it has one, each choosing their own -- Borborygmos and
-- Fblthp's "you may discard any number of land cards".
--
-- Pawl.Types.CountedDiscard's shape with the count replaced by the choice, so
-- the slot and the bound `discarded` read exactly as that record's do.
data AnyNumberDiscard = MkAnyNumberDiscard
  { slot :: SlotName.SlotName,
    cards :: AnyNumberMatching.AnyNumberMatching,
    -- | Where the cards this discard moved are written, CountedDiscard's
    -- `discarded` -- Borborygmos and Fblthp's "twice that much", Nantuko
    -- Cultivator's "that many".
    discarded :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
