module Pawl.Types.AnyNumberDiscard where

import qualified Pawl.Types.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.9b / 107.1c's discard: EVERY player the reference names discards any
-- number of the cards in their own hand the payload's Filter matches, up to its
-- ceiling where it has one, each choosing their own -- Borborygmos and
-- Fblthp's "you may discard any number of land cards", Flux's "each player
-- discards any number of cards".
--
-- Pawl.Types.CountedDiscard's shape with the count replaced by the choice. The
-- discarders are a PlayerRef, Pawl.Types.Draw's `player`, rather than that
-- record's slot: Flux's "each player" is no slot a card can declare.
data AnyNumberDiscard = MkAnyNumberDiscard
  { player :: PlayerRef.PlayerRef,
    cards :: AnyNumberMatching.AnyNumberMatching,
    -- | Where the cards this discard moved are written, CountedDiscard's
    -- `discarded` -- Borborygmos and Fblthp's "twice that much", Nantuko
    -- Cultivator's "that many". The UNION across every discarder, so Flux's
    -- per-player "that many" narrows it by Filter.OwnedByRecipient.
    discarded :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
