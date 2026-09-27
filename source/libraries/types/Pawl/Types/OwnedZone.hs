module Pawl.Types.OwnedZone where

import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Zone as Zone

-- | A destination zone and whose copy of it, as a trigger names one: Enigma
-- Sphinx's "your graveyard", God-Eternal Oketra's "exile". CR 400.3 sends a card
-- to its OWNER's graveyard, library or hand, so the owner is read off the card.
data OwnedZone = MkOwnedZone
  { zone :: Zone.Zone,
    -- | CR 400.3's owner, relative to the ability's controller; AnyPlayer for a
    -- zone the printed text does not scope, or a shared one (CR 400.1).
    owner :: PlayerRelation.PlayerRelation
  }
  deriving (Eq, Ord, Show)
