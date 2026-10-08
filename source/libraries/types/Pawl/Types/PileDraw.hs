module Pawl.Types.PileDraw where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Pile as Pile

-- | CR 406.4: one card to be drawn at random out of a pile of face-down exiled
-- cards. The ordinal tells two draws out of one pile apart, so a slot wanting
-- several cards can name the pile once per card (CR 115.3 restricts the card a
-- draw names, not the pile).
data PileDraw = MkPileDraw
  { pile :: Pile.Pile,
    ordinal :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
