module Pawl.Types.FullText where

import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Modification's HasFullText arm: CR 612.6's "full
-- text" of the top card of the graveyard the PlayerRef names, plus text of the
-- granting card's own (Volrath's Shapeshifter's "and has the text '{2}: Discard
-- a card.'").
--
-- Parametric in `ability` for Modification's GainAbility reason: the extra text
-- is whole abilities, which this module cannot name.
data FullText ability = MkFullText
  { graveyard :: PlayerRef.PlayerRef,
    -- | Text the object has beside that card's, in the same layer-3 step: the
    -- card says "has the text", so it is not a CR 612.3 granted ability.
    alsoHas :: [ability]
  }
  deriving (Eq, Ord, Show)
