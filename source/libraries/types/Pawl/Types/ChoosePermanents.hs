module Pawl.Types.ChoosePermanents where

import qualified Pawl.Types.ChosenPermanents as ChosenPermanents
import qualified Pawl.Types.SlotName as SlotName

-- | CR 608.2d's battlefield choice bound for a later instruction to act on --
-- Archfiend of Depravity's "that player chooses up to two creatures they
-- control, then sacrifices the rest".
data ChoosePermanents = MkChoosePermanents
  { permanents :: ChosenPermanents.ChosenPermanents,
    -- | Where the chosen permanents are bound, as a group; nothing is bound when
    -- none was chosen, so a later Filter.IsBound matches nothing.
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
