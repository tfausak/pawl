module Pawl.Types.ChoosePermanents where

import qualified Pawl.Types.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName

-- | CR 608.2d's plural battlefield choice made by a NAMED seat and bound for a
-- later instruction to act on -- Archfiend of Depravity's "that player chooses up
-- to two creatures they control, then sacrifices the rest".
--
-- The Filter is read from the ABILITY's perspective whoever chooses,
-- Pawl.Types.ChosenPermanent's reason, and the chooser is a PlayerRef naming ONE
-- seat for that type's reason too.
data ChoosePermanents = MkChoosePermanents
  { chooser :: PlayerRef.PlayerRef,
    permanents :: AnyNumberMatching.AnyNumberMatching,
    -- | Where the chosen permanents are bound, as a group; nothing is bound when
    -- none was chosen, so a later Filter.IsBound matches nothing.
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
