module Pawl.Types.AttachTarget where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.3: attach the slot's object to a permanent matching the filter. WHICH
-- matching permanent is the opcode's to say: Effect.AttachTarget reads the filter
-- as one destination chosen on resolution, Effect.AttachTargetToEach as all of
-- them at once.
data AttachTarget = MkAttachTarget
  { slot :: SlotName.SlotName,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
