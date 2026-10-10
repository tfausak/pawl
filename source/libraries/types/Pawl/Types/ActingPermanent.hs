module Pawl.Types.ActingPermanent where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | Which permanent's act Pawl.Types.TriggerCondition's PermanentActs watches.
data ActingPermanent
  = -- | CR 603.2: the bearer itself ("when this creature evolves"), compared by
    -- id, so a bearer that has since left is still answered about the event.
    Self
  | -- | CR 603.2 read by a bystander ("whenever a creature you control
    -- explores"), the Filter judging the actor as it is or last was.
    Matching (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
