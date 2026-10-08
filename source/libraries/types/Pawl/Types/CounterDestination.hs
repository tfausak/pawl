module Pawl.Types.CounterDestination where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

-- | Where a countered spell goes instead of CR 701.6a's graveyard: Delay's
-- exile, Remand's hand, Memory Lapse's library top, Desertion's battlefield.
--
-- `only` narrows which countered spells it applies to -- Desertion's "if an
-- artifact or creature spell is countered this way" -- and is read off the
-- spell on the stack; absent, it applies to every countered spell.
--
-- `slot` binds the cards the countering put into `zone`, as a group, for a
-- later effect of the same resolution to name -- Delay's "exile it with three
-- time counters on it".
data CounterDestination = MkCounterDestination
  { zone :: Zone.Zone,
    position :: LibraryPosition.LibraryPosition,
    only :: Maybe (Filter.Filter Keyword.Keyword),
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
