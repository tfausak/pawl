module Pawl.Types.SpellCast where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

-- | CR 603.2 over CR 601's cast: which spells fire the ability, whose turns
-- count, and which zone the cast came out of -- Monastery Swiftspear's prowess
-- against Kess's once-per-turn window against Harness the Storm's "from your
-- hand".
data SpellCast = MkSpellCast
  { filter :: Filter.Filter Keyword.Keyword,
    scope :: TurnScope.TurnScope,
    -- | CR 601.2a's "moves it from where it is to the stack": which zone that
    -- was. Nothing for the overwhelming majority of cast triggers, which watch
    -- every cast whatever zone it came from.
    --
    -- NOT a conjunct of the Filter above, and for the reason the TurnScope beside
    -- it is not either: CR 400.7 mints the spell as a new object with no memory
    -- of the zone it left, so no characteristic of the candidate can answer the
    -- question. The event carries it instead (Pawl.Types.SpellWasCast.zone).
    --
    -- ONE zone, not a set and not a negation: every printing in the pool names a
    -- single zone. Vega, the Watcher's "from anywhere other than your hand" is
    -- the shape that would want more, and no card here prints it.
    zone :: Maybe Zone.Zone
  }
  deriving (Eq, Ord, Show)
