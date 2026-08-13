module Pawl.Types.SpellCast where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TurnScope as TurnScope

-- | CR 603.2 over CR 601's cast: which spells fire the ability, and whose turns
-- count -- Monastery Swiftspear's prowess against Kess's once-per-turn window.
data SpellCast = MkSpellCast
  { filter :: Filter.Filter Keyword.Keyword,
    scope :: TurnScope.TurnScope
  }
  deriving (Eq, Ord, Show)
