module Pawl.Types.IncreaseSpellCost where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.PlayerEffect's IncreaseSpellCost arm (#1305).
--
-- The amount is GENERIC mana (CR 601.2f), which is why it is a bare Natural
-- where ReduceSpellCost's is a whole ManaCost: no printing taxes a spell by a
-- coloured symbol.
data IncreaseSpellCost = MkIncreaseSpellCost
  { whichSpells :: Filter.Filter Keyword.Keyword,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
