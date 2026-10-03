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
    amount :: Natural.Natural,
    -- | CR 601.2c / 601.2f: when Just, 'amount' is owed once per distinct
    -- object or player the spell targets that this matches -- Hinata,
    -- Dawn-Crowned's "{1} more to cast for each target". Nothing is a fixed
    -- amount.
    perTarget :: Maybe (Filter.Filter Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
