module Pawl.Types.ReduceSpellCost where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost

-- | The payload of Pawl.Types.PlayerEffect's ReduceSpellCost arm (#1305).
--
-- The reduction is an amount of MANA and not a number, because CR 118.7 reduces
-- by mana of a stated type: Sapphire Medallion's {1} and Edgewalker's {W}{B}
-- are the same shape of thing.
data ReduceSpellCost = MkReduceSpellCost
  { whichSpells :: Filter.Filter Keyword.Keyword,
    reduction :: ManaCost.ManaCost
  }
  deriving (Eq, Ord, Show)
