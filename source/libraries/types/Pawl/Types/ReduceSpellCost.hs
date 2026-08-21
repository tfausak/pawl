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
    reduction :: ManaCost.ManaCost,
    -- | Edgewalker's "This effect reduces only the amount of colored mana you
    -- pay", card text CR 101.1 lets override CR 118.7b-d's spill: a coloured
    -- symbol this cost cannot use does nothing instead of coming off the generic
    -- component. Every printed reducer that names a colour prints this sentence
    -- (Edgewalker, Ragemonger, Bard Class, Morophon, the Dominaria United
    -- Defiler cycle), and the reminder text is what pins it -- "if you cast a
    -- Cleric spell with mana cost {1}{W}, it costs {1} to cast".
    --
    -- False is the RULE and the default on the wire: a reducer that does not
    -- print the sentence spills (CR 118.7b-d), which is what
    -- Pawl.Engine.Cost.applyAdjustments does with it.
    coloredOnly :: Bool
  }
  deriving (Eq, Ord, Show)
