module Pawl.Types.AddSpellCost where

import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.PlayerEffect's AddSpellCost arm (#1679).
--
-- The SPELL-side sibling of Pawl.Types.AddActivationCost, and separate from it
-- for the reason that whole family is split: which MOMENT an arm is asked at is
-- the constructor's to say, since the Filter classifies an object and nothing
-- in a Filter can say "and it is a spell". The Filter here matches the SPELL
-- (Drought's is universal); AddActivationCost's matches the ability's source
-- permanent.
data AddSpellCost = MkAddSpellCost
  { whichSpells :: Filter.Filter Keyword.Keyword,
    components :: [CostComponent.CostComponent Keyword.Keyword],
    scale :: CostScale.CostScale
  }
  deriving (Eq, Ord, Show)
