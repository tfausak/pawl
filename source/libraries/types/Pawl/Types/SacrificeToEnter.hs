module Pawl.Types.SacrificeToEnter where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 614.1a / 614.12 / Heart of Yavimaya: "if this would enter, sacrifice
-- [count] [filter] instead. If you do, put it onto the battlefield. If you
-- don't, put it into its owner's graveyard."
data SacrificeToEnter = MkSacrificeToEnter
  { count :: Natural.Natural,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
