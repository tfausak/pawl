module Pawl.Types.KeywordTally where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Scope as Scope

-- | The payload of Pawl.Types.KeywordCount's Tally arm: Pawl.Types.Count's scope
-- and filter, counted by members.
--
-- Not Pawl.Types.Count itself, which names Keyword concretely and so cannot sit
-- inside one. PARAMETRIC in the keyword for Pawl.Types.Devour's reason.
--
-- `filter` shadows the Prelude's, as Count's does.
data KeywordTally keyword = MkKeywordTally
  { scope :: Scope.Scope,
    filter :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
