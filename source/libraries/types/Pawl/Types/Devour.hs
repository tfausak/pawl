module Pawl.Types.Devour where

import qualified Pawl.Types.DevourCount as DevourCount
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.Keyword's Devour arm: CR 702.82a's "devour N" and
-- CR 702.82c's "devour [quality] N".
--
-- PARAMETRIC in the keyword for Pawl.Types.Reinforce's reason: the Filter can
-- name a Keyword and Keyword names this. Only @Devour Keyword.Keyword@ is ever
-- written.
data Devour keyword = MkDevour
  { -- | CR 702.82c's [quality], which the sacrifice narrows to. Nothing is rule
    -- 702.82a's bare "creatures", supplied by Pawl.Engine.Keyword rather than by
    -- the card, so the two rules' printings stay distinguishable.
    quality :: Maybe (Filter.Filter keyword),
    count :: DevourCount.DevourCount
  }
  deriving (Eq, Ord, Show)
