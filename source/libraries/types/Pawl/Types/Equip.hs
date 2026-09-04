module Pawl.Types.Equip where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.Keyword's Equip arm: CR 702.6a's "Equip [cost]",
-- plus CR 702.6c's "Equip [quality] creature" as one field on it.
--
-- PARAMETRIC in the keyword, for Pawl.Types.Cycling's reason: the fields name a
-- Cost and a Filter, both of which can name a Keyword, and Keyword names THIS.
-- Only @Equip Keyword.Keyword@ is ever written.
--
-- quality is Nothing for plain equip and Just for CR 702.6c. One field rather
-- than a second Keyword constructor because rule 702.6c narrows the minted
-- ability's TARGET and changes nothing else about it, and rule 702.6d admits
-- both spellings on one permanent under the one name.
data Equip keyword = MkEquip
  { cost :: Cost.Cost keyword,
    quality :: Maybe (Filter.Filter keyword)
  }
  deriving (Eq, Ord, Show)
