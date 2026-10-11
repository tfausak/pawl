module Pawl.Types.Equip where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.EquipTarget as EquipTarget
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.Keyword's Equip arm: CR 702.6a's "Equip [cost]",
-- plus CR 702.6c's "Equip [quality] creature" and CR 702.6e's "Equip
-- planeswalker" as fields on it.
--
-- PARAMETRIC in the keyword, for Pawl.Types.Cycling's reason: the fields name a
-- Cost and a Filter, both of which can name a Keyword, and Keyword names THIS.
-- Only @Equip Keyword.Keyword@ is ever written.
--
-- quality is Nothing for plain equip and Just for CR 702.6c. Fields rather than
-- more Keyword constructors because rules 702.6c and 702.6e change the minted
-- ability's TARGET and nothing else about it (CR 702.6e's "as though that
-- planeswalker were a creature" is the attach it licenses), and rule 702.6e
-- calls the planeswalker form "a variant of the equip ability".
data Equip keyword = MkEquip
  { cost :: Cost.Cost keyword,
    quality :: Maybe (Filter.Filter keyword),
    target :: EquipTarget.EquipTarget
  }
  deriving (Eq, Ord, Show)
