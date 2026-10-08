module Pawl.Types.CostKeyword where

-- | A rule-702 keyword whose whole payload is one [cost], named WITHOUT that
-- cost: what Pawl.Types.Modification's GainKeywordAtManaCost hands out, priced
-- at the receiving card's own mana cost (CR 202.1a). One constructor per
-- keyword a printed "the [keyword] cost is equal to its mana cost" grants.
data CostKeyword
  = -- | CR 702.34a (Lier, Disciple of the Drowned).
    Flashback
  | -- | CR 702.97a (Varolz, the Scar-Striped).
    Scavenge
  | -- | CR 702.128a (Cursecloth Wrappings).
    Embalm
  | -- | CR 702.141a (Wire Surgeons).
    Encore
  | -- | CR 702.168a (Disguise Agent).
    Disguise
  deriving (Bounded, Enum, Eq, Ord, Show)
