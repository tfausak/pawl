module Pawl.Types.MadnessCost where

import qualified Pawl.Types.Cost as Cost

-- | The payload of Pawl.Types.Keyword's Madness arm: CR 702.35a's [cost], the
-- one a card exiled by madness is cast for.
--
-- PARAMETRIC in the keyword for Pawl.Types.ForetellCost's reason. Only
-- @MadnessCost Keyword.Keyword@ is ever written.
data MadnessCost keyword
  = -- | CR 702.35a: the cost the card states -- madness {2}{G}.
    Stated (Cost.Cost keyword)
  | -- | CR 702.35a / 202.1: the card's own mana cost -- Falkenrath Gorger's
    -- "the madness cost is equal to its mana cost".
    OwnManaCost
  deriving (Eq, Ord, Show)
