module Pawl.Types.Morph where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.MorphVariant as MorphVariant

-- | The payload of Pawl.Types.Keyword's Morph arm (#1305): CR 702.37a's cost to
-- turn the permanent face up, and which of rule 702.37's two spellings the
-- ability is written in.
--
-- PARAMETRIC in the keyword for Pawl.Types.Cycling's reason: the Cost can name a
-- Keyword and Keyword names this. Only @Morph Keyword.Keyword@ is ever written.
--
-- CR 702.37b's "A megamorph cost is a morph cost" is why the variant sits beside
-- the cost rather than replacing it -- see Pawl.Types.MorphVariant.
data Morph keyword = MkMorph
  { cost :: Cost.Cost keyword,
    variant :: MorphVariant.MorphVariant
  }
  deriving (Eq, Ord, Show)
