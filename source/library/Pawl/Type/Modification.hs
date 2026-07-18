module Pawl.Type.Modification where

import Pawl.Type.Keyword (Keyword)
import Pawl.Type.Quantity (Quantity)

-- The open-half continuous-effect vocabulary -- its own leaf family (design.md's
-- M3g note: "continuous-effect specifications, classified by layer"), distinct
-- from Effect. The ONLY module that may case on a constructor is Pawl.Projection
-- (Projection.layer classifies it; Projection.applyModification applies it) --
-- the same standing Pawl.Resolve has over Effect. GainKeyword carries a Keyword,
-- a closed-half CITATION (casing on it is not an invariant violation -- see the
-- M2a spec). P/T constructors carry signed Quantity (+3/+3 or a future -1/-1).
data Modification
  = GainKeyword Keyword -- layer 6 (Serpent's Gift)
  | LoseAllAbilities -- layer 6 (Humility)
  | SetBasePowerToughness Quantity Quantity -- layer 7b (Humility 1/1)
  | ModifyPowerToughness Quantity Quantity -- layer 7c (Giant Growth +3/+3)
  deriving (Eq, Ord, Show)
