module Pawl.Type.PermanentCriterion where

import Pawl.Type.Subtype (Subtype)

-- CR 614.1: which permanents a replacement's pattern admits (and, since P8,
-- which permanents a Sacrifice cost component admits too). Hardened Scales
-- scopes to creatures; Doubling Season's counter clause to any permanent;
-- Fireblast's cost to Mountains.
--
-- The sibling of Pawl.Type.CardCriterion, deliberately NOT merged with it: P9
-- merges both into one filter language, and merging them here would be building
-- half of P9 with one customer.
data PermanentCriterion
  = AnyPermanent
  | CreaturePermanent
  | -- CR 205.3: a permanent with this subtype (Fireblast's Mountains). Matched
    -- through the projection, so a type-changing effect is seen.
    PermanentOfSubtype Subtype
  deriving (Eq, Ord, Show)
