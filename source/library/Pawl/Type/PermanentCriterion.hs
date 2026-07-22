module Pawl.Type.PermanentCriterion where

-- CR 614.1: which permanents a replacement's pattern admits. Hardened Scales
-- scopes to creatures; Doubling Season's counter clause to any permanent.
--
-- The sibling of Pawl.Type.CardCriterion, deliberately NOT merged with it: P9
-- merges both into one filter language, and merging them here would be building
-- half of P9 with one customer.
data PermanentCriterion
  = AnyPermanent
  | CreaturePermanent
  deriving (Eq, Ord, Show)
