module Pawl.Types.Uses where

-- | CR 614.3: floating replacement and prevention effects "last until they're
-- used up or their duration has expired". Regeneration is CR 701.19a's "the
-- NEXT time this permanent would be destroyed this turn" (Once); Fog watches
-- every combat damage event for its whole duration (Unlimited).
--
-- A sum, not a Bool and not a counter: "used up" is a rules concept, and CR
-- 615's prevent-the-next-N shape (which would be a counted arm here) has no
-- producer in the pool.
data Uses
  = Unlimited
  | Once
  deriving (Eq, Ord, Show)
