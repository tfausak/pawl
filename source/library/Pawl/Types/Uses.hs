module Pawl.Types.Uses where

-- | CR 614.3: floating replacement and prevention effects "last until they're
-- used up or their duration has expired". Regeneration is CR 701.19a's "the
-- NEXT time this permanent would be destroyed this turn" (Once); Fog watches
-- every combat damage event for its whole duration (Unlimited).
--
-- A sum, not a Bool and not a counter: CR 615.7's prevent-the-next-N shield
-- (Mending Hands) rides Unlimited and carries its remaining amount on the rewrite
-- (Pawl.Types.DamageRewrite.PreventNext), because CR 615.7 counts DAMAGE while
-- this type counts APPLICATIONS. An unbounded shield (Selfless Squire) rides
-- Unlimited too, and for the simpler reason that CR 615.3 leaves it nothing but a
-- duration to end it.
data Uses
  = Unlimited
  | Once
  deriving (Bounded, Enum, Eq, Ord, Show)
