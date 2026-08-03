module Pawl.Types.Uses where

-- | CR 614.3: floating replacement and prevention effects "last until they're
-- used up or their duration has expired". Regeneration is CR 701.19a's "the
-- NEXT time this permanent would be destroyed this turn" (Once); Fog watches
-- every combat damage event for its whole duration (Unlimited).
--
-- A sum, not a Bool and not a counter: "used up" is a rules concept, and the
-- one counted thing in CR 615 is not counted in THIS unit. CR 615.7's
-- prevent-the-next-N shield (Mending Hands) rides Unlimited and carries its own
-- remaining amount on the rewrite (Pawl.Types.DamageRewrite.PreventNext),
-- because 615.7's last sentence -- "such effects count only the amount of
-- damage; the number of events or sources dealing it doesn't matter" -- makes
-- its unit DAMAGE while this type's is APPLICATIONS.
data Uses
  = Unlimited
  | Once
  deriving (Eq, Ord, Show)
