module Pawl.Type.DamageKind where

-- Whether a damage event is combat damage (CR 510) or damage from a resolving
-- spell or ability (CR 608). Read by Event.applyPreventions (Fog prevents only
-- combat damage, CR 615) and, later, by combat-damage triggers and lifelink. A
-- Bool would blind the reader to which it is (no-boolean-blindness).
data DamageKind = Combat | Noncombat
  deriving (Eq, Ord, Show)
