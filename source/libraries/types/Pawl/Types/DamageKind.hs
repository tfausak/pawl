module Pawl.Types.DamageKind where

-- | Whether a damage event is combat damage (CR 510) or damage from a resolving
-- spell or ability (CR 608). Read by Replacement.applies's DamageR arm (Fog
-- prevents only combat damage, CR 615) and, later, by combat-damage triggers
-- and lifelink. A Bool would blind the reader to which it is
-- (no-boolean-blindness).
data DamageKind = Combat | Noncombat
  deriving (Eq, Ord, Show)
