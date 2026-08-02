module Pawl.Types.DamageKind where

-- Whether a damage event is combat damage (CR 510) or damage from a resolving
-- spell or ability (CR 608). Read by Replacement.applies's DamageR arm (Fog
-- prevents only combat damage, CR 615), by CR 120.3g's toxic arm in
-- Pawl.Engine.Damage.applyDamage, and, later, by combat-damage triggers. A Bool would
-- blind the reader to which it is (no-boolean-blindness).
--
-- Lifelink is deliberately NOT among those readers, though this comment used to
-- anticipate that it would be: CR 120.3f states the life gain with no
-- combat/noncombat qualifier at all, and CR 702.15d makes the point explicitly
-- ("the lifelink rules function no matter what zone an object with lifelink
-- deals damage from"). Toxic is the one that IS scoped -- CR 120.3g says
-- "combat damage dealt to a player" -- which is the whole contrast.
data DamageKind = Combat | Noncombat
  deriving (Eq, Ord, Show)
