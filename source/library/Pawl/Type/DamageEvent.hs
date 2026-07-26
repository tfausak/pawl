module Pawl.Type.DamageEvent where

import Numeric.Natural (Natural)
import Pawl.Type.DamageKind (DamageKind)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Recipient (Recipient)

-- One instance of combat damage: a source dealt `amount` to `target`. The first
-- reader is deathtouch's CR 704.5h SBA, which asks `dealtByDeathtouch`. Minimal
-- by design (CR 702.2 needs only source + creature target + nonzero amount);
-- lifelink and M4 combat-damage triggers grow the payload rather than reshape
-- it. See the M2c spec, section 2.
data DamageEvent = MkDamageEvent
  { source :: ObjectId,
    target :: Recipient,
    amount :: Natural,
    -- CR 702.2e: whether the source had deathtouch WHEN THIS DAMAGE WAS DEALT.
    -- Captured from the projection at deal time (Projection.hasKeyword), not
    -- re-derived at SBA-check time -- last-known information. Read by the CR
    -- 704.5h SBA. See the M3b spec, section 4.
    dealtByDeathtouch :: Bool,
    -- CR 702.90d: whether the source had infect WHEN THIS DAMAGE WAS DEALT
    -- (last known information), captured exactly as dealtByDeathtouch is.
    dealtByInfect :: Bool,
    -- CR 702.164b: the source's TOTAL TOXIC VALUE when this damage was dealt,
    -- captured exactly as the two bits above are. Zero for a source without
    -- toxic, which is every source but Branchblight Stalker. Read by
    -- Pawl.Damage.applyDamage, and only for COMBAT damage dealt to a player --
    -- CR 120.3g scopes toxic to that alone, so a noncombat event carries the
    -- value and ignores it.
    dealtByToxic :: Natural,
    -- CR 510 vs CR 608: combat damage or not. Set at deal time -- Damage tags
    -- Combat, Resolve's DealDamage tags Noncombat. Read by Replacement.applies's
    -- DamageR arm (CR 615.1's damage pattern).
    kind :: DamageKind
  }
  deriving (Eq, Ord, Show)
