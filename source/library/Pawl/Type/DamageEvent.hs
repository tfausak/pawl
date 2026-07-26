module Pawl.Type.DamageEvent where

import Numeric.Natural (Natural)
import Pawl.Type.DamageKind (DamageKind)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Recipient (Recipient)

-- One instance of damage: a source dealt `amount` to `target`. The first reader
-- is deathtouch's CR 704.5h SBA, which asks `dealtByDeathtouch`. Started minimal
-- (CR 702.2 needs only source + creature target + nonzero amount) on the bet
-- that it would GROW rather than be reshaped, and it has: a kind tag, then
-- infect (P10), then toxic. The M2c spec's §2 named lifelink and M4's
-- combat-damage triggers as the growth vectors; the actual ones have all been
-- deal-time RIDERS -- a fact about the source, captured as the damage is dealt
-- because it may be unaskable later. Pawl.Damage.damageEvent is where they are
-- read.
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
    -- The source's TOTAL TOXIC VALUE (CR 702.164b: the sum of its toxic
    -- abilities' Ns) when this damage was dealt. Captured the way the two bits
    -- above are, but NOT for their reason: rule 702.164 has no last-known-
    -- information clause of its own, so this one is uniformity, not citation --
    -- see Pawl.Damage.damageEvent. Zero for a source without toxic. Read by
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
