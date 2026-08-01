module Pawl.Types.DamageEvent where

import Numeric.Natural (Natural)
import Pawl.Types.DamageKind (DamageKind)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.Recipient (Recipient)

-- One instance of damage: a source dealt `amount` to `target`. The first reader
-- is deathtouch's CR 704.5h SBA, which asks `dealtByDeathtouch`. Started minimal
-- (CR 702.2 needs only source + creature target + nonzero amount) on the bet
-- that it would GROW rather than be reshaped, and it has: a kind tag, then
-- infect (P10), then toxic, then lifelink. The M2c spec's §2 named lifelink and
-- M4's combat-damage triggers as the growth vectors; every one that has actually
-- arrived is a deal-time RIDER -- a fact about the source, captured as the
-- damage is dealt because it may be unaskable later. Pawl.Engine.Damage.damageEvent is
-- where they are read.
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
    -- see Pawl.Engine.Damage.damageEvent. Zero for a source without toxic. Read by
    -- Pawl.Engine.Damage.applyDamage, and only for COMBAT damage dealt to a player --
    -- CR 120.3g scopes toxic to that alone, so a noncombat event carries the
    -- value and ignores it.
    dealtByToxic :: Natural,
    -- CR 702.15b: WHO this damage's lifelink pays -- "that source's controller,
    -- or its owner if it has no controller" -- and Nothing when the source had
    -- no lifelink at all.
    --
    -- The rule's WHOLE answer, resolved at deal time, rather than a bare bit
    -- plus a controller lookup when the life is handed over. Both halves are
    -- facts about the source AS THE DAMAGE WAS DEALT: CR 702.15c says the first
    -- is last-known information outright ("its last known information is used
    -- to determine whether it had lifelink"), and re-asking the second later
    -- would read a board that the CR 616.1 replacement loop has already moved.
    -- Sized for the answer the way dealtByToxic is (a value, not a bit, because
    -- CR 702.164b's answer is a number); CR 702.15f's redundancy clause is why
    -- there is no count here.
    --
    -- Read by Pawl.Engine.Damage.applyDamage, for EVERY damage event and not just
    -- combat ones -- CR 702.15d: "the lifelink rules function no matter what
    -- zone an object with lifelink deals damage from."
    dealtByLifelink :: Maybe PlayerId,
    -- CR 510 vs CR 608: combat damage or not. Set at deal time -- Damage tags
    -- Combat, Resolve's DealDamage tags Noncombat. Read by Replacement.applies's
    -- DamageR arm (CR 615.1's damage pattern).
    kind :: DamageKind
  }
  deriving (Eq, Ord, Show)
