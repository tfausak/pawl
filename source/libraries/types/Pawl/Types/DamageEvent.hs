module Pawl.Types.DamageEvent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient

-- | One instance of damage: a source dealt `amount` to `target`. The first reader
-- is deathtouch's CR 704.5h SBA, which asks `dealtByDeathtouch`.
--
-- Every field past the first three is a deal-time RIDER -- a fact about the
-- source, captured as the damage is dealt because it may be unaskable later.
-- Pawl.Engine.Damage.damageEvent is where they are read.
data DamageEvent = MkDamageEvent
  { source :: ObjectId.ObjectId,
    target :: Recipient.Recipient,
    amount :: Natural.Natural,
    -- | CR 702.2e: whether the source had deathtouch WHEN THIS DAMAGE WAS DEALT.
    -- Captured at deal time rather than re-derived at SBA-check time, so a source
    -- that has since changed or ceased cannot alter it. The same read supplies
    -- CR 702.2e's own last-known-information clause, for a source already gone as
    -- the damage is dealt. Read by the CR 704.5h SBA.
    dealtByDeathtouch :: Bool,
    -- | CR 702.90d: whether the source had infect WHEN THIS DAMAGE WAS DEALT
    -- (last known information), captured exactly as dealtByDeathtouch is.
    dealtByInfect :: Bool,
    -- | CR 702.80b: whether the source had wither WHEN THIS DAMAGE WAS DEALT
    -- (last known information), captured exactly as dealtByInfect is. Read only
    -- for damage to a CREATURE -- CR 120.3d pairs wither with infect there, and
    -- CR 120.3a's life-loss exception names infect alone, so a wither source's
    -- damage to a player is ordinary life loss.
    dealtByWither :: Bool,
    -- | The source's TOTAL TOXIC VALUE (CR 702.164b: the sum of its toxic
    -- abilities' Ns) when this damage was dealt. Captured the way the two bits
    -- above are, but NOT for their reason: rule 702.164 has no last-known-
    -- information clause of its own, so this one is uniformity, not citation.
    -- Zero for a source without toxic. Read only for COMBAT damage dealt to a
    -- player -- CR 120.3g scopes toxic to that alone, so a noncombat event
    -- carries the value and ignores it.
    dealtByToxic :: Natural.Natural,
    -- | CR 702.15b: WHO this damage's lifelink pays -- the source's controller,
    -- or its owner if it has none -- and Nothing when the source had no lifelink
    -- at all.
    --
    -- The rule's WHOLE answer, resolved at deal time, rather than a bare bit plus
    -- a controller lookup when the life is handed over. Both halves are facts
    -- about the source AS THE DAMAGE WAS DEALT: CR 702.15c makes the first last-
    -- known information outright, and re-asking the second later would read a
    -- board the CR 616.1 replacement loop has already moved. CR 702.15f's
    -- redundancy clause is why there is no count here.
    --
    -- Read for EVERY damage event and not just combat ones (CR 702.15d).
    dealtByLifelink :: Maybe PlayerId.PlayerId,
    -- | CR 510 vs CR 608: combat damage or not. Set at deal time -- Damage tags
    -- Combat, Resolve's DealDamage tags Noncombat. Read by Replacement.applies's
    -- DamageR arm (CR 615.1's damage pattern).
    kind :: DamageKind.DamageKind
  }
  deriving (Eq, Ord, Show)
