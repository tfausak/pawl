module Pawl.Types.DealDamage where

import qualified Pawl.Types.ExcessDestination as ExcessDestination
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | CR 120.1: deal this much damage to the objects or players the ObjectRef names.
data DealDamage = MkDealDamage
  { ref :: ObjectRef.ObjectRef,
    -- | HOW MUCH, read once per recipient rather than once for the set: Acidic
    -- Soil's "each player equal to the number of lands they control" is a
    -- different number per seat. Still one CR 608.2f batch -- see
    -- Pawl.Engine.Resolve's arm, which reads every recipient's amount off the
    -- same pre-effect state.
    quantity :: Quantity.Quantity,
    -- | WHICH OBJECT DEALS IT -- CR 120.1's "an object that deals damage is the
    -- source of that damage", which CR 120.2b lets a spell or ability name for
    -- itself: "the spell or ability will specify which object deals that
    -- damage."
    --
    -- Nothing is CR 113.7's default and what every other damage-dealing card in
    -- the pool wants: Lightning Bolt's damage comes from Lightning Bolt, and the
    -- resolving object's source is the source. Just names a slot instead --
    -- Rabid Bite's "target creature you control deals damage equal to its power
    -- to target creature you don't control", where the DEALER is a targeted
    -- permanent and the sorcery is only the instruction.
    --
    -- The difference is observable and not bookkeeping: every rule that asks
    -- about a damage source asks about the dealer. CR 120.3f pays the dealer's
    -- lifelink to the DEALER's controller, CR 702.2b destroys on the dealer's
    -- deathtouch, and CR 120.3b/120.3d/120.3g read the dealer's infect, wither
    -- and toxic. Pawl.Engine.Damage.damageEvent reads all of them off the one
    -- ObjectId Pawl.Engine.Resolve hands it, so naming a different object here
    -- redirects every one of them at once.
    --
    -- A SlotName rather than an ObjectRef, where `ref` above is the wider type:
    -- a damage event has exactly ONE source (CR 120.1), and every dealer in the
    -- pool is a single object named before the effect runs -- CR 115.10a's
    -- distinction, which is exactly the InSlot arm's. A swept SET of dealers
    -- would be a different sentence and no card in the pool writes one.
    --
    -- An empty slot deals nothing. The dealer of a printed card is a TARGET, so
    -- one that has become illegal (CR 608.2b) leaves the instruction with no
    -- source and it does as much as it can, which is nothing -- not damage from
    -- the resolving object instead.
    dealer :: Maybe SlotName.SlotName,
    -- | WHERE THE EXCESS GOES -- CR 120.4a's first sentence, which happens only
    -- when "an effect that's causing damage to be dealt STATES that excess
    -- damage that would be dealt to a permanent is dealt to another permanent or
    -- player instead". The effect states it, so the instruction carries it:
    -- Flame Spill's "Excess damage is dealt to that creature's controller
    -- instead".
    --
    -- Nothing is every other damage-dealing card: the damage event is not
    -- rewritten at all and the whole amount lands on the recipient, however much
    -- of it CR 120.6 makes redundant. Lightning Bolt at a 2/1 marks three.
    --
    -- Here rather than on Pawl.Types.DamageEvent, where the amount and the
    -- recipient live, because CR 120.4a's rewrite is keyed to the EFFECT and
    -- combat damage can never state it (CR 510.1's assignment is not an effect).
    -- Pawl.Engine.Damage.redirectExcess does the rewriting, on the events
    -- Pawl.Engine.Resolve's DealDamage arm has built and before
    -- Pawl.Engine.Damage.applyDamage sees them -- CR 120.4a strictly before CR
    -- 120.4b, which is observable: a prevention effect on the redirected player
    -- applies to the redirected half.
    excess :: Maybe ExcessDestination.ExcessDestination
  }
  deriving (Eq, Ord, Show)
