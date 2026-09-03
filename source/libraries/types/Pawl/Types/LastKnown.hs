module Pawl.Types.LastKnown where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Source as Source

-- | CR 608.2h: what an object WAS, filed under the id it had while it existed.
-- Written by the one zone-change funnel (Pawl.Engine.Event.changeZoneAttaching)
-- as the object ceases, from the same pre-move state the GameEvent.Moved
-- snapshot is taken against.
--
-- Ten things rather than the characteristics alone, because the other nine
-- questions CR 608.2h is asked have no home in that fold. Control is not a
-- characteristic (CR 109.3), yet "who controlled it" is what CR 603.3a
-- asks of a triggered ability whose source is gone. Neither is the object's
-- SOURCE: the projection folds characteristics, and CR 603.7's delayed-ability
-- declarations are read straight from the card, so an ArmDelayedTrigger whose
-- source has just exiled itself has nowhere else to find the ability it names.
-- Nor are COUNTERS -- CR 109.3's list has none -- and unlike the other two the
-- projection actively CONSUMES them (CR 613.4c), so the record has to be taken
-- beside it rather than out of it. The COPIABLE values are the fourth, for the
-- reason its own field gives. Nor is the ATTACHMENT, the fifth -- CR 109.3
-- names "what an Aura enchants" as an example of what is not one. Nor are the
-- CHOSEN NAMES, the sixth, for the reason its own field gives. Nor is the
-- OWNER, the seventh -- CR 109.3's list has no owner either -- for the reason
-- its own field gives. Nor is the COMBAT STATUS, the eighth, for the reason its
-- own field gives. Nor is the PROTECTOR, the ninth, for the reason its own field
-- gives.
--
-- All ten fields STRICT (!): entries are keyed by an id that no longer exists
-- and are never pruned, so an unforced field would be a thunk retaining the whole
-- pre-move GameState for the rest of the game.
data LastKnown = MkLastKnown
  { characteristics :: !ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 110.2 / 613.1b: the PROJECTED controller as the object left, which is
    -- not its owner -- layer 2 can have moved it. A permanent stolen by Control
    -- Magic was controlled by the thief right up to the moment it died, and
    -- CR 603.3a hands that player its trigger.
    controller :: !PlayerId.PlayerId,
    -- | CR 108.3: who OWNED it, which CR 110.2 never moves -- so unlike
    -- `controller` this is the same player the live object carried, filed because
    -- the object it was read off no longer exists.
    --
    -- CR 400.3's reader is what wants it: "your graveyard" names the copy a
    -- card's owner has, so an intervening "if" asking where a permanent came from
    -- (Pawl.Engine.Quantity's EnteredFrom and WasCastFrom) needs the owner of an
    -- entrant CR 603.4 re-checks after it has died. That it is ANSWERABLE is
    -- proved by Pawl.ConditionSpec's "the entrant killed between the two checks
    -- still grows the Knight"; WHICH player it names is a regression fence, the
    -- Knight's clause reading "a graveyard" over every player and so never asking.
    -- Pawl.Engine.Projection.viewWithLastKnownAnywhere is the one reader, since CR
    -- 608.2b still wants a blank answer for a gone TARGET.
    owner :: !PlayerId.PlayerId,
    -- | CR 608.2h: what KIND of object it was and the card behind it -- the same
    -- Object.source the live object carried, copied as it ceased.
    source :: !Source.Source,
    -- | CR 122.1: the counters that were on it, per kind -- the same
    -- Object.counters the live object carried. What Promising Duskmage's "if it
    -- had a +1/+1 counter on it" asks after.
    --
    -- It cannot be recovered from `characteristics` and is not redundant with it:
    -- CR 613.4c applies a +1/+1 counter at layer 7c, so the projection above
    -- records 3/4 where the printed box said 2/3 and says nothing at all about
    -- what produced the difference. A +1/+1 counter and a still-live pump effect
    -- land in the same sublayer and leave the same power behind; only this field
    -- tells them apart, and Promising Duskmage's clause is about exactly that
    -- difference.
    --
    -- Counters are not characteristics -- CR 109.3's list has none -- so this
    -- sits beside the projection for the reason `controller` does rather than
    -- inside it.
    counters :: !(Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural),
    -- | CR 707.2: the COPIABLE values as the object left -- layer 1 alone, where
    -- `characteristics` above is the whole fold. What CR 608.2h hands a copy
    -- effect that names a permanent already gone (Watchful Radstag's "create a
    -- token that's a copy of it", the Radstag killed in response to its own
    -- trigger).
    --
    -- Beside the projection rather than derived from it, for `counters`' reason
    -- turned around: the fold is lossy in the other direction too, and a +1/+1
    -- counter or a live pump is exactly what CR 707.2 does not copy. Nor is it
    -- recoverable from `source`, which knows the card but not which face was up
    -- and not whether the object was ITSELF a copy (CR 707.2's "as modified by
    -- other copy effects").
    copiable :: !ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 303.4b / CR 301.5a: what it was attached to as it left -- the same
    -- Object.attachedTo the live object carried. What "enchanted creature" and
    -- "equipped creature" mean for an Aura or an Equipment that is itself
    -- already gone.
    --
    -- Not a characteristic either (CR 109.3's list has no attachment), and not
    -- recoverable from anything above, so it sits beside the projection for
    -- `controller`'s reason. CR 704.5m takes an AURA off the battlefield in the
    -- very SBA batch that killed its host, so a live read of the link is
    -- unavailable exactly when the rules still ask for it --
    -- Pawl.Types.TriggerCondition.AttachedCreatureDies is the reader.
    --
    -- Not implemented: the same question for an EQUIPMENT, which CR 704.5n
    -- merely detaches, so it survives the batch with no entry here and no live
    -- link either (#3144).
    --
    -- Nothing for an object that was attached to nothing, which is every object
    -- that is not an Aura, an Equipment or a Fortification.
    attachedTo :: !(Maybe Recipient.Recipient),
    -- | CR 201.4: the card names that had been chosen for it -- the same
    -- Object.chosenNames the live object carried. What CR 608.2h answers for a
    -- CR 611.2c effect whose source chose a name and then left the zone it was
    -- expected to be in: Conjurer's Ban chooses during its own resolution and is
    -- in a graveyard by the time the prohibition it stored is first read
    -- (Pawl.Engine.PlayerEffect.chosenNamesOf).
    --
    -- Not a characteristic either -- CR 109.3's list has no chosen name -- and
    -- not recoverable from `characteristics` or `copiable`, since choosing a name
    -- changes nothing the projection folds. So it sits beside them for
    -- `controller`'s reason.
    chosenNames :: !(Set.Set CardName.CardName),
    -- | CR 509.1g: was it blocking as it left -- the same membership
    -- Pawl.Engine.Filter.View's `blocking` reports, read off GameState.combat
    -- before the object ceased.
    --
    -- Not a characteristic either (CR 109.3's list has no combat status), and not
    -- recoverable from anything above: CR 506.4 takes a departed creature out of
    -- combat, so the live read is unavailable exactly when CR 603.4's intervening
    -- "if" on a dies-trigger still asks for it (Guildsworn Prowler).
    --
    -- A Bool rather than the attackers this creature was blocking, because that
    -- is the whole of what the view reports and all any clause in the pool asks.
    blocking :: !Bool,
    -- | CR 310.9a: which player protected it as it left -- the same
    -- Object.protector the live object carried, and Nothing for everything that
    -- is not a battle.
    --
    -- Not a characteristic either (CR 109.3's list has no protector), and not
    -- recoverable from anything above: CR 506.4 removes a departed battle from
    -- combat while CR 506.4c keeps its attacker attacking it, and CR 508.5's
    -- second sentence then asks for "the protector of the battle that creature
    -- was attacking before it was removed from combat" -- a question with no live
    -- object left to read. Pawl.Engine.Battle.lastKnownProtectorOf is the reader,
    -- through Pawl.Engine.Defender.playerOf's battle arm.
    protector :: !(Maybe PlayerId.PlayerId)
  }
  deriving (Eq, Ord, Show)
