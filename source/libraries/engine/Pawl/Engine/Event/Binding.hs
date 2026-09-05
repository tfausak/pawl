-- What a trigger condition binds off the event it matched (CR 603.2, CR
-- 603.10): the slot map a matched trigger carries onto the stack, read from the
-- event alone with no game state. Split out of Pawl.Engine.Event for size; it
-- sits below Pawl.Engine.Event.Match and Pawl.Engine.Event.Trigger.
module Pawl.Engine.Event.Binding where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BecameAttacked as BecameAttacked
import qualified Pawl.Types.BecameBlocking as BecameBlocking
import qualified Pawl.Types.BecameTarget as BecameTarget
import Pawl.Types.Binding (Binding)
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.Discarded as Discarded
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Mentored as Mentored
import qualified Pawl.Types.Moved as Moved
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PermanentWasSacrificed as PermanentWasSacrificed
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.StepBegan as StepBegan
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.ZoneChange as ZoneChange

-- CR 603.2: the bindings the EVENT contributes to a trigger it has just fired --
-- the environment in which the ability's "that player" / "that creature" is read.
-- Called only for a pair `matchesTrigger` already accepted, so an arm may assume
-- its condition's shape matched; a mismatched pair contributes nothing.
--
-- Separate from `matchesTrigger` rather than folded into a `Maybe bindings`
-- return, the two having different customers: a DELAYED ability matches several
-- events at once and carries the environment captured when it was armed (CR
-- 603.7c). The parallel for a sourceless inherent ability is
-- Monarch.inherentMatch, which has no bearer to scope a shared matcher to.
--
-- THE FIRST ARGUMENT IS NOT READ OFF THE EVENT, and is the one datum here that
-- is not: CR 400.7f's "the new object that each Aura enchanting that permanent
-- became in its owner's graveyard" is a fact about the BEARER's own zone change,
-- which the event that fired the trigger says nothing about. `eventTriggers`
-- computes it off the same CR 117.5 batch and hands it in; `delayedPending` has
-- no bearer departure to scan for and hands in Nothing. Kept here rather than
-- unioned in at the call site so that eventBindingSlots below stays the single
-- statement of which slots a condition makes available, which is what the card
-- lint reads.
--
-- THE THIRD ARGUMENT is CR 109.5 / 603.3a's "you", the ability's controller --
-- matchesTriggerGiven's own third argument, and read the same way at both call
-- sites (eventTriggers' `ctrl`, delayedPending's `DelayedTrigger.controller`).
-- Most arms never read it; DamageToPlayerPrevented below does, to re-ask the
-- condition's relation of a record a wider shield stamped for more than one
-- player (#3079).
eventBindings :: GameState -> Maybe ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Map.Map SlotName.SlotName Binding
eventBindings gs bearerBecame you cond event = case (cond, event) of
  -- CR 603.2b's "that player": the active player, on whose turn the step began.
  -- Shizuko, Caller of Autumn's "at the beginning of each player's upkeep, THAT
  -- PLAYER adds {G}{G}{G}" is the reader, and the seat it names is nobody the
  -- ability already has -- CR 109.5's "you" is Shizuko's controller, and CR
  -- 603.2b's step is each player's in turn.
  --
  -- Bound for EVERY TurnScope, not only EachTurn. Under ControllersTurn the
  -- active player IS the controller, which makes the slot a redundant second name
  -- rather than a wrong one -- the posture the PlayerBecomesMonarch arm below
  -- takes for its own You case, and what eventBindingSlots' unconditional promise
  -- for this condition needs.
  --
  -- Unconditional given a match: every GameEvent.StepBegan carries a PlayerId,
  -- CR 500.1 giving every step exactly one turn to belong to.
  (TriggerCondition.StepBegins {}, GameEvent.StepBegan ev) ->
    Binding.setTriggerPlayer (StepBegan.player ev) Map.empty
  -- CR 702.70a's "that player": the player the bearer dealt combat damage to.
  --
  -- HOW MUCH, alongside it: Questing Beast's "it deals THAT MUCH damage to target
  -- planeswalker that player controls" counts the damage the event carried, under
  -- the same reserved slot CR 615.13's prevention and CR 119.9's life gain stamp
  -- (see Binding.eventAmount) and the same one the bystander arm below stamps.
  --
  -- Both stamped on the ToPlayer branch alone. The other four recipients are
  -- events this condition does not admit -- matchesTrigger requires
  -- isPlayerRecipient -- so claiming a slot there would name a match that never
  -- happened. Given a match both are unconditional, which is what
  -- eventBindingSlots' per-condition promise needs: every GameEvent.DamageDealt
  -- carries a DamageEvent.amount.
  (TriggerCondition.SelfDealsCombatDamageToPlayer, GameEvent.DamageDealt ev) ->
    case DamageEvent.target ev of
      Recipient.ToPlayer pid -> Binding.setTriggerPlayer pid (Binding.setEventAmount (DamageEvent.amount ev) Map.empty)
      Recipient.ToCreature _ -> Map.empty
      Recipient.ToPlaneswalker _ -> Map.empty
      Recipient.ToBattle _ -> Map.empty
      Recipient.ToObject _ -> Map.empty
      -- Unreachable, for the reason the DamageToPlayerPrevented arm above gives.
      Recipient.ToPile _ -> Map.empty
  -- CR 603.2's "that much": how many counters actually came off, read off the
  -- event's own before/after pair. Chandra, Fire Artisan's "she deals that much
  -- damage" counts THAT and not the damage that caused it -- CR 306.8's removal
  -- saturates, so five damage to a four-loyalty planeswalker removes four -- and
  -- one CR 510.2 batch is one record, so two attackers taking two counters off
  -- between them stamp 2 once rather than 1 twice.
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.CountersRemoved carries both counts, and the
  -- record exists only where `before` exceeds `after`.
  --
  -- Saturating, and the guard is nominal for that reason; a Natural difference
  -- has no other honest floor.
  (TriggerCondition.SelfCountersRemoved _, GameEvent.CountersRemoved change) ->
    Binding.setEventAmount (Natural.minusSaturating (CounterChange.before change) (CounterChange.after change)) Map.empty
  -- CR 510.2's "it": the permanent that dealt the combat damage, which Aragorn,
  -- Hornburg Hero's payload doubles the counters on. Read off the event's source,
  -- the same field matchesTrigger applied the Filter to, so the slot names exactly
  -- the permanent the condition admitted.
  --
  -- HOW MUCH, alongside it: Shroofus Sproutsire's "create that many 1/1 green
  -- Saproling creature tokens" counts the damage the event carried, under the same
  -- reserved slot CR 615.13's prevention and CR 119.9's life gain stamp (see
  -- Binding.eventAmount). The AMOUNT the event recorded and never the damager's
  -- power: CR 702.19b lets a trampler assign part of its power to a blocker, so
  -- the two come apart on exactly the board Pawl.TriggerSpec's shroofusSpec runs.
  --
  -- The DAMAGED PLAYER beside them, under the same `triggerPlayer` slot the
  -- self-scoped arm above stamps: Larceny's "whenever a creature you control deals
  -- combat damage to a player, THAT PLAYER discards a card" names a seat that is
  -- neither the bearer's controller nor the damager's.
  --
  -- All three unconditional given a match, which is what eventBindingSlots'
  -- per-condition promise needs: every GameEvent.DamageDealt carries a
  -- DamageEvent.source and a DamageEvent.amount, and matchesTrigger has already
  -- required isPlayerRecipient of the target -- so Recipient.playerOf's Nothing is
  -- unreachable for an event this condition admitted.
  (TriggerCondition.PermanentDealsCombatDamageToPlayer _, GameEvent.DamageDealt ev) ->
    maybe id Binding.setTriggerPlayer (Recipient.playerOf (DamageEvent.target ev)) (Binding.setCombatDamager (DamageEvent.source ev) (Binding.setEventAmount (DamageEvent.amount ev) Map.empty))
  -- CR 400.7e: a zone-change trigger can find the new object the card became in
  -- the zone it moved to, if that zone is public. CR 603.6c and CR 603.6e say it
  -- from the other side.
  --
  -- ZoneChange.object, NOT `departed`, which is the whole point of this arm:
  -- `departed` is what matchesTrigger matched the bearer against (CR 603.10a's
  -- look-back) and names an id CR 400.7 has deleted, so an effect handed it would
  -- move nothing. `object` is the card in the graveyard.
  --
  -- Bound ALONGSIDE the source, not instead of it: Engine.placeBorne stamps
  -- Binding.triggerSource over these and must keep stamping the departed id, that
  -- slot being CR 113.7a's source. One printed "it", two objects.
  --
  -- CR 400.7e's public-zone proviso holds by construction here, matchesTrigger's
  -- SelfDies arm having required `to == Graveyard`; the arm below is where it
  -- becomes a real test.
  (TriggerCondition.SelfDies, GameEvent.Moved m) ->
    setBecameArrivals m Map.empty
  -- The same rule and the same field, watched by a BYSTANDER: Promise of
  -- Tomorrow's "whenever a creature you control dies, exile IT". What differs
  -- from the arm above is only which object CR 113.7a's source is -- there the
  -- bearer and the deceased are two incarnations of one card, here the bearer is
  -- a third object entirely (an enchantment) and the source slot cannot reach
  -- the dead creature at all.
  --
  -- ZoneChange.object, NOT `departed`, and here the two really are two different
  -- cards' worth of trap: matchesTrigger's PermanentDies arm matches on
  -- `departed`, because CR 603.10a's look-back is what makes "you control"
  -- answerable off CR 608.2h last known information -- but CR 400.7 deleted that
  -- id, so a payload handed it would move nothing. `object` is the graveyard
  -- card the payload has to act on.
  --
  -- CR 400.7e's public-zone proviso holds BY CONSTRUCTION, needing no guard of
  -- the kind SelfLeavesTheBattlefield below carries: matchesTrigger's
  -- PermanentDies arm has already required the battlefield-to-graveyard pair, and
  -- CR 400.2 lists the graveyard among the public zones. That is what makes
  -- eventBindingSlots' unconditional promise for this condition honest.
  --
  -- CR 603.10a's departed permanent BESIDE it, the PermanentLeavesTheBattlefield
  -- arm below's second slot for that arm's reason: one printed "it" is two
  -- objects, and which one a card means is the card's business. Cleopatra,
  -- Exiled Pharaoh's "draw a card for each counter on it" is about the creature
  -- as it last existed on the battlefield, whose counters ceased to exist on the
  -- way out (CR 122.2: they are not "removed") and which only CR 608.2h still
  -- answers for; Promise of Tomorrow's "exile it" is about the graveyard card.
  -- Unconditional, needing no guard at all: `departed` is on every zone change,
  -- and this condition admits no other event.
  (TriggerCondition.PermanentDies _, GameEvent.Moved m) ->
    Binding.setDepartedPermanent (ZoneChange.departed (Moved.change m)) (setBecameArrivals m Map.empty)
  -- CR 400.7e off the CARD rather than off the move: Planar Void's "exile that
  -- card" acts on the one arrival its trigger matched, so this binds
  -- ZoneChange.object alone where the two arms above bind every arrival as a
  -- group. That is what makes a melded permanent's two triggers exile a card
  -- each instead of both exiling both.
  --
  -- CR 400.7e's public-zone proviso holds by construction, matchesTrigger's arm
  -- having required the graveyard.
  (TriggerCondition.CardPutIntoGraveyard _, GameEvent.Moved m) ->
    Binding.setBecame (ZoneChange.object (Moved.change m)) Map.empty
  (TriggerCondition.CardPutIntoGraveyard _, GameEvent.CardArrived zc) ->
    Binding.setBecame (ZoneChange.object zc) Map.empty
  -- The same rule, with its proviso doing real work for the first time: CR 603.6c's
  -- wider condition accepts ANY destination, and CR 400.2 makes two of them hidden.
  --
  -- The binding is ABSENT for a hidden destination rather than present-but-useless:
  -- ZoneChange.object names a real card in that hand, and stamping it would hand
  -- the ability an object the rule forbids it to find. Absence is what CardSpec's
  -- slot lint reads and what Resolve's arms treat as "nothing to act on".
  --
  -- Classified by the ZONE, never by whether the card is currently visible -- CR
  -- 400.2 draws exactly that distinction.
  (TriggerCondition.SelfLeavesTheBattlefield, GameEvent.Moved m)
    | not (Game.isHiddenZone (ZoneChange.to (Moved.change m))) ->
        setBecameArrivals m Map.empty
  -- The bystander reading of that same arm. CR 400.7e's arrival is guarded the
  -- same way and for the same rule -- and unlike the self-scoped arm this one
  -- binds a second slot the guard does not reach.
  (TriggerCondition.PermanentLeavesTheBattlefield _, GameEvent.Moved m) ->
    -- CR 603.10a's look-back, bound UNCONDITIONALLY where CR 400.7e's arrival is
    -- bound only for a public destination: Resourceful Defense's "if IT had
    -- counters on it" is about the permanent as it last existed on the
    -- battlefield, which ZoneChange.departed names whatever zone the card
    -- reached. Both slots, since one printed "it" is two objects for the reader
    -- that wants the arrival instead.
    Binding.setDepartedPermanent (ZoneChange.departed (Moved.change m)) $
      if Game.isHiddenZone (ZoneChange.to (Moved.change m))
        then Map.empty
        else setBecameArrivals m Map.empty
  -- CR 603.6c's other trigger event reaches no zone, so it offers CR 400.7e
  -- nothing to bind -- but the departed id is the whole of what it carries, and
  -- CR 608.2h is the only way to read a permanent that left the GAME. Which is
  -- what keeps eventBindingSlots' promise for this condition honest across all
  -- three of its events.
  (TriggerCondition.PermanentLeavesTheBattlefield _, GameEvent.LeftTheGame oid) ->
    Binding.setDepartedPermanent oid Map.empty
  -- The arm above with the destination pinned to a hand, which CR 400.3 makes
  -- the OWNER's: Warped Devotion's "whenever a permanent is returned to a
  -- player's hand, THAT PLAYER discards a card" names the owner, and
  -- PlayerRef.ControllerOfBound would name the wrong seat for a stolen permanent.
  --
  -- Read off CR 608.2h's record of the DEPARTED id, not off the arrival, and CR
  -- 108.3 makes the two the same answer: the owner is the one thing a zone change
  -- cannot move. The arrival is what CR 111.7 and CR 704.5d take away -- a token
  -- that reached a hand ceases to exist, and Engine.performSettle runs that
  -- state-based action BEFORE placePendingTriggers, so by the time this runs the
  -- arriving object is already deleted and a live lookup of it would leave the
  -- slot unbound for exactly the case CR 111.7's parenthetical says must still
  -- trigger. The departed id's record survives: the zone-change funnel files it
  -- in the same write that deletes the object, and nothing prunes it. Proved by
  -- Pawl.ZoneTriggerSpec's "CR 111.7 bob's Piker TOKEN under alice's control
  -- still makes bob discard".
  --
  -- CR 400.7e's `became` is NOT bound: a hand is hidden (CR 400.2), the
  -- SelfLeavesTheBattlefield arm's guard, here settled by the condition itself.
  (TriggerCondition.PermanentReturnedToHand _, GameEvent.Moved m) ->
    let zc = Moved.change m
        owner = fmap LastKnown.owner (Projection.lastKnownOf (ZoneChange.departed zc) gs)
     in Binding.setDepartedPermanent (ZoneChange.departed zc) (maybe Map.empty (`Binding.setTriggerPlayer` Map.empty) owner)
  -- CR 400.7e again, read in the ENTRY direction: the object that moved is the
  -- entrant, and what it became is the permanent now on the battlefield --
  -- ZoneChange.object, the field the SelfDies arm reads for the same reason.
  --
  -- The SAME slot as that arm, CR 400.7e being one rule with two readings. What
  -- differs is which object CR 113.7a's source happens to be, a fact about the
  -- CONDITION rather than the slot: SelfDies matches the departing incarnation, so
  -- `triggerSource` and `became` are two incarnations of one card, while here the
  -- bearer is another permanent entirely. Two slots would have to be kept apart by
  -- every reader for a distinction no rule draws -- and Resolve, where the slot is
  -- read, never learns which condition placed the ability.
  --
  -- The public-zone proviso holds by construction here too, `to == Battlefield`
  -- having already been required.
  --
  -- Bound whatever the Filter admits, creature or not: whether the entrant can
  -- RECEIVE what the payload does is the payload's question (CR 120.1a for
  -- damage), and a binding that existed only for creatures would make the slot's
  -- presence depend on the entrant, which eventBindingSlots cannot express.
  (TriggerCondition.PermanentEnters _, GameEvent.Moved (Moved.MkMoved zc _ _)) ->
    Binding.setBecame (ZoneChange.object zc) Map.empty
  -- CR 708.7's "that creature": the permanent that was turned face up, which Pine
  -- Walker untaps. The bearer is a bystander here -- CR 113.7a's source slot names
  -- the WATCHER, and on Pine Walker's own board the two are different permanents --
  -- so the subject needs a name of its own.
  --
  -- THE SAME SLOT the zone-change arms above stamp, deliberately widened rather
  -- than a fresh one. Turning face up is NOT a zone change: CR 708.8 restores the
  -- permanent's copiable values and leaves it on the battlefield, so CR 400.7
  -- mints no new id and nothing here is an incarnation a card became. What carries
  -- the widening is that CR 400.7e's slot is the printed word "it"/"that
  -- creature" -- the thing the event names, which the ability's source is not --
  -- and Resolve, where the slot is read, never learns which condition placed the
  -- ability, so a second slot would be two names for one notion kept apart by
  -- every reader for a distinction no rule draws. It is also the only choice that
  -- can ever serve CR 603.1b's AnyOf, whose slots eventBindingSlots INTERSECTS: a
  -- fresh name would make the intersection with PermanentEnters empty forever
  -- (#963).
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.TurnedFaceUp carries exactly one ObjectId, and
  -- it is the only thing the event carries. Bound whatever the Filter admitted,
  -- for the PermanentEnters arm's reason.
  --
  -- SelfTurnedFaceUp gets no such arm: there the subject IS the bearer, whom CR
  -- 113.7a's source slot already names.
  (TriggerCondition.PermanentTurnedFaceUp _, GameEvent.TurnedFaceUp oid) ->
    Binding.setBecame oid Map.empty
  -- CR 603.2's "that creature": the permanent the counters went on, which Auntie
  -- Ool, Cursewretch draws off or drains for. The bearer is a bystander, exactly
  -- as it is under the arm above -- Wickersmith's Tools is an artifact watching
  -- creatures -- so the subject needs a name of its own.
  --
  -- THE SAME SLOT, the arm above's widening rather than a fresh one, and for its
  -- reasons: putting counters on a permanent is no zone change (CR 122.1 puts a
  -- marker on the object that is already there, and CR 400.7 mints nothing), so
  -- what carries the reuse is that CR 400.7e's slot is the printed word "that
  -- creature" -- the thing the EVENT names, which CR 113.7a's source is not --
  -- and, as the arm above says, a fresh name would empty CR 603.1b's AnyOf
  -- intersection forever.
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.CountersPut carries exactly one
  -- CounterChange.object. Bound whatever the Filter admitted, for the
  -- PermanentEnters arm's reason.
  --
  -- A DEAD ID IS POSSIBLE and is the payload's problem, `became`'s standing
  -- posture: matchesTrigger admits the CR 704.5f victim a -1/-1 counter made
  -- through Projection.viewWithLastKnown, so a creature that took the counters
  -- and died before the CR 117.5 boundary still fires the trigger with this slot
  -- naming an object that no longer exists.
  --
  -- Its BATCH sibling reaches the fallthrough instead and stamps nothing, which
  -- is eventBindingSlots' answer for it: CR 603.2c's batch reading fires one
  -- PermanentsGetCounters trigger for placements across several permanents, and
  -- one slot cannot name them all.
  (TriggerCondition.PermanentGetsCounters _, GameEvent.CountersPut change) ->
    Binding.setBecame (CounterChange.object change) Map.empty
  -- "That player": the discarder, which CR 701.9a makes one player and the event
  -- carries directly. The same reserved slot CR 702.70a's poisonous uses, for the
  -- same reason -- a player the EVENT names, which CR 109.5's `you` cannot stand
  -- in for.
  (TriggerCondition.PlayerDiscards _, GameEvent.Discarded (Discarded.MkDiscarded discarder _ _)) ->
    Binding.setTriggerPlayer discarder Map.empty
  -- CR 702.86a's "defending player": CR 508.5 resolves that phrase through what
  -- the attacking creature is attacking, and Pawl.Engine.Combat.declareAttackers
  -- stamped the answer onto the event as the declaration was written down. The
  -- same reserved slot the discard and poisonous arms use, for the same reason --
  -- a player the EVENT names, whom CR 109.5's `you` cannot stand in for. Here it
  -- is not even an opponent by construction: the attacking creature's controller
  -- is `you`, and CR 506.2a picks the defender out of several opponents.
  --
  -- Read off the event rather than derived, which is what makes this arm possible
  -- at all: this function takes no game state, and both the planeswalker and the
  -- battle forms of CR 508.5 need the board.
  (TriggerCondition.SelfAttacks _, GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared _ defending _)) ->
    Binding.setTriggerPlayer defending Map.empty
  -- CR 702.83a's "that creature": the creature that attacked alone, which is the
  -- id the same event names -- and NOT the bearer, since rule 702.83a's condition
  -- watches every creature its controller has. The defending player the event
  -- also carries is not bound, because rule 702.83a names no player.
  (TriggerCondition.CreatureAttacksAlone _, GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared attacker _ _)) ->
    Binding.setAttackingCreature attacker Map.empty
  -- The same slot off the same event, for Marchesa's Decree's "that creature's
  -- controller" -- again not the bearer, which is a bystanding enchantment. CR
  -- 508.5's defending player goes unbound here where the SelfAttacks arm above
  -- binds it: matchesTrigger has already required that player to be CR 109.5's
  -- "you", so a slot would be a second name for a seat the ability has.
  (TriggerCondition.CreatureAttacksYou, GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared attacker _ _)) ->
    Binding.setAttackingCreature attacker Map.empty
  -- CR 508.3b's subject, under the same reserved slot every other "that player"
  -- takes: whom the Curse enchants, which matchesTrigger has already required the
  -- event to name. Bound rather than left to the ability's own attachment because
  -- the payload reads it as a player -- Curse of Vitality's "each opponent
  -- attacking that player" -- and Object.attachedTo is a Recipient.
  (TriggerCondition.AttachedPlayerIsAttacked, GameEvent.BecameAttacked (BecameAttacked.MkBecameAttacked _ (AttackTarget.OfPlayer attacked))) ->
    Binding.setTriggerPlayer attacked Map.empty
  -- CR 508.3d's subject: the player who declared the attackers, which is the
  -- only thing the event carries. Bound under Binding.attackingPlayer and not
  -- under the reserved "that player" slot, for the arm below's reason -- rule
  -- 508.3e names two players at once, and one name per role is what makes a
  -- payload that reaches for the wrong one read a dead slot.
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.AttackersDeclared carries a PlayerId, and it
  -- is the only thing it carries.
  (TriggerCondition.PlayerAttacks _, GameEvent.AttackersDeclared attacker) ->
    Binding.setAttackingPlayer attacker Map.empty
  -- CR 508.3e's TWO subjects, both off the one event. Whom the declaration was
  -- aimed at goes under the reserved "that player" slot, which is what the
  -- phrase means in Seifer, Balamb Rival's "goad target creature that player
  -- controls"; the player who declared goes under Binding.attackingPlayer, the
  -- arm above's slot, which is what it means in Archnemesis' "you may attach
  -- this Aura to that player".
  --
  -- The narrowing to AttackTarget.OfPlayer is matchesTrigger's, re-stated here
  -- because this function is not given its answer: an event this condition
  -- rejected reaches no binding at all.
  (TriggerCondition.PlayerAttacksPlayer {}, GameEvent.BecameAttacked (BecameAttacked.MkBecameAttacked attacker (AttackTarget.OfPlayer attacked))) ->
    Binding.setAttackingPlayer attacker (Binding.setTriggerPlayer attacked Map.empty)
  -- CR 509.3e's "that attacking creature", off the grouped blocking event: the
  -- attacker the event names, and again not the bearer, which is a bystander.
  -- CR 508.5's defending player rides that event too and goes unbound, the
  -- CreatureAttacksYou arm above's reasoning -- matchesTrigger has already
  -- required whom the attacker attacked to be a player the PlayerRelation
  -- admits.
  (TriggerCondition.CreatureBecomesBlockedByAtLeast {}, GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked attacker _ _)) ->
    Binding.setAttackingCreature attacker Map.empty
  -- The same slot off matchesTrigger's OTHER road into this condition (rule
  -- 509.3e's "effects that add or remove blockers"): the attacker
  -- GameEvent.BecameBlocking names, which is the creature that just became
  -- blocked by one more. Without this arm the trigger fires with an empty
  -- binding map and Seifer's "that attacking creature" resolves to nothing --
  -- a board indistinguishable from one where it never fired, which is why
  -- Pawl.ZoneTriggerSpec's representativeEvents lists BOTH events for this
  -- condition and intersects what they stamp.
  --
  -- The BLOCKER the event also names goes unbound: this form names a number
  -- rather than an object, and eventBindingSlots promises
  -- Binding.attackingCreature alone.
  --
  -- Unguarded on putOntoBattlefield where matchesTrigger is not, for the
  -- PlayerAttacksPlayer arm's converse reason: an event this condition
  -- rejected reaches no binding at all, so a declaration's clear flag has
  -- already been answered by the time anything asks here.
  (TriggerCondition.CreatureBecomesBlockedByAtLeast {}, GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.attacker = attacker})) ->
    Binding.setAttackingCreature attacker Map.empty
  -- CR 702.130a's "defending player", the same phrase and the same reserved slot
  -- as the arm above -- CR 508.5 resolves it for an ability of an ATTACKING
  -- creature, which is what the bearer of this condition is. Read off the event
  -- for that arm's reason: every writer of it stamps CR 508.5's answer there --
  -- Combat.declareBlockers, Combat.becomeBlocked (CR 509.1h's effect) and
  -- Combat.putOntoBattlefieldBlocking (CR 509.4).
  (TriggerCondition.SelfBecomesBlocked, GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked _ defending _)) ->
    Binding.setTriggerPlayer defending Map.empty
  -- CR 615.13's "that many": how much this prevention effect prevented, which is
  -- the whole reason the event carries a number. The first reserved slot holding
  -- an AMOUNT rather than a reference, read back by Quantity.InSlot off the stack
  -- object these bindings are stamped on (see Binding.eventAmount).
  --
  -- The PLAYER share of the record, not the whole of it: one application can
  -- cover a player and their permanents at once (Divine Deflection), and the
  -- printed sentence this condition spells says "damage that would be dealt to
  -- you", so what was stopped on the way to a permanent is no part of "that
  -- many".
  --
  -- RE-ASKED against the relation, exactly as matchesTrigger's own arm asks it
  -- (CR 109.5): a shield admitting more than one player (Synthetic Impartial
  -- Ward's "any player") stamps every one of their entries into one
  -- DamagePrevented record, and only the share addressed to a player THIS
  -- relation admits is "that much" -- a wider shield's other recipients are no
  -- part of it. `you` is the ability's controller, threaded in from the two
  -- call sites below exactly as matchesTriggerGiven already receives it.
  --
  -- The recipient is NOT bound alongside it. Every payload this CONDITION
  -- carries acts on the ability's own source (Selfless Squire counters itself),
  -- and the player the recipient names here is CR 109.5's "you", already bound.
  (TriggerCondition.DamageToPlayerPrevented relation, GameEvent.DamagePrevented prevented) ->
    Binding.setEventAmount
      ( sum
          ( Map.filterWithKey
              (\recipient _ -> maybe False (PlayerRelation.holds (Game.teams gs) relation you) (Target.playerOf recipient))
              (DamagePrevented.amounts prevented)
          )
      )
      Map.empty
  -- CR 615.13's "that much" once more, off the same event and into the same
  -- reserved slot: the Vindicator deals what its own prevention stopped. The
  -- WHOLE record here where the arm above takes the player share, this condition
  -- being scoped to the applying instance rather than to a recipient.
  --
  -- The recipient is not bound alongside it for the arm above's reason -- the payload
  -- acts on a target it chooses, never on whoever the prevented damage was
  -- addressed to.
  (TriggerCondition.SelfPreventsDamage _, GameEvent.DamagePrevented prevented) ->
    Binding.setEventAmount (sum (DamagePrevented.amounts prevented)) Map.empty
  -- CR 119.9's "that much": how much life the gain was, which CR 603.2 makes part
  -- of the event that fired the trigger -- Sanguine Bond's "target opponent loses
  -- that much life". The SAME slot the prevention arm above stamps, one printed
  -- phrase and one number (see Binding.eventAmount).
  --
  -- The AMOUNT the event recorded, never the gainer's life total: CR 119.3
  -- adjusts a total by the gain, so the two coincide only on a board that started
  -- at nothing, and the printed word means the gain.
  --
  -- The gaining PLAYER alongside it, under the reserved slot the loss arm below
  -- and CR 701.9a's discard trigger already stamp: False Cure's "that player loses
  -- 2 life for each 1 life they gained" reads both halves of one event, and
  -- CR 603.2 makes both halves part of it. Bound whichever relation matched, for
  -- the reason the loss arm spells out -- under You the slot is a second name for
  -- CR 109.5's "you", a redundancy rather than a wrong answer, and
  -- eventBindingSlots answers per condition with no relation in hand.
  (TriggerCondition.PlayerGainsLife _, GameEvent.LifeGained (LifeChange.MkLifeChange pid amount)) ->
    Binding.setTriggerPlayer pid (Binding.setEventAmount amount Map.empty)
  -- The other direction's "that much" -- Exquisite Blood's "you gain that much
  -- life". The same slot and the same reading as the gain arm above, off an
  -- event CR 603.2 makes the number part of.
  --
  -- The AMOUNT the event recorded, never the loser's life total. Under the one
  -- relation a card in the pool uses the two are not even the same player's
  -- number: Exquisite Blood's controller is bound as "you" while the loss is an
  -- opponent's.
  --
  -- The LOSING player alongside it, under the reserved slot CR 701.9a's discard
  -- trigger already stamps: Mindcrank's "that player mills that many cards" reads
  -- both halves of one event, and CR 603.2 makes both halves part of it.
  --
  -- Bound whichever relation matched, and that is a statement about the EVENT
  -- rather than about the relation -- eventBindingSlots below answers per
  -- condition with no relation in hand, so a slot it promises has to hold for
  -- every relation the condition admits. Under You the loser is also CR 109.5's
  -- "you", so the slot is a second name for one player there; that is a
  -- redundancy, not a wrong answer, and the alternative -- binding it only under
  -- Opponent -- would make the promise depend on the relation.
  (TriggerCondition.PlayerLosesLife _, GameEvent.LifeLost (LifeChange.MkLifeChange pid amount)) ->
    Binding.setTriggerPlayer pid (Binding.setEventAmount amount Map.empty)
  -- CR 601.2i's "it": the spell that became cast, which the event names and
  -- which nothing else on the ability does. Presence of the Master's "whenever a
  -- player casts an enchantment spell, counter it" is the reader.
  --
  -- The STACK object, not a card in a hand or a library -- see GameEvent.SpellCast
  -- for why the two are different objects and why this is the one the rule is
  -- about. Guaranteed to be a real id when the binding is made: CR 601.2a leaves
  -- the spell on the stack "until it resolves, it's countered, or a rule or
  -- effect moves it elsewhere", and none of those can have happened before the
  -- gather, the cast being the last thing Pawl.Engine.Cast does. By RESOLUTION
  -- it can be gone -- the case CR 608.2h is about -- which is the payload's
  -- business rather than this function's: CR 701.6a's funnel no-ops on a dead id.
  --
  -- The CASTER alongside it, under the reserved slot CR 701.9a's discard trigger
  -- and CR 702.70a's poisonous already stamp: Kambal, Consul of Allocation's
  -- "that player loses 2 life" is the reader, and CR 112.2 makes that player the
  -- spell's controller -- "by default, the player who put it on the stack".
  --
  -- A SLOT rather than a reader that derives the controller from the bound
  -- spell, and CR 608.2h is the argument: the spell can be GONE by the time this
  -- ability resolves (another trigger counters it first), and a derivation would
  -- then have to fall back to last known information for a fact the event
  -- carried outright. The PlayerLosesLife arm binds both halves of its event for
  -- the same reason.
  --
  -- Bound whatever the Filter admitted, which is what eventBindingSlots'
  -- per-condition promise needs -- that function answers with no event and no
  -- Filter in hand, so a slot it names has to hold for every cast the condition
  -- can match. Both do: GameEvent.SpellCast carries an ObjectId and a PlayerId
  -- unconditionally, so no shape of the event withholds either.
  (TriggerCondition.SpellCast {}, GameEvent.SpellCast (SpellWasCast.MkSpellWasCast caster spell _ _)) ->
    Binding.setTriggerPlayer caster (Binding.setCastSpell spell Map.empty)
  -- CR 702.21a's "that spell or ability": the object whose announcement fired
  -- this, which ward counters and whose controller ward offers the cost to.
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.BecameTarget carries a source.
  --
  -- The TARGETED object is the bearer, already bound as CR 113.7a's source, so it
  -- gets no second name. Nor does the controller: Resolve.payerOf reads this very
  -- slot as "whoever controls that object", which is the whole of rule 702.21a's
  -- "unless that player pays".
  (TriggerCondition.SelfBecomesTargeted _, GameEvent.BecameTarget t) ->
    Binding.setTargetingObject (BecameTarget.source t) Map.empty
  -- The same slot off the same field, one recipient over: Amulet of Safekeeping's
  -- "counter THAT SPELL OR ABILITY unless its controller pays {1}" names the
  -- object that did the targeting, and here the targeted party is a player rather
  -- than the bearer, so nothing else on the ability reaches it.
  --
  -- Unconditional in the same sense as the arm above, and its controller likewise
  -- takes no slot of its own: Resolve.payerOf reads this slot as "whoever
  -- controls that object", which is CR 405.4's answer to "its controller".
  (TriggerCondition.ControllerBecomesTarget {}, GameEvent.BecameTarget t) ->
    Binding.setTargetingObject (BecameTarget.source t) Map.empty
  -- CR 509.3d's "that creature": the blocker whose declaration fired this, which
  -- rule 702.25a's payload gives -1/-1. Unconditional in the same sense as the
  -- arm above -- every GameEvent.BecameBlocking carries both ids -- so
  -- eventBindingSlots' per-condition promise holds with no event in hand.
  --
  -- The ATTACKER on the same event is the bearer, already bound as CR 113.7a's
  -- source, so it gets no second name.
  (TriggerCondition.SelfBecomesBlockedBy _, GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = blocker})) ->
    Binding.setBlockingCreature blocker Map.empty
  -- CR 701.54c's "the blocking creature", off the same field and under the same
  -- name. The bystander form: the ATTACKER is not the bearer here, and it gets no
  -- slot, rule 701.54c naming it only as "your Ring-bearer" in the condition.
  (TriggerCondition.PermanentBecomesBlockedBy _, GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = blocker})) ->
    Binding.setBlockingCreature blocker Map.empty
  -- CR 509.3b's "that creature": the ATTACKER on the very same declaration, which
  -- Loyal Sentry's payload destroys. The mirror of the arm above, and
  -- unconditional for the same reason; here it is the BLOCKER that is the bearer
  -- and so gets no second name.
  (TriggerCondition.SelfBlocksCreature _, GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.attacker = attacker})) ->
    Binding.setBlockedCreature attacker Map.empty
  -- CR 702.134c's "that creature": the creature that was mentored, the event's
  -- second id -- Aegis of the Legion's shield counter goes on it. The MENTOR gets no
  -- slot: matchesTrigger has just proved it is the bearer's host, and no printed
  -- payload points at it.
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.Mentored carries both ids.
  (TriggerCondition.AttachedCreatureMentors, GameEvent.Mentored (Mentored.MkMentored _ mentored)) ->
    Binding.setMentoredCreature mentored Map.empty
  -- CR 400.7f, the sibling of CR 400.7e's `became` arms above: an ability that
  -- triggers when an enchanted permanent leaves the battlefield "can find the new
  -- object that each Aura enchanting that permanent became in its owner's
  -- graveyard". Screams from Within's "return THIS CARD from your graveyard to
  -- the battlefield" is the reader, and this is the id that answers it -- CR
  -- 400.7 having already destroyed the battlefield id CR 113.7a's `triggerSource`
  -- slot carries. Binding.became's own comment draws the line: `triggerSource` is
  -- everything the ability says ABOUT itself, this slot everything it DOES to
  -- itself.
  --
  -- Both sentences of the rule, and one lookup serves them: the "at the same time
  -- the enchanted permanent left the battlefield" case is the wrath, where host
  -- and Aura share an EventGroup, and the CR 704.5m case is the ordinary one,
  -- where the Aura is buried by a LATER pass of the same CR 117.5 batch. The
  -- argument is computed over the whole scanned batch, which is exactly those two
  -- and no wider: an Aura that reached a graveyard at an EARLIER group than the
  -- host's death is offered by none of eventTriggers' candidate sources, so its
  -- trigger is never gathered and this is never asked about it.
  --
  -- Nothing where the bearer did not reach a graveyard: an Equipment host dying
  -- under CR 704.5n leaves the bearer standing, and an effect that sent the Aura
  -- elsewhere in the same batch put it somewhere the rule cannot look. Both are
  -- CR 400.7f's own answer rather than a hole -- the payload finds nothing and
  -- moves nothing -- and eventBindingSlots below says why the floor still claims
  -- `became`.
  --
  -- CR 303.4b's ENCHANTED CREATURE beside it, under Binding.departedPermanent:
  -- Banewasp Affliction's "that creature's controller loses life equal to ITS
  -- toughness" names the host rather than the Aura, and both halves of that
  -- sentence are CR 608.2h reads of a permanent CR 400.7 has already replaced.
  -- Pawl.Engine.Resolve.Slots.effectViewOf is what answers the toughness, and it looks
  -- back for exactly this slot and Binding.sacrificedPermanent;
  -- Pawl.Engine.Projection.controllerWithLastKnown answers the controller for any
  -- slot PlayerRef.ControllerOfBound names.
  --
  -- ZoneChange.departed, not the arrival: the id the card says "that creature"
  -- about is the permanent as it last existed on the battlefield, which is the
  -- argument Pawl.Engine.Binding.departedPermanent already carries for CR
  -- 603.10a. The graveyard incarnation has no controller (CR 108.4) and no
  -- battlefield toughness, so the arrival would answer neither clause.
  --
  -- Unconditional given a match, which is what lets eventBindingSlots below claim
  -- it: matchesTrigger accepts only a GameEvent.Moved out of the battlefield into
  -- a graveyard whose `departed` IS the bearer's host, so the id is always there.
  -- The event is therefore matched rather than wildcarded, unlike every other
  -- arm's use of the first argument.
  (TriggerCondition.AttachedCreatureDies, GameEvent.Moved (Moved.MkMoved zc _ _)) ->
    Binding.setDepartedPermanent (ZoneChange.departed zc) (maybe Map.empty (`Binding.setBecame` Map.empty) bearerBecame)
  -- Nothing at all, stated rather than left to the fallthrough below: the
  -- attachment link already names the permanent that became tapped, and CR 109.5
  -- answers the Aura's "you" from Binding.triggerSource. There is no second
  -- object for the payload to name.
  (TriggerCondition.AttachedCreatureBecomesTapped, _) -> Map.empty
  -- CR 725.1's newly crowned player: Garland, Royal Kidnapper's "that player",
  -- whose creature the trigger then targets and whose crown its duration watches.
  -- Bound whichever relation matched, for the reason the PlayerLosesLife arm
  -- gives: eventBindingSlots answers per CONDITION with no relation in hand, so
  -- the slot has to hold for every relation this condition admits. Under You the
  -- crowned player is also CR 109.5's "you", which is a redundancy rather than a
  -- wrong answer.
  --
  -- Unconditional given a match: GameEvent.BecameMonarch carries a PlayerId
  -- outright, and CR 725.3 makes it exactly one.
  (TriggerCondition.PlayerBecomesMonarch _, GameEvent.BecameMonarch crowned) ->
    Binding.setTriggerPlayer crowned Map.empty
  -- CR 701.21a's sacrificing player: Vengeful Tracker's "this creature deals 2
  -- damage to THEM", a seat CR 109.5's "you" is not -- the condition's Opponent
  -- relation is what makes the two come apart.
  --
  -- Bound whichever relation matched, the PlayerBecomesMonarch arm's reason:
  -- eventBindingSlots answers per CONDITION with no relation in hand, so the slot
  -- has to hold for every relation this condition admits, and under You it is a
  -- redundant second name rather than a wrong one.
  --
  -- Unconditional given a match: GameEvent.PermanentSacrificed carries a PlayerId
  -- outright, CR 701.21a's sacrifice having exactly one performer.
  --
  -- The PERMANENT is not bound. The id the event carries is the pre-move one CR
  -- 603.10a looked back for, and which slot a printed "exile it" wants is the
  -- question #977 still holds open.
  (TriggerCondition.PermanentSacrificed {}, GameEvent.PermanentSacrificed ev) ->
    Binding.setTriggerPlayer (PermanentWasSacrificed.player ev) Map.empty
  -- CR 603.3b's "that Saga" and "that player": GameEvent.AbilityTriggered names
  -- the object the chapter ability hangs on (CR 113.7) and the player who
  -- controls it (CR 603.3a), and the watcher is neither of them.
  -- data/cards/synthetic-chronicle-warden.json reads both, and its Opponent
  -- relation is what makes the seat something CR 109.5's "you" is not.
  --
  -- The Saga under CR 400.7e's slot rather than one of its own, which is
  -- PermanentTurnedFaceUp's case above and settled there: one permanent, one id,
  -- two unrelated objects, and Pawl.Engine.Resolve never learns which condition
  -- placed the ability whose slot it is reading, so a second slot would have to
  -- be kept apart by every reader for a distinction no rule draws.
  --
  -- Both unconditional given a match, which is what eventBindingSlots'
  -- per-condition promise needs: matchesTrigger's arm rejects
  -- TriggerSource.Sourceless outright -- CR 725.2's and CR 702.179d's abilities
  -- hang on no Saga -- so every record it accepts carries an ObjectId, and every
  -- record carries a controller.
  (TriggerCondition.SagaFinalChapterTriggers _, GameEvent.AbilityTriggered record) ->
    case AbilityTriggered.source record of
      TriggerSource.Sourceless -> Map.empty
      TriggerSource.OfObject saga -> Binding.setBecame saga (Binding.setTriggerPlayer (AbilityTriggered.controller record) Map.empty)
  -- CR 120.3's "that much", read by the damage's RECIPIENT: Coalhauler Swine's
  -- "whenever this creature is dealt damage, it deals that much damage to each
  -- player". The same reserved slot CR 615.13's prevention and CR 119.9's life
  -- gain stamp (see Binding.eventAmount), and the same reading -- the AMOUNT the
  -- event recorded, never the damager's power or the bearer's: CR 702.19b lets a
  -- trampler split its power across a blocker and a player, and CR 120.3 admits
  -- noncombat damage whose amount its source's power never named at all.
  --
  -- ONE event's amount, not a batch's: CR 510.2 deals a combat damage step's
  -- damage simultaneously and Pawl.Engine.Damage records a DamageDealt per
  -- surviving event, so two blockers stamp two triggers with their own numbers
  -- rather than one with the sum. Pinned by Pawl.TriggerSpec's two-blocker board.
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.DamageDealt carries a DamageEvent.amount,
  -- whichever of CR 120.3's damage kinds it is.
  --
  -- The DAMAGER takes Binding.combatDamager beside it, which is CR 120.1's "an
  -- object that deals damage is the source of that damage" -- Belltower Sphinx's
  -- "that source's controller mills that many cards", read through
  -- PlayerRef.ControllerOfBound. Also unconditional given a match:
  -- DamageEvent.source is an ObjectId outright, so there is nothing to fail.
  --
  -- The slot is not combat-scoped here even though its name is: CR 120.1 makes
  -- every damage event name a source, and Belltower Sphinx's trigger admits a
  -- Prodigal Sorcerer's ping as readily as a blocker's. Pinned by
  -- Pawl.TriggerSpec's noncombat board.
  --
  -- The RECIPIENT needs no slot -- matchesTrigger has just proved it is the
  -- bearer, whom CR 113.7a's source slot already names.
  (TriggerCondition.SelfIsDealtDamage, GameEvent.DamageDealt ev) ->
    Binding.setCombatDamager (DamageEvent.source ev) (Binding.setEventAmount (DamageEvent.amount ev) Map.empty)
  -- CR 603.1b's multi-condition ability reaches this fallthrough and stamps
  -- nothing, which agrees with eventBindingSlots' intersection for the pool's one
  -- AnyOf and is pinned by Pawl.TriggerSpec against every event either branch
  -- admits. An AnyOf two of whose branches bind the SAME slot is not handled
  -- (#963).
  --
  -- So do the five CR 701/702 keyword-action conditions, deliberately: no card
  -- in the pool reads the scrying player, the plotted card or the explorer, and
  -- SelfExerted's "it" is the bearer, which CR 113.7a's source slot already
  -- names. eventBindingSlots claims nothing for any of them; see that function's
  -- arms.
  --
  -- CR 705.2's PlayerWinsCoinFlip reaches it too, on the same reasoning: the
  -- winner is CR 109.5's "you", whom Binding.setYou already names, and Tavern
  -- Scoundrel's "create two Treasure tokens" reads nothing else off the flip.
  --
  -- And so does SelfDiscarded, for SelfCycled's reason: CR 701.9a's discarded
  -- card is the bearer, whom CR 113.7a's source slot already names, and its owner
  -- is CR 113.8's controller, whom Binding.setYou already names.
  --
  -- CR 603.12's Reflexive reaches it NECESSARILY rather than by choice: it
  -- admits no event at all, so delayedPending never calls this for one and there
  -- is nothing an arm could read. What such an ability knows comes from CR
  -- 603.7c's captured environment instead.
  _ -> Map.empty

-- CR 400.7e's `became` slot, in the plural CR 712.21c asks for: "if an effect
-- can find the new object that a melded permanent becomes as it leaves the
-- battlefield, it finds both cards... If that effect causes actions to be taken
-- upon those cards, the same actions are taken upon each of them."
--
-- ONE arrival is bound exactly as it always was -- Binding.toObject, a recipient
-- -- so every ordinary move produces the byte-identical binding it did before
-- this rule landed, and no reader of the slot in data/cards/ can tell the
-- difference. TWO OR MORE are bound as a GROUP (Binding.toObjects), which is
-- the shape Pawl.Types.Binding documents for "every object one instruction
-- produced or acted on" and which Pawl.Engine.Resolve.Slots.objectRefObjects reads
-- ahead of the recipient path for ObjectRef.InSlot.
--
-- The shape is conditional because the two readers are: an ObjectRef reads
-- either, while a bare SlotName (Effect.ExileHaunting's `card`,
-- Pawl.Engine.Resolve's legalOne) reads only the recipient. Every
-- condition that can carry a split is a battlefield DEPARTURE -- CR 712.21's own
-- scope -- and the pool's three readers under those (Promise of Tomorrow, Yedora
-- Grave Gardener and Endless Cockroaches, all under SelfDies or PermanentDies)
-- spend the slot as an ObjectRef, so the group reaches every one of them.
--
-- Two bare-SlotName readers exist and neither is reachable. Unstable
-- Shapeshifter hangs on PermanentEnters, which cannot split -- Agent's Toolkit
-- hangs there too and is no longer one of these at all, CR 122.5's destination
-- having become an ObjectRef. Rule
-- 702.55a's haunt ability (Pawl.Engine.Keyword) hands Binding.became to
-- Effect.ExileHaunting under SelfDies, which CAN split -- but haunt is granted by
-- no card and printed on neither half of the pool's one meld pair, so no board
-- reaches it (#3105). Screams from Within is NOT among these: its `became` comes
-- from AttachedCreatureDies, which binds the AURA's own incarnation
-- (`bearerBecame`) rather than the event's arrivals, and no Aura is a meld
-- component.
--
-- Not implemented: a group and a recipient at once, which would let a bare
-- SlotName reader see the first card while an ObjectRef reader saw both.
-- Pawl.Types.Binding's `objects` states the invariant that no slot carries both
-- (#3105).
setBecameArrivals :: Moved.Moved -> Map.Map SlotName.SlotName Binding -> Map.Map SlotName.SlotName Binding
setBecameArrivals m = case Moved.arrivals m of
  only Seq.:<| Seq.Empty -> Binding.setBecame only
  every -> Binding.setBecameGroup every

-- Which slots eventBindings above can stamp for a condition, as a set. A
-- CLASSIFICATION of a rule 603 trigger condition -- the sibling of
-- zonesTriggeredFrom below, which asks the other structural question about the
-- same closed type -- so it never reaches an ability's payload and no reader of
-- it learns what any effect IS.
--
-- Its customer is the card lint (CardSpec's "every slot a triggered ability
-- reads is bound for its condition"): an effect naming CR 400.7e's `became` or
-- CR 702.70a's `thatPlayer` under a condition that binds neither would place its
-- trigger, miss the lookup and silently do nothing, which is the worst failure
-- mode card data has.
--
-- Exhaustive with no wildcard, deliberately unlike eventBindings' own
-- `_ -> Map.empty`: that case is over (condition, event) PAIRS, where a wildcard
-- is the only way to say "this pair does not match", while a new CONDITION here
-- must force a decision rather than defaulting to "binds nothing" -- the default
-- that would silently un-lint whatever slot the new condition binds.
--
-- A PARALLEL STATEMENT, PINNED BY A TEST. This says in one dimension what
-- eventBindings says in two, so the two can drift out of agreement. Deriving
-- this from that would mean fabricating a representative GameEvent per condition
-- inside the rules core, which is fixture work the engine has no other use for;
-- the agreement is therefore pinned from the test side instead, by TriggerSpec's
-- "CR 603.2 eventBindingSlots names exactly the keys eventBindings stamps for
-- EVERY event a condition admits", which runs every condition against the events
-- that genuinely fire it and intersects the Map.keysSet of each result against
-- the answer here.
--
-- Every slot named here is GUARANTEED given a match, the only reading that makes a
-- per-CONDITION set sound: the answer must hold for every event the condition
-- admits, the card lint having no event in hand. For most conditions the readings
-- coincide, matchesTrigger having already pinned the destination or recipient.
-- SelfLeavesTheBattlefield is where they come apart, and gets the floor.
eventBindingSlots :: TriggerCondition -> Set.Set SlotName.SlotName
eventBindingSlots cond = case cond of
  -- CR 309.4c names nothing but the room, and the room is in the condition rather
  -- than in the event's bindings. The dungeon card itself arrives under CR 113.7a's
  -- reserved source slot, which every borne trigger gets at placement.
  TriggerCondition.RoomEntered _ -> Set.empty
  -- Nothing either: CR 309.7 names the completing player, and Dungeon Crawler's
  -- payload points at no one -- it returns itself.
  TriggerCondition.PlayerCompletesDungeon _ -> Set.empty
  -- Nothing, for all four keyword actions. CR 701.22d and CR 701.25d name a
  -- player and CR 702.170a and CR 701.44b an object, but no printed payload
  -- under any of them points at one: Matoya, Archon Elder draws, Aloe
  -- Alchemist targets a creature of its controller's choosing and Wildgrowth
  -- Walker grows itself. A card printing "that player" or "that creature" is
  -- what would earn a slot, and eventBindings has no arm for any of the four
  -- until one does.
  TriggerCondition.PlayerScries _ -> Set.empty
  TriggerCondition.RingTemptsPlayer _ -> Set.empty
  TriggerCondition.PlayerSurveils _ -> Set.empty
  TriggerCondition.SelfBecomesPlotted -> Set.empty
  TriggerCondition.PermanentExplores _ -> Set.empty
  -- Nothing here either. CR 706.1's event names the roller, but Feywild
  -- Trickster's payload points at no one -- it creates a token for its own
  -- controller -- and a card printing "that player" is what would earn a slot.
  -- The roll's numerical RESULT is not a binding of this condition at all:
  -- Pawl.Engine.Resolve binds it at Pawl.Types.RollDie's own slot, during the
  -- roller's own resolution, for a later effect of THAT ability to read.
  TriggerCondition.PlayerRollsDice _ -> Set.empty
  TriggerCondition.PlayerWinsCoinFlip _ -> Set.empty
  -- Empty for the same reason, and CR 701.43d is what settles it: the linked
  -- trigger's "it" is the exerted permanent, which is already CR 113.7a's source
  -- slot, so a binding here would be a second name for one object. Glory-Bound
  -- Initiate reads it as Filter.IsSource.
  TriggerCondition.SelfExerted -> Set.empty
  -- Empty DELIBERATELY. CR 701.3a's event names two objects, and the bearer is
  -- one of them -- CR 113.7a's source slot already names the host. The other,
  -- the attachment, has no printed reader: Bramble Elemental says "create two
  -- 1\/1 green Saproling creature tokens" and names no "it". Enormous Energy
  -- Blade's "tap that creature" is the printing that earns a slot, and it reads
  -- the event from the other end (gap #1837).
  TriggerCondition.SelfBecomesAttachedBy _ -> Set.empty
  -- CR 603.6a's two written forms differ only in which object the bearer is.
  -- SelfEnters matches on `object == bearer`, so CR 113.7a's source slot already
  -- names the entrant and `became` would be a second name for one object.
  -- "Whenever a [type] enters" has no such luck.
  TriggerCondition.SelfEnters -> Set.empty
  TriggerCondition.PermanentEnters _ -> Set.singleton Binding.became
  -- CR 603.2b's step beginning names no OBJECT -- but it names the active player,
  -- and the active player is not what CR 109.5's `you` means, so "that player"
  -- needs a slot of its own. Shizuko, Caller of Autumn is the reader.
  --
  -- Unconditional, as this classification has to be: every GameEvent.StepBegan
  -- carries a PlayerId, whatever the TurnScope.
  TriggerCondition.StepBegins {} -> Set.singleton Binding.triggerPlayer
  -- CR 603.8: a state trigger matches a game STATE rather than an event
  -- (matchesTrigger's StateIs arm answers False for every event), so no event
  -- contributes anything to one.
  TriggerCondition.StateIs _ -> Set.empty
  -- CR 702.70a's "that player": the player the bearer dealt combat damage to.
  --
  -- CR 510.2's amount beside it, which Questing Beast's "that much" reads: the
  -- same slot CR 615.13's prevention and CR 119.9's life gain stamp. Guaranteed
  -- given a match -- every DamageDealt event carries an amount.
  TriggerCondition.SelfDealsCombatDamageToPlayer -> Set.fromList [Binding.eventAmount, Binding.triggerPlayer]
  -- CR 120.3's amount for enrage, which Coalhauler Swine's "it deals that much
  -- damage to each player" reads: the same slot CR 615.13's prevention and CR
  -- 119.9's life gain stamp, and guaranteed given a match -- every DamageDealt
  -- event carries an amount, whichever of CR 120.3's two damage kinds this
  -- unfiltered condition admitted.
  --
  -- Plus CR 120.1's source, which Belltower Sphinx's "that source's controller"
  -- reads, and equally guaranteed -- every DamageDealt event carries one. No slot
  -- for the recipient, who is the bearer. See the eventBindings arm above.
  TriggerCondition.SelfIsDealtDamage -> Set.fromList [Binding.combatDamager, Binding.eventAmount]
  -- CR 510.2's damager, which the bystander's form needs and the self-scoped one
  -- above does not: there the damager IS the bearer, already bound as CR 113.7a's
  -- source. Aragorn, Hornburg Hero's "double the number of +1/+1 counters on it"
  -- is the reader. Guaranteed given a match -- every DamageDealt event carries a
  -- source.
  --
  -- CR 510.2's amount beside its damager, which Shroofus Sproutsire's "that many"
  -- reads: the same slot CR 615.13's prevention and CR 119.9's life gain stamp, and
  -- guaranteed given a match for the same reason -- every DamageDealt event carries
  -- an amount.
  --
  -- CR 603.2's "that player" beside them, which Larceny's "that player discards a
  -- card" reads -- the same slot the self-scoped arm above stamps. Guaranteed
  -- given a match: matchesTrigger admits only a player recipient here.
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> Set.fromList [Binding.combatDamager, Binding.eventAmount, Binding.triggerPlayer]
  -- Empty where the arm above binds three, and NECESSARILY so, PermanentsDie's
  -- reason one event family over: the trigger event is a whole CR 510.2 step,
  -- which may hold several damagers, amounts and damaged players, and one slot
  -- cannot name them all. Pia Nalaar, Chief Mechanic's payload names none of
  -- them. Not implemented: a slot for the damagers' CONTROLLER, which Norn's
  -- Decree's "that opponent" reads and which IS one seat per step, CR 508.1
  -- letting only the active player declare attackers (#2930).
  TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> Set.empty
  -- CR 725.2's inherent ability is borne by no card, and its bindings come from
  -- Monarch.inherentMatch rather than eventBindings -- so a card declaring this
  -- condition would honestly get nothing from the event.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> Set.empty
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> Set.empty
  TriggerCondition.PlayerTookInitiative -> Set.empty
  -- CR 702.179d's ability is borne by no card either, and binds nothing at all --
  -- "your speed" is the controller's, whom Binding.setYou already names.
  TriggerCondition.OpponentLostLifeDuringYourTurn -> Set.empty
  -- CR 702.29c's cycled card is the bearer itself, already bound as CR 113.7's
  -- source.
  TriggerCondition.SelfCycled -> Set.empty
  -- CR 702.94a's revealed card is the bearer itself, already bound as CR 113.7's
  -- source, and the revealing player is its owner -- the same seat CR 113.8 makes
  -- the ability's controller, whom Binding.setYou already names.
  TriggerCondition.SelfRevealedForMiracle -> Set.empty
  -- CR 701.9a's discarded card is the bearer itself, already bound as CR 113.7's
  -- source, and the discarding player is its owner -- the same seat CR 113.8 makes
  -- the ability's controller, whom Binding.setYou already names.
  TriggerCondition.SelfDiscarded -> Set.empty
  -- CR 701.9a's discarding player, which is nobody the bearer already names --
  -- Megrim's "that player" is the opponent whose hand the card left.
  TriggerCondition.PlayerDiscards _ -> Set.singleton Binding.triggerPlayer
  -- NOTHING, where the cause-blind sibling above binds the discarder. Prickly
  -- Marmoset's payload says "this creature", which is CR 113.7's source slot
  -- the placement already stamps, and names no player; a printing under this
  -- condition that said "that player" is what would earn the slot. eventBindings
  -- has no arm for this condition, and the two must agree.
  TriggerCondition.PlayerCycles _ -> Set.empty
  -- NOTHING. The event names the drawing player, and CR 701.9a's `triggerPlayer`
  -- is the slot they would take, but no card in the pool reads them under this
  -- condition: Erudite Wizard's payload points only at its own bearer, and Faerie
  -- Mastermind's says "you". Ian Malcolm, Chaotician's "that player exiles" is the
  -- card that would add the slot, and it needs a PlayerRelation for "a player"
  -- that does not exist either.
  TriggerCondition.PlayerDrawsNthCard {} -> Set.empty
  -- CR 508.5's defending player, which the declaration event carries -- rule
  -- 702.86a's annihilator is the reader. The DECLARED attacker itself is the
  -- bearer, already bound as CR 113.7a's source, so it needs no slot of its own.
  --
  -- Unconditional, as this classification has to be: every AttackerDeclared event
  -- carries a PlayerId, so no shape of the event withholds it.
  TriggerCondition.SelfAttacks _ -> Set.singleton Binding.triggerPlayer
  -- NOTHING, unlike SelfAttacks above: rule 702.149a's payload names only "this
  -- creature", so neither the companion that qualified nor CR 508.5's defending
  -- player is pointed at afterwards.
  TriggerCondition.SelfAttacksWithAnother _ -> Set.empty
  -- CR 506.5's lone attacker, which the same event names -- rule 702.83a's
  -- exalted is the reader. Where SelfAttacks above needs a slot for the PLAYER
  -- and gets the creature free (it is the bearer), this one needs a slot for the
  -- CREATURE and wants no player: the bearer is a bystander.
  --
  -- Unconditional, as this classification has to be: every AttackerDeclared event
  -- carries an ObjectId, and matchesTrigger has already required the count to be
  -- one.
  TriggerCondition.CreatureAttacksAlone _ -> Set.singleton Binding.attackingCreature
  -- The attacker, CreatureAttacksAlone's slot above -- Marchesa's Decree's "that
  -- creature's controller". CR 508.5's defending player is NOT bound alongside it:
  -- the match has already pinned that player to be CR 109.5's "you".
  --
  -- Unconditional, as this classification has to be: every AttackerDeclared event
  -- carries an ObjectId.
  TriggerCondition.CreatureAttacksYou -> Set.singleton Binding.attackingCreature
  -- The PLAYER instead, where the arm above binds the attacker: rule 508.3b names
  -- a set of creatures rather than one, and Curse of Vitality's payload says
  -- "that player" and nothing about them.
  TriggerCondition.AttachedPlayerIsAttacked -> Set.singleton Binding.triggerPlayer
  -- The DECLARING player, and not the creatures: rule 508.3d names a SET of
  -- them, so there is no one attacker to point at, where the player the rule
  -- makes its subject is exactly one seat. Norn's Decree's "the attacking
  -- player" is the phrase.
  --
  -- Unconditional, as this classification has to be: every
  -- GameEvent.AttackersDeclared carries a PlayerId.
  TriggerCondition.PlayerAttacks _ -> Set.singleton Binding.attackingPlayer
  -- The creatures are not bound, for the arm above's reason: rule 508.3c's
  -- Filter names a SET of them. Not implemented: the declaring player, which the
  -- same GameEvent.AttackersDeclared carries and the arm above stamps as
  -- Binding.attackingPlayer -- Total War's "that player" reads it (#2937).
  TriggerCondition.PlayerAttacksWith {} -> Set.empty
  -- BOTH of rule 508.3e's players: the attacked one under the reserved "that
  -- player" slot, which Seifer, Balamb Rival's "that player controls" reads, and
  -- the attacking one under the slot the PlayerAttacks arm above uses, which
  -- Archnemesis' "attach this Aura to that player" reads. The only condition
  -- that promises two players, rule 508.3e being the only one whose subject is a
  -- pair.
  --
  -- Unconditional, as this classification has to be, although matchesTrigger
  -- admits only AttackTarget.OfPlayer: every event this condition MATCHES
  -- carries both players, and eventBindings is consulted for no other.
  TriggerCondition.PlayerAttacksPlayer {} -> Set.fromList [Binding.triggerPlayer, Binding.attackingPlayer]
  -- NOTHING, for SelfAttacksWithAnother's reason: rule 702.105a's payload names
  -- only "this creature", so the attacked player is compared and then never
  -- pointed at. That is also why this condition needs no arm in eventBindings.
  TriggerCondition.SelfAttacksPlayerWithMostLife -> Set.empty
  -- Nothing, unlike SelfAttacks above: the blocker is the bearer, already bound
  -- as CR 113.7a's source, and the attacker the event also carries is what CR
  -- 509.3b's condition names rather than this one, below. CR 509.1a makes the
  -- blocker's controller the defending player, whom CR 109.5's `you` already
  -- names.
  TriggerCondition.SelfBlocks -> Set.empty
  TriggerCondition.SelfBlocksAtLeast _ -> Set.empty
  TriggerCondition.SelfBlocksOneOrMore _ -> Set.empty
  -- CR 509.3b's form is the one that DOES name the attacker, off the same event.
  -- Guaranteed rather than conditional, as SelfBecomesBlockedBy's is: every
  -- declaration carries both ids, and matchesTrigger has already pinned the
  -- blocker to the bearer.
  TriggerCondition.SelfBlocksCreature _ -> Set.singleton Binding.blockedCreature
  -- CR 508.5's defending player, which the becomes-blocked event carries for
  -- SelfAttacks' reason -- rule 702.130a's afflict is the reader. No BLOCKER: CR
  -- 509.3c names none, so GameEvent.AttackerBlocked carries none, and CR 509.3d's
  -- form below is the one that binds one. The blocked attacker itself is the
  -- bearer, already bound as CR 113.7a's source.
  --
  -- Unconditional, as this classification has to be: every AttackerBlocked event
  -- carries a PlayerId.
  TriggerCondition.SelfBecomesBlocked -> Set.singleton Binding.triggerPlayer
  -- CR 509.3d's form is the one that DOES name a blocker, and
  -- GameEvent.BecameBlocking carries it: rule 702.25a's "the blocking creature".
  -- Guaranteed rather than conditional -- every such event carries both ids, and
  -- matchesTrigger has already pinned the attacker to the bearer.
  TriggerCondition.SelfBecomesBlockedBy _ -> Set.singleton Binding.blockingCreature
  TriggerCondition.PermanentBecomesBlockedBy _ -> Set.singleton Binding.blockingCreature
  -- CR 509.3e names a SET of blockers rather than one, and no reader in the
  -- pool reaches into it: Serra Inquisitors' payload names only itself.
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> Set.empty
  -- Seifer, Balamb Rival's "that attacking creature", the same reserved slot
  -- CreatureAttacksYou fills off its own event.
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> Set.singleton Binding.attackingCreature
  -- CR 509.1h's unblocked branch names no second object at all -- no blocker,
  -- and no defending player on GameEvent.AttackerUnblocked to bind one from.
  -- eventBindings therefore has no arm and falls through to the empty map.
  TriggerCondition.SelfAttacksUnblocked -> Set.empty
  -- CR 113.6k: the bearer of a library-to-graveyard trigger IS the arriving
  -- incarnation, so binding it again under `became` would be a second name for
  -- one object. Narcomoeba reads the source slot instead.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Set.empty
  -- The same answer for the same reason: with no look-back (CR 603.6c's last
  -- sentence), this condition's bearer already IS the arriving incarnation, so
  -- `became` would be a second name for one object. Serra Avatar's "shuffle IT"
  -- reads the source slot.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> Set.empty
  -- CR 400.7e's arrival, and here it is the CARD the condition matched on rather
  -- than a second name for the bearer -- Planar Void's "exile THAT CARD". One
  -- object and never a group: this condition sees one arrival per event, the
  -- CardArrived events carrying the rest, which is what setBecameArrivals'
  -- group binding is for on the conditions that read the whole move at once.
  --
  -- Guaranteed given a match, matchesTrigger having pinned the destination to
  -- the graveyard, which CR 400.2 makes a public zone.
  TriggerCondition.CardPutIntoGraveyard _ -> Set.singleton Binding.became
  -- CR 400.7e: the incarnation the card became, which CR 603.10a's look-back
  -- keeps out of the source slot.
  TriggerCondition.SelfDies -> Set.singleton Binding.became
  -- PermanentEnters' `became` pointed at the opposite zone change, the two being
  -- the same bystander shape: CR 400.7e supplies the name and CR 400.2 makes the
  -- graveyard public. Guaranteed rather than conditional, unlike
  -- SelfLeavesTheBattlefield below, because matchesTrigger has already pinned the
  -- destination to the graveyard. Promise of Tomorrow's "exile it" is the reader.
  -- CR 400.7e's graveyard card and CR 603.10a's departed permanent, the pair
  -- AttachedCreatureDies binds below and for the same reason: the two are
  -- different objects, and a card names whichever its sentence means (Promise of
  -- Tomorrow the arrival, Cleopatra, Exiled Pharaoh the deceased). Both
  -- guaranteed given a match -- matchesTrigger has pinned the battlefield-to-
  -- graveyard pair, which CR 400.2 makes public, and every zone change carries
  -- `departed`.
  TriggerCondition.PermanentDies _ -> Set.fromList [Binding.became, Binding.departedPermanent]
  -- Empty where PermanentDies binds two, and NECESSARILY so: the trigger event is
  -- a whole CR 704.3 batch, which may have buried several cards, and neither slot
  -- can name them all. Nothing in print asks for
  -- one either: Scryfall o:"one or more other creatures you control die",
  -- 2026-08-24, matches Vengeful Townsfolk and Vraan, Executioner Thane, whose
  -- payloads act on the bearer and on the players. A printing whose payload said
  -- "it" would refute this, and the lint would reject it -- and rightly, since
  -- one slot naming one of several buried cards is no partial binding but a
  -- wrong one, which is why this is no member of eventBindingSlotsSometimes.
  TriggerCondition.PermanentsDie _ -> Set.empty
  -- The same slot and rule as SelfDies, but bound only for a PUBLIC destination
  -- (CR 400.7e's proviso over CR 400.2's hidden zones), so the guaranteed floor is
  -- empty. A card whose leaves-the-battlefield payload names `became` is still
  -- accepted: the slot is claimed by eventBindingSlotsSometimes below, which the
  -- card lint unions in, and the rule's own answer for a hidden destination is
  -- that the ability finds nothing.
  TriggerCondition.SelfLeavesTheBattlefield -> Set.empty
  -- CR 400.7e's `became` is withheld for the arm above's reason and for a second
  -- one: this condition's other event (CR 603.6c's leaving-the-game form)
  -- reaches no zone, so even a public destination is not guaranteed by a match.
  -- CR 603.10a's departed permanent IS guaranteed, and by both events -- a zone
  -- change carries ZoneChange.departed and a departure from the game carries the
  -- id itself -- which is what lets Resourceful Defense read "it".
  TriggerCondition.PermanentLeavesTheBattlefield _ -> Set.singleton Binding.departedPermanent
  -- CR 603.10a's departed permanent, the arm above's reason, and beside it the
  -- player whose hand it reached -- CR 400.3's OWNER, which Warped Devotion's
  -- "that player discards a card" reads. Both guaranteed given a match: the one
  -- event this condition admits is a zone change out of the battlefield, and the
  -- funnel files CR 608.2h's record of the departing id in the same write that
  -- deletes it. NOT off the arrival, which CR 704.5d can have already taken away
  -- (a token); eventBindings' own arm has the whole of that. CR 400.7e's
  -- `became` is withheld, the hand being hidden (CR 400.2).
  TriggerCondition.PermanentReturnedToHand _ -> Set.fromList [Binding.departedPermanent, Binding.triggerPlayer]
  -- Empty where the arm above binds two, and NECESSARILY so, PermanentsDie's
  -- reason one event family over: the batch may return several permanents to
  -- several owners' hands, and one slot cannot name them all. Tameshi, Reality
  -- Architect's payload names none of them.
  TriggerCondition.PermanentsReturnedToHand _ -> Set.empty
  -- Nothing either, and not for the batch arm's reason: Kishla Skimmer's payload
  -- draws a card and names neither the card that left nor what it became, so no
  -- slot is earned. eventBindings has no arm for this condition, which the empty
  -- floor pins -- and a FLOOR is what it would have to clear: this condition
  -- admits every destination, and CR 400.2 makes a hand and a library hidden, so
  -- CR 400.7e withholds `became` for some of the moves it matches.
  TriggerCondition.CardLeavesGraveyard {} -> Set.empty
  -- Nothing, where PermanentDies binds CR 400.7e's graveyard card and CR 603.10a's
  -- departed permanent: rule 702.55b's ability speaks about the creature it
  -- HAUNTS -- an object GameState.haunting names rather than the event -- and
  -- never about the deceased or the card it became, so no printing of haunt names
  -- either. eventBindings has no arm for this condition either, which is what the
  -- empty floor pins.
  TriggerCondition.HauntedCreatureDies -> Set.empty
  -- CR 701.6a's countering names two objects and a player and this binds none of
  -- them -- eventBindings has no arm for it. Empty by decision rather than
  -- default: both ids are dead by the time the trigger resolves, and CR 400.7e
  -- would name the countered card in its owner's graveyard. A card that says
  -- "exile it instead" is the one that must bind `became` here.
  TriggerCondition.SpellOrAbilityCounters _ -> Set.empty
  -- CR 615.13's amount, guaranteed given a match: the event carries a Natural
  -- unconditionally, so unlike SelfLeavesTheBattlefield's `became` there is no
  -- shape of the event that withholds it.
  TriggerCondition.DamageToPlayerPrevented _ -> Set.singleton Binding.eventAmount
  -- The same slot the arm above declares, off the same event: this condition's
  -- payload reads "that much" too.
  TriggerCondition.SelfPreventsDamage _ -> Set.singleton Binding.eventAmount
  -- CR 119.9's amount, guaranteed given a match for the prevention arm's reason:
  -- GameEvent.LifeGained carries a Natural unconditionally, so no shape of the
  -- event withholds it. Sanguine Bond's "that much" is what reads it.
  --
  -- And the GAINING player, for the reason the loss arm below binds the loser:
  -- under False Cure's AnyPlayer relation that player is not the "you"
  -- Binding.setYou names, and "that player loses 2 life for each 1 life they
  -- gained" reads them. Guaranteed for the same reason the amount is --
  -- GameEvent.LifeGained carries a PlayerId unconditionally, so the promise holds
  -- under every relation.
  TriggerCondition.PlayerGainsLife _ -> Set.fromList [Binding.eventAmount, Binding.triggerPlayer]
  -- Nothing, PermanentsGetCounters' answer and for its reason: the trigger event
  -- is a whole CR 608.2f batch, which may hold a gain by every seat at the table
  -- and a different number for each, so neither reserved slot could name one.
  -- Synthetic Communal Vigil, the card that landed the constructor, draws a card
  -- and reads no part of the event; a printing whose payload said "that player"
  -- would refute this, and the lint would reject it -- PermanentsDie's case
  -- above, and no member of eventBindingSlotsSometimes for its reason.
  TriggerCondition.PlayersGainLife _ -> Set.empty
  -- The loss condition's amount, guaranteed for the same reason:
  -- GameEvent.LifeLost carries a Natural unconditionally. Exquisite Blood's "you
  -- gain that much life" is what reads it.
  --
  -- And the LOSING player, which is what separates this from the gain arm above:
  -- under Exquisite Blood's Opponent relation that player is NOT the "you"
  -- Binding.setYou names, and Mindcrank's "that player mills that many cards"
  -- reads them. Guaranteed for the same reason the amount is -- GameEvent.LifeLost
  -- carries a PlayerId unconditionally, so the promise holds under either
  -- relation.
  TriggerCondition.PlayerLosesLife _ -> Set.fromList [Binding.eventAmount, Binding.triggerPlayer]
  -- CR 714.2b names one object -- the bearer -- which CR 113.7a's source slot
  -- already names, so `became` would be a second name for it. The counts the
  -- event carries are the CONDITION's, not the payload's: no chapter ability in
  -- print says "that many", and eventBindings has no arm for this condition.
  TriggerCondition.SelfCountersReached {} -> Set.empty
  TriggerCondition.SelfBecomesClassLevel _ -> Set.empty
  TriggerCondition.SelfLastCounterRemoved _ -> Set.empty
  -- CR 603.2's "that much", the one thing this condition binds that neither
  -- sibling does: Chandra, Fire Artisan reads the number of counters that came
  -- off. The bearer needs no slot, CR 113.7a's source slot already naming it.
  TriggerCondition.SelfCountersRemoved _ -> Set.singleton Binding.eventAmount
  -- Nothing, PermanentsDie's answer and for its reason: the trigger event is a
  -- whole CR 704.3 / CR 608.2f batch, which may have put counters on several
  -- permanents, and one slot cannot name them all. Cloaked Cadet, the only
  -- printing of this written form (Scryfall o:"counters are put on one or more",
  -- 2026-08-25), draws a card and names none of them; a printing whose payload
  -- said "it" would refute this, and the lint would reject it -- PermanentsDie's
  -- case above, and no member of eventBindingSlotsSometimes for its reason.
  TriggerCondition.PermanentsGetCounters {} -> Set.empty
  -- CR 400.7e's slot in its widest reading, and NOT the arm above's answer: this
  -- condition names one permanent, so the slot is honest. Auntie Ool,
  -- Cursewretch's "draw a card if you control that creature. If you don't control
  -- it, its controller loses 1 life" is what reads it; Wickersmith's Tools, the
  -- printing that landed the constructor, reads nothing off the creature and is
  -- unaffected by the slot being available.
  TriggerCondition.PermanentGetsCounters {} -> Set.singleton Binding.became
  -- CR 709.5h names the permanent and the half, and CR 113.7a's source slot
  -- already names the permanent. The HALF is not bound: no printing says "that
  -- door", so there is nothing for a payload to read it as.
  TriggerCondition.SelfHalfUnlocked _ -> Set.empty
  -- CR 709.5i names a permanent the bearer does not have to be, so a slot for it
  -- would be honest -- but no printing reads one ("each opponent loses 1 life"
  -- names nothing about the Room), and eventBindings stamps nothing for this
  -- condition, so claiming one would promise a slot that is never filled.
  TriggerCondition.RoomFullyUnlocked _ -> Set.empty
  -- The INTERSECTION, because this function answers the guaranteed FLOOR: a slot
  -- named here has to be bound for every event the condition admits, and an AnyOf
  -- admits every event any of its branches does. A UNION would promise a slot only
  -- one branch binds, and Pawl.Engine.Resolve would look it up on an event the
  -- other branch matched and silently do nothing.
  --
  -- Set.empty for the empty list, which is the floor read literally: an AnyOf
  -- with no branches matches no event, so there is no event for which a slot
  -- could fail to be bound -- but there is also no payload that could ever read
  -- one, and the empty intersection is the only answer a Set can give. No card
  -- writes one; Pawl.CardSpec's modal lint is what would notice the ability that
  -- can never fire.
  TriggerCondition.AnyOf conditions -> case fmap eventBindingSlots conditions of
    [] -> Set.empty
    slots : rest -> List.foldl' Set.intersection slots rest
  -- CR 708.7's event names the permanent and nothing else, and CR 113.7a's source
  -- slot already names it -- so this is a DELIBERATE empty rather than an arm
  -- nobody wrote. eventBindings' fallthrough would answer the same for a
  -- condition that had been forgotten, which is exactly why it is spelled out
  -- here: Pawl.TriggerSpec pins the two against each other.
  TriggerCondition.SelfTurnedFaceUp -> Set.empty
  -- The same deliberate empty, and for the same reason one step further on: CR
  -- 701.27e's event names the permanent that transformed, which CR 113.7a's
  -- source slot already names, and the face it turned into is a NAME rather than
  -- an object for a slot to hold.
  TriggerCondition.SelfTransformedInto _ -> Set.empty
  -- A deliberate empty too, and the arm below is where the difference would
  -- show if a card asked for one: Cult of the Waxing Moon's payload names the
  -- permanent that turned over nowhere, so there is no subject to claim a slot
  -- for. Norn's Inquisitor's "put a +1/+1 counter on it" is what would take CR
  -- 400.7e's slot, exactly as Pine Walker does below.
  TriggerCondition.PermanentTransforms _ -> Set.empty
  -- The SAME event read by a bystander, and here the answer is NOT empty. CR
  -- 113.7a's source slot names the WATCHER rather than the permanent that turned
  -- over, so the subject needs a slot of its own, and Pine Walker's "untap that
  -- creature" is the printing that reads it. The zone-change slot, widened; see
  -- eventBindings' arm for why it is that slot and not a new one.
  --
  -- ALWAYS bound and never sometimes, which is what Binding.became's own contract
  -- demands: GameEvent.TurnedFaceUp carries one ObjectId unconditionally, so
  -- unlike CR 400.7e's hidden-destination case (eventBindingSlotsSometimes)
  -- there is no shape of this event that withholds it.
  TriggerCondition.PermanentTurnedFaceUp _ -> Set.singleton Binding.became
  -- A deliberate empty: Valeron Wardens draws a card and names no "it", so there
  -- is no subject to claim a slot for. The arm above is the worked example of what
  -- a card reading the designated permanent would take -- CR 400.7e's slot, since
  -- the event names one object and CR 113.7a's source names the watcher.
  TriggerCondition.PermanentBecomesDesignated {} -> Set.empty
  -- Empty too: rule 702.100b's event names the creature that evolved, and that is
  -- the bearer -- Renegade Krasis says "this creature", so there is no "it" to
  -- bind that Binding.triggerSource does not already answer.
  TriggerCondition.SelfEvolves -> Set.empty
  -- NOT empty, unlike SelfEvolves above, and the pair CR 702.134c names is why:
  -- neither the mentor nor the mentored creature is the bearer, so Aegis of the
  -- Legion's "that creature" has no other name to be read under. Guaranteed given a
  -- match, as this classification has to be: every Mentored event carries both ids.
  TriggerCondition.AttachedCreatureMentors -> Set.singleton Binding.mentoredCreature
  -- CR 400.7f's `became` for the Aura's own new incarnation, which Screams from
  -- Within's payload returns, and CR 303.4b's `departedPermanent` for the
  -- enchanted creature, which Banewasp Affliction's "that creature's controller
  -- loses life equal to its toughness" reads on both clauses. The event names
  -- exactly those two permanents and each gets one slot.
  --
  -- `departedPermanent` is GUARANTEED off the event, the ordinary way: every
  -- pair matchesTrigger accepts is a battlefield-to-graveyard GameEvent.Moved
  -- whose `departed` is the host itself, so no shape of the event withholds it.
  --
  -- `became` is guaranteed on CR 704.5m rather than on the event: an Aura
  -- whose host has left the battlefield is attached to nothing, and that rule
  -- puts it into its owner's graveyard as a state-based action, which CR 117.5
  -- runs to completion before any trigger is placed. matchesTrigger's arm makes
  -- the same observation from the other side -- by the time the condition is
  -- asked, the live attachment is ALWAYS gone.
  --
  -- Two shapes escape it. An effect that sends the Aura somewhere other than its
  -- owner's graveyard in the same batch puts it where CR 400.7f cannot look --
  -- and there the rule's own answer is that the ability finds nothing, so the
  -- payload moving nothing is correct rather than the silent no-op this lint
  -- exists to catch. That is what separates this arm from
  -- SelfLeavesTheBattlefield's floor, where a BOUNCE is an ordinary printed
  -- destination and the slot's absence is an ordinary printed case that
  -- eventBindingSlotsSometimes hands to the lint instead.
  --
  -- Not implemented: the EQUIPMENT bearer, which CR 704.5n leaves standing, so
  -- CR 400.7f mints no incarnation for it and this floor over-claims. Its
  -- condition really is asked from the battlefield (Skullclamp,
  -- data/cards/skullclamp.json), and the classification is per CONDITION and
  -- cannot see the bearer's subtypes. Nothing in the pool reads the slot there:
  -- Skullclamp's payload names none, and an Equipment ability that moved ITSELF
  -- out of a zone would be pinned to that zone by CR 113.6m, whose exception is
  -- the Aura's alone (data/cards/synthetic-widowed-blade.json, proved in
  -- Pawl.ZoneTriggerSpec) (#3153).
  TriggerCondition.AttachedCreatureDies -> Set.fromList [Binding.became, Binding.departedPermanent]
  -- Empty, and for the opposite reason to the arm above: nothing MOVED, so there
  -- is no arrival for a payload to find. The tapped permanent is still the one
  -- Object.attachedTo names.
  TriggerCondition.AttachedCreatureBecomesTapped -> Set.empty
  -- Empty for SelfEvolves' reason and not for AttachedCreatureMentors' -- rule
  -- 702.149a's counter goes on the bearer, so Savior of Ollenbock's "this creature"
  -- is Binding.triggerSource and the event names nobody else.
  TriggerCondition.SelfTrains -> Set.empty
  -- CR 701.21a's event names a player and a permanent, and this claims the
  -- PLAYER: Vengeful Tracker's "deals 2 damage to them" reads the seat that
  -- sacrificed, which the eventBindings arm stamps for every match.
  --
  -- Not implemented: the sacrificed PERMANENT. This event is recorded before the
  -- move (CR 603.10a's look-back), so the id it carries is the pre-move one, and
  -- which slot a printed "exile it" should name is what #977 still holds open.
  TriggerCondition.PermanentSacrificed {} -> Set.singleton Binding.triggerPlayer
  -- CR 601.2i's spell, the object the event names and nobody the bearer already
  -- does. Guaranteed given a match for the reason CR 615.13's amount is:
  -- GameEvent.SpellCast carries an ObjectId unconditionally, so no shape of the
  -- event withholds it, and unlike SelfLeavesTheBattlefield's `became` there is
  -- no CR 400.7e proviso to fail. Presence of the Master's "counter it" is what
  -- reads it.
  --
  -- And the CASTER, whom CR 112.2 makes the spell's controller: Kambal, Consul
  -- of Allocation's "that player loses 2 life" reads it, and under that card's
  -- Opponent-scoped Filter the player is not the "you" Binding.setYou names.
  -- Guaranteed for the reason the spell is -- GameEvent.SpellCast carries a
  -- PlayerId unconditionally -- so the promise holds for every cast the Filter
  -- can admit.
  TriggerCondition.SpellCast {} -> Set.fromList [Binding.castSpell, Binding.triggerPlayer]
  -- Nothing, a deliberate empty rather than a default: the spell the event names
  -- is the BEARER, which every ability already reaches as its own source, and the
  -- caster is CR 109.5's "you", whom Binding.setYou already names. A slot would
  -- be a second name for each. eventBindings has no arm for this condition and
  -- its fallthrough answers the same.
  TriggerCondition.SelfCast -> Set.empty
  -- CR 601.2c's targeting object, guaranteed given a match: every
  -- GameEvent.BecameTarget carries a source, so unlike SelfLeavesTheBattlefield's
  -- `became` there is no shape of the event that withholds it. The CONTROLLER
  -- rule 702.21a offers the cost to takes no slot of its own -- Resolve.payerOf
  -- reads a slot bound to an object as that object's controller, which is what
  -- "unless that player pays" asks for.
  TriggerCondition.SelfBecomesTargeted _ -> Set.singleton Binding.targetingObject
  -- CR 601.2c's targeting object again, the sibling directly above one recipient
  -- over: Amulet of Safekeeping's "counter that spell or ability" reads it, and
  -- with a PLAYER targeted there is no bearer-shaped slot that would already
  -- name it. Guaranteed for the sibling's reason -- every GameEvent.BecameTarget
  -- carries a source.
  --
  -- Claimed for the condition rather than for the printing, which is what a
  -- per-CONDITION set means: Dormant Gomazoa's "you may untap this creature"
  -- reads the bearer and never this slot, and a slot promised but unread is
  -- harmless where the reverse is the failure this lint exists to catch.
  TriggerCondition.ControllerBecomesTarget {} -> Set.singleton Binding.targetingObject
  -- CR 603.3b's second class names two things and binds both: CR 113.7's Saga,
  -- which the chapter ability hangs on, under CR 400.7e's slot for the reason
  -- eventBindings' arm gives, and CR 603.3a's controller of that chapter ability
  -- under the reserved player slot. Synthetic Chronicle Warden reads both;
  -- Historian's Boon, the printed bearer, reads neither and is unaffected by the
  -- slots being available.
  --
  -- Both guaranteed given a match: matchesTrigger's arm rejects
  -- TriggerSource.Sourceless, so the record it accepts always carries an
  -- ObjectId, and every record carries a controller. Claimed for the CONDITION
  -- rather than for the printing, which is what a per-condition set means.
  TriggerCondition.SagaFinalChapterTriggers _ -> Set.fromList [Binding.became, Binding.triggerPlayer]
  -- CR 725.1's newly crowned player -- Garland, Royal Kidnapper watches an
  -- OPPONENT take the crown, so the seat is one nothing else on the ability
  -- names. Under the You relation it is also CR 109.5's "you", a second name for
  -- one player and no wrong answer; a per-CONDITION set cannot depend on the
  -- relation.
  --
  -- Unconditional: GameEvent.BecameMonarch carries a PlayerId outright.
  TriggerCondition.PlayerBecomesMonarch _ -> Set.singleton Binding.triggerPlayer
  -- Empty by decision: the permanent the event names is the one the condition's own
  -- SLOT already names, and Ray of Command's "tap it" reads that slot rather than
  -- anything the event bound. A slot for the player who GAINED control is what a
  -- card printing "that player" would earn; nothing prints one.
  TriggerCondition.LoseControlOfBound _ -> Set.empty
  -- Empty NECESSARILY rather than by choice: this condition admits no event, so
  -- there is none to read a slot out of. What a reflexive knows about the
  -- resolution that made it comes from CR 603.7c's captured environment instead,
  -- which Pawl.Engine.Resolve stamps into the entry as it is armed.
  TriggerCondition.Reflexive -> Set.empty

-- The slots eventBindings stamps for SOME of a condition's events and not for
-- all of them -- what the classification above deliberately leaves out, since a
-- floor may promise only what every admitted event supplies.
--
-- ONE customer, the card lint, which unions this with the floor. A slot bound
-- for some events is a slot a card may legitimately name, because CR 400.7e is
-- itself the rule that withholds it ("if that zone is a public zone"): the
-- payload finding nothing for the other destination is what the rule
-- PRESCRIBES, not the silent no-op the lint exists to catch. Every other reader
-- wants the floor, a slot bound sometimes being no guarantee at all.
--
-- Two arms and no more, which is a fact about eventBindings above rather than a
-- convenience: CR 400.7e's public-zone proviso is the only guard there that
-- turns on the EVENT's own shape and that a match does not already settle, and
-- it stands on exactly these two conditions (CR 603.6c admits every destination
-- for both). The other two conditional arms turn on GAME STATE instead --
-- AttachedCreatureDies on CR 400.7f's arrival and PermanentReturnedToHand on CR
-- 608.2h's record -- and the floor claims both outright, each for the reasons
-- its own arm gives.
--
-- A WILDCARD here where eventBindingSlots forbids one, the asymmetry being which
-- way each default fails. Defaulting a new condition to "binds nothing" there
-- would silently un-lint a slot it does bind; defaulting to "nothing extra" here
-- only makes the lint STRICTER, and a card rejected at the corpus sweep fails
-- loudly. Pinned even so, by Pawl.LeavesTriggerSpec's "CR 603.2 eventBindingSlots
-- and eventBindingSlotsSometimes together name every key eventBindings stamps",
-- which unions where the floor's pin intersects.
eventBindingSlotsSometimes :: TriggerCondition -> Set.Set SlotName.SlotName
eventBindingSlotsSometimes cond = case cond of
  -- CR 400.7e's proviso, read against CR 400.2's split: a death binds `became`
  -- and a bounce does not, and CR 603.6c admits both, so the floor is empty
  -- while a card naming the slot is correct for every destination -- it acts on
  -- the card for a public one and does nothing for a hidden one, which is the
  -- whole of what the rule says. data/cards/synthetic-persistent-roaches.json is
  -- the reader.
  TriggerCondition.SelfLeavesTheBattlefield -> Set.singleton Binding.became
  -- The bystander reading of that same arm, withheld by the same guard for the
  -- same rule. Its floor holds CR 603.10a's departed permanent instead, so
  -- unlike the arm above this condition is not silent about the departure --
  -- nothing in data/cards/ names the arrival under it.
  TriggerCondition.PermanentLeavesTheBattlefield _ -> Set.singleton Binding.became
  _ -> Set.empty
