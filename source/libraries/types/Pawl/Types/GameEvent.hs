module Pawl.Types.GameEvent where

import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BecameAttached as BecameAttached
import qualified Pawl.Types.BecameAttacked as BecameAttacked
import qualified Pawl.Types.BecameBlocking as BecameBlocking
import qualified Pawl.Types.BecameDesignated as BecameDesignated
import qualified Pawl.Types.BecameTarget as BecameTarget
import qualified Pawl.Types.BecameUnattached as BecameUnattached
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.ClassLevelChange as ClassLevelChange
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import qualified Pawl.Types.ControlChanged as ControlChanged
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.Discarded as Discarded
import qualified Pawl.Types.Drew as Drew
import qualified Pawl.Types.HalfUnlocked as HalfUnlocked
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Mentored as Mentored
import qualified Pawl.Types.Milled as Milled
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PermanentWasSacrificed as PermanentWasSacrificed
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Transformed as Transformed
import qualified Pawl.Types.VentureMarkerEntered as VentureMarkerEntered
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | CR 608.2i: one entry of the turn-scoped record of what happened. Effects
-- that look back in time read it, so entries are APPENDED by the
-- change-and-emit funnels and never removed by a reader. Each reader keeps its
-- own watermark into GameState.events; the log itself is cleared only at turn
-- handoff.
data GameEvent
  = -- | CR 400.7: an object moved between zones. The ZoneChange is the RESOLVED
    -- (post-replacement) event, carrying the RESULTING object's id, beside a
    -- strict snapshot of the moved object as it last existed in the zone it LEFT
    -- (CR 608.2h) rather than a re-derivation from the printed card.
    Moved Moved.Moved
  | -- | CR 712.21: one of the CARDS a zone change put into a zone, other than
    -- the one the Moved event above already names. The ZoneChange's `departed`
    -- is the object that left and `to` is where this card landed, which CR
    -- 903.9c can make a different zone from the Moved event's.
    --
    -- Recorded only where one departure minted several arrivals, so CR 712.21's
    -- first clause keeps its arity -- one permanent left the battlefield, and a
    -- condition watching for that sees one Moved and no CardArrived.
    CardArrived ZoneChange.ZoneChange
  | -- | CR 120 / 510: damage was dealt. The record the CR 704.5h deathtouch
    -- state-based action reads, watermarked rather than drained.
    DamageDealt DamageEvent.DamageEvent
  | -- | CR 615.13: a prevention effect was applied to one or more SIMULTANEOUS
    -- damage events and prevented some or all of that damage. One entry per
    -- prevention effect per batch, that rule's own arity, and never derivable
    -- from DamageDealt above -- CR 615.6 makes a fully prevented event never
    -- happen. Carries the applying instance's CR 614.5 identity, which is what a
    -- card saying "prevented this way" compares against.
    DamagePrevented DamagePrevented.DamagePrevented
  | -- | CR 603.2b: a phase or step began, on whose turn (the active player). What
    -- both an "at the beginning of each end step" step trigger and a CR 603.7
    -- delayed ability match against.
    StepBegan StepBegan.StepBegan
  | -- | CR 601.2i: a player cast a spell -- the caster, and the spell's STACK
    -- incarnation (CR 601.2a), beside a strict snapshot of that object as the
    -- spell became cast (CR 608.2h) for a CR 608.2i look-back reader. The cast is
    -- the event, so a countered spell still counts, and it is not derivable from
    -- the Moved event the same cast records: CR 707.10's copy reaches the stack
    -- without being cast.
    SpellCast SpellWasCast.SpellWasCast
  | -- | CR 725.1: a player became the monarch. What Palace Jailer's exile duration
    -- keys off, and the substrate for any future "whenever a player becomes the
    -- monarch" trigger.
    BecameMonarch PlayerId.PlayerId
  | -- | CR 726.1/726.5: a player took the initiative, recorded whether or not
    -- they already held it -- CR 726.5 re-triggers on a re-take, which is why
    -- this is not GameEvent.BecameMonarch's shape.
    TookInitiative PlayerId.PlayerId
  | -- | CR 701.9a: a card was DISCARDED, by the discarding player, naming the
    -- incarnation the card became (CR 400.7). Emitted by
    -- Pawl.Engine.Event.discard, the one funnel every discard goes through, and
    -- distinct from the Moved event the same discard records -- CR 701.9c leaves
    -- a redirected discard a discard all the same. The cause is a FIELD rather
    -- than a sibling constructor, CR 702.29a making a cycle one discard that CR
    -- 702.29d caps at one trigger.
    Discarded Discarded.Discarded
  | -- | CR 701.17a: a player MILLED cards -- one event per instruction per
    -- player, holding every card that player milled at once. Distinct from the
    -- Moved entries the same mill records, for the reason the Discarded arm above
    -- is distinct from its own: surveil (CR 701.25a) and explore (CR 701.44a) bin
    -- a card off a library without milling it.
    Milled Milled.Milled
  | -- | CR 121.1: a player DREW a card, and which of that player's draws this
    -- turn it was -- 1 for the first, counting up, which is what CR 702.94a's
    -- "the first card you've drawn this turn" asks. Recorded only for a draw that
    -- COMPLETED, CR 121.4's draw from an empty library drawing nothing, and
    -- distinct from the Moved event the same draw records (CR 121.5).
    Drew Drew.Drew
  | -- | CR 508.2b: an attacker was DECLARED -- one entry per creature the active
    -- player chose in CR 508.1's turn-based action, appended by
    -- Pawl.Engine.Combat.declareAttackers alone, which is what makes CR 508.3a's
    -- exclusion of a creature put onto the battlefield attacking hold.
    --
    -- The PlayerId is CR 508.5's defending player FOR THIS CREATURE, stamped at
    -- declaration time because the planeswalker and battle forms of that rule
    -- need the board and Pawl.Engine.Event.Binding.eventBindings has none; CR 508.5a
    -- makes it one player rather than a set. The Natural is how many creatures
    -- the same declaration named, CR 506.5's "the only creature declared as an
    -- attacker", carried because CR 702.83b scopes "alone" to a combat phase
    -- where this log is cleared per turn.
    AttackerDeclared AttackerDeclared.AttackerDeclared
  | -- | CR 508.3b: a player, planeswalker or battle WAS ATTACKED, and by whom --
    -- one event per DISTINCT target the CR 508.1 declaration named, appended by
    -- Pawl.Engine.Combat.declareAttackers alone. AttackerDeclared's grouping
    -- sibling: three creatures sent at one player are three of that event and one
    -- of this, and Pawl.Engine.Event.matchesTrigger sees one event at a time so
    -- the arity is built here. Carries CR 508.3b's target rather than CR 508.5's
    -- defending player, and the attacking player beside it for CR 508.3e.
    BecameAttacked BecameAttacked.BecameAttacked
  | -- | CR 508.3d: a player DECLARED ATTACKERS -- ONE event per CR 508.1
    -- declaration, naming the attacking player, appended by
    -- Pawl.Engine.Combat.declareAttackers alone. The third arity a declaration
    -- records, AttackerDeclared (CR 508.3a) and BecameAttacked (CR 508.3b) being
    -- the others, and recorded only where the declaration named at least one
    -- creature.
    AttackersDeclared PlayerId.PlayerId
  | -- | CR 509.1g: a creature BECAME A BLOCKING CREATURE, naming it and one
    -- attacking creature it is blocking -- one event per PAIR, CR 509.3b's arity.
    -- Two appenders: Pawl.Engine.Combat.declareBlockers (CR 509.1a) and
    -- Pawl.Engine.Combat.putOntoBattlefieldBlocking (CR 509.4).
    --
    -- Which producer it was rides on the payload's putOntoBattlefield, because CR
    -- 509.4 makes CR 509.3b's "blocks a creature" and CR 509.3d's "becomes
    -- blocked by a creature" disagree about it, and Combat.blockers cannot tell
    -- them apart. The payload's blockersBefore is the only record of what was
    -- blocking the attacker before this arrival joined them.
    BecameBlocking BecameBlocking.BecameBlocking
  | -- | CR 509.1h: an attacking creature BECAME a blocked creature -- one event
    -- per attacker, CR 509.3c's arity where BecameBlocking above has CR 509.3b's.
    -- Three appenders: Pawl.Engine.Combat.declareBlockers,
    -- Pawl.Engine.Combat.becomeBlocked (Effect.BecomesBlocked) and
    -- Pawl.Engine.Combat.putOntoBattlefieldBlocking, the last only where CR
    -- 509.3c's "was an unblocked creature at that time" holds.
    --
    -- Carries how many blockers there were at this becoming (CR 509.3e) rather
    -- than the blockers themselves, and carries it on the event rather than
    -- reading Combat.blockers when the condition is scanned, which CR 509.2a
    -- would make a stale read. The PlayerId is CR 508.5's defending player for
    -- this attacker, stamped for AttackerDeclared's reason; CR 702.130a's afflict
    -- reads it.
    AttackerBlocked AttackerBlocked.AttackerBlocked
  | -- | CR 509.1h's other half: an attacking creature became an UNBLOCKED
    -- creature -- one event per attacker the CR 509.1 declaration gave no
    -- blockers, appended by Pawl.Engine.Combat.declareBlockers alone. Recorded
    -- once, as the turn-based action finishes: that rule's last sentence keeps a
    -- creature blocked when every blocker leaves combat, so an emptied
    -- Combat.blockers entry is not this event.
    --
    -- The declaration is the only producer, which is no shortfall against rule
    -- 509.1h's "an effect says that it becomes blocked or unblocked": the blocked
    -- half is Pawl.Engine.Combat.becomeBlocked, and no printing states the
    -- unblocked half -- Scryfall has no card whose text makes a creature become
    -- unblocked.
    AttackerUnblocked ObjectId.ObjectId
  | -- | CR 509.1i: a creature was declared BLOCKING -- one event per blocking
    -- creature the CR 509.1 declaration named, appended by
    -- Pawl.Engine.Combat.declareBlockers alone. BecameBlocking's grouped twin,
    -- splitting a blocker's declaration by CR 509.3a against CR 509.3b. The
    -- Natural is how many attacking creatures it was declared against, CR
    -- 509.3e's number.
    --
    -- Not implemented: rule 509.3a's second sentence, an effect that causes a
    -- creature to block, which no card in the pool states (#1146). A creature put
    -- onto the battlefield blocking is not that shortfall but rule 509.3a's last
    -- sentence: Combat.putOntoBattlefieldBlocking records BecameBlocking and
    -- deliberately not this.
    BlocksDeclared BlocksDeclared.BlocksDeclared
  | -- | CR 701.20a: a player revealed a card. The log is where it has to live, CR
    -- 701.20b moving nothing, and it carries the CARD's characteristics beside
    -- the id CR 702.94a's linked miracle trigger (CR 603.11) needs to name "this
    -- card". Strict, for Moved's reason. The RevealCause is CR 702.94a's "this
    -- way".
    --
    -- Not implemented: CR 701.20a's lasting cases -- a card revealed to pay a
    -- cost, and one that stays revealed while a triggered ability it caused is on
    -- the stack -- which need a per-object flag no card in the pool asks for
    -- (#1408).
    Revealed Revealed.Revealed
  | -- | CR 701.6a: a spell was COUNTERED. Emitted by Pawl.Engine.Event.counter,
    -- the one funnel every countering goes through, and distinct from the Moved
    -- event the same removal records: CR 608.2n sends a resolved spell to the
    -- same place. Emitted only where a countering actually happened, both of that
    -- funnel's "can't be countered" gates (CR 113.6g, CR 613.11) returning first.
    SpellCountered Countering.Countering
  | -- | CR 701.6a's other subject: an ABILITY was countered. Emitted by the same
    -- funnel and carrying the same Countering, and a sibling arm rather than a
    -- widening of the one above: rule 608.2n makes a countered ability cease to
    -- exist rather than move, so there is no Moved event beside this one, and
    -- Baral, Chief of Compliance's "counters A SPELL" must not see it.
    AbilityCountered Countering.Countering
  | -- | CR 119.3: a player LOST LIFE, and how much -- greater than 0, every
    -- producer guarding its own zero so that neither CR 702.179d's speed increase
    -- nor a "whenever an opponent loses life" fires on nothing.
    --
    -- Recorded at all three places life leaves a player, which is a fact about
    -- the RULES rather than about plumbing: CR 119.3's instructed loss, CR 119.2's
    -- damage dealt to a player, and CR 119.4's life a cost or an effect has a
    -- player pay. A reader asking "did this player lose life" must find all three.
    -- LifeGained below is the sibling rather than one "life total changed" arm,
    -- the two being distinct events for triggers.
    LifeLost LifeChange.LifeChange
  | -- | CR 119.3's other direction: a player GAINED life, and how much. Greater
    -- than 0, which CR 119.9 states outright. Recorded at both places a source
    -- causes a life total to go up: CR 119.3's instructed gain, and CR 120.3f's
    -- lifelink damage, which gains its SOURCE'S CONTROLLER life.
    --
    -- Three life-total facts are not gains and record nothing here: a starting
    -- life total (CR 119.1), which no source caused; prevented damage (CR 615.6),
    -- where the life was never lost; and paying life (CR 119.4), which that rule
    -- calls losing life.
    LifeGained LifeChange.LifeChange
  | -- | CR 606.3: a LOYALTY ability of this permanent was activated -- the record
    -- that rule's once-per-permanent-per-turn limit is read out of, as a
    -- look-back read of the log rather than a stamp on the object, so a skipped
    -- untap step (CR 500.11) cannot strand it. The ObjectId is the SOURCE
    -- PERMANENT, so CR 400.7's flickered planeswalker may activate again.
    LoyaltyAbilityActivated ObjectId.ObjectId
  | -- | CR 122.6: one or more counters were PUT onto an object -- the object, the
    -- kind, and the counts of that kind on it BEFORE and AFTER. The pair rather
    -- than one amount because CR 714.2b's chapter ability asks whether the number
    -- "was less than N and became at least N", a threshold crossing neither the
    -- amount nor the total alone can answer. Before is strictly less than after,
    -- a placement that landed nothing recording no event; removal is
    -- CountersRemoved below rather than the pair reversed.
    CountersPut CounterChange.CounterChange
  | -- | Counters were REMOVED from an object -- CountersPut's mirror, shaped the
    -- same way because CR 310.12b's Siege ability asks whether the LAST counter
    -- came off. Recorded by three paths: CR 120.3h's and CR 120.3c's damage to a
    -- battle or a planeswalker, CR 704.5q's annihilation of paired counters, and
    -- Pawl.Engine.Event.removeCounters. CR 122.2's zone change is not among them
    -- -- those counters "simply cease to exist".
    CountersRemoved CounterChange.CounterChange
  | -- | CR 709.5c: a permanent was given an UNLOCKED DESIGNATION -- the
    -- permanent, who unlocked it (CR 709.5h's "a player unlocks"), and the half
    -- by name. Emitted by Pawl.Engine.Event.unlockHalves, and only where the
    -- permanent did not already have it. An event rather than a board read
    -- because CR 709.5h asks the question "regardless of whether it was given
    -- that designation while entering the battlefield or after".
    --
    -- The Bool is CR 709.5i's "fully unlocks", computed at the write rather than
    -- when a trigger is matched, and marking the WRITE and not the half so that
    -- rule's second branch fires once. Nothing is emitted for a LOCK (CR 709.5g),
    -- both rules asking about a permanent GAINING a designation.
    HalfUnlocked HalfUnlocked.HalfUnlocked
  | -- | CR 708.7: a face-down permanent was turned face up, which CR 708.8 makes
    -- a change to copiable values rather than a zone change, so no Moved event
    -- describes it. One direction only: no printed card triggers on a permanent
    -- being turned face down.
    TurnedFaceUp ObjectId.ObjectId
  | -- | CR 701.27a: a double-faced permanent TRANSFORMED. CR 701.27b makes that a
    -- different game action from TurnedFaceUp above and CR 712.18 keeps it the
    -- same object, so nothing else in this list carries it. Recorded through
    -- Pawl.Engine.Event.recordTransformed by CR 701.27a's opcode and CR
    -- 702.145c/f's day/night sweep, and only for a permanent that ACTUALLY turned
    -- -- never for daybound's "enters transformed" (CR 702.145b).
    --
    -- CR 701.28a routes convert through CR 701.27a-f, so a convert records this
    -- event and no second one; Pawl.TransformSpec's "a convert records the
    -- transform event, not one of its own" is what proves it.
    Transformed Transformed.Transformed
  | -- | A permanent GAINED THIS DESIGNATION -- CR 702.112b's renowned, CR
    -- 701.37b's monstrous or CR 701.60b's suspected. Emitted by
    -- Pawl.Engine.Resolve's Effect.Designate arm, the one place any of them is
    -- written, and only on a TRANSITION. One direction only: the rules let only
    -- suspected end (CR 701.60a), and no printed card triggers on that.
    BecameDesignated BecameDesignated.BecameDesignated
  | -- | CR 702.100b: a creature EVOLVED -- "one or more +1/+1 counters are put on
    -- it as a result of its evolve ability resolving", so the placement must
    -- actually have landed some. Distinct from the CountersPut event the same
    -- placement records: that one says +1/+1 counters arrived, this one says the
    -- evolve ability put them.
    Evolved ObjectId.ObjectId
  | -- | CR 702.134c: a creature MENTORED another. TWO ids, in the rule's own
    -- order, the second not being derivable from the first since CR 603.3d
    -- chooses rule 702.134a's target. Emitted on the ability RESOLVING and gated
    -- on nothing else, unlike Evolved above: a CR 122.6 replacement that reduces
    -- the placement to nothing still mentors.
    Mentored Mentored.Mentored
  | -- | CR 702.149c: a creature TRAINED -- "a resolving training ability puts one
    -- or more +1/+1 counters on this creature", so it sides with Evolved's gate
    -- rather than Mentored's. ONE id, rule 702.149a putting its counter on the
    -- training creature itself.
    Trained ObjectId.ObjectId
  | -- | CR 701.21a: a permanent was SACRIFICED, and by whom. Emitted by
    -- Pawl.Engine.Event.sacrifice, the one funnel every sacrifice goes through,
    -- and distinct from the Moved event the same sacrifice records: CR 700.4
    -- makes a sacrifice a death, whose zone change a destruction writes too. CR
    -- 603.10a is why the record is written BEFORE the move, naming the pre-move
    -- id, which is why Pawl.Engine.Event.Binding.eventBindings is handed the
    -- batch's arrival table to bind CR 400.7e's new object for Prowling
    -- Geistcatcher's "exile it".
    PermanentSacrificed PermanentWasSacrificed.PermanentWasSacrificed
  | -- | CR 603.3b: an ABILITY TRIGGERED -- what it hangs on (CR 113.7), the
    -- ability's controller as it triggered (CR 603.3a), and the ability itself.
    -- Appended by Pawl.Engine.Engine.placePendingTriggers before the batch is put
    -- onto the stack, which is what lets the abilities reacting to it join the
    -- SAME batch in rule 603.3b's second pass.
    --
    -- Not implemented: a CR 603.7 DELAYED ability never triggers off one of
    -- these entries, Engine.reactions re-scanning only the event triggers
    -- (#1026).
    AbilityTriggered AbilityTriggered.AbilityTriggered
  | -- | A permanent's CONTROLLER CHANGED: the permanent, the player who
    -- controlled it when the game last looked, and the player who controls it
    -- now. What Ray of Command's "when you lose control of the creature" matches
    -- (CR 603.7), and both players because that condition asks about the player
    -- control left.
    --
    -- SAMPLED into being by Pawl.Engine.Engine.sampleControl rather than emitted
    -- by an action, control being derived in CR 613.1b's layer 2 with no
    -- resolution to announce the change. Battlefield-scoped, since that is where
    -- the sample is taken.
    ControlChanged ControlChanged.ControlChanged
  | -- | CR 309.4c \/ 701.49a\/b: a player moved their venture marker into a room
    -- -- the player, the dungeon card it is on, and which room. Minted by
    -- Pawl.Engine.Dungeon.venture on ENTERING a dungeon as well as on advancing
    -- within one, CR 701.49a putting the marker on the topmost room. The dungeon
    -- rides beside the room because the index alone names nothing.
    VentureMarkerEntered VentureMarkerEntered.VentureMarkerEntered
  | -- | CR 309.7: a player completed a dungeon, as the dungeon card they own was
    -- removed from the game -- CR 309.6's owner, recorded by
    -- Pawl.Engine.Dungeon.remove. No dungeon on the payload: rule 309.7 states
    -- the fact and names no card, and Scryfall o:"completed" (2026-08-31) returns
    -- one printing naming a dungeon, Acererak the Archlich, which reads
    -- Pawl.Types.Player.completedDungeonNames rather than the moment.
    DungeonCompleted PlayerId.PlayerId
  | -- | CR 601.2c: an object or player BECAME A TARGET of a spell or ability, the
    -- payload's `targeted` being a Recipient since CR 115.1 makes a player a
    -- target in its own right. One event per targeted recipient, as the targets
    -- are announced, plus CR 707.10c's copy, which reaches the stack with targets
    -- that were never announced. Nothing fires for an object merely CHOSEN at
    -- resolution (CR 115.10a).
    BecameTarget BecameTarget.BecameTarget
  | -- | CR 701.3a: an Aura, Equipment or Fortification BECAME ATTACHED to an
    -- object or player -- CR 303.4b's "enchants" for an Aura, the payload's
    -- `host` a Recipient because CR 702.5a's enchant ability can name a player.
    -- Emitted for rule 701.3's move and for a permanent that ARRIVES attached (CR
    -- 608.3c, CR 303.4f), one event for both routes.
    --
    -- Not emitted by Pawl.Engine.Phasing, which is CR 702.26j: a permanent
    -- phasing in keeps the Object.attachedTo it phased out with, so there is no
    -- attachment to record. Pawl.PhasingSpec holds that fence.
    BecameAttached BecameAttached.BecameAttached
  | -- | CR 701.3d: an Aura, Equipment or Fortification BECAME UNATTACHED from
    -- the object or player it was attached to. BecameAttached's mirror, emitted
    -- by every route rule 701.3d lists -- Pawl.Types.BecameUnattached enumerates
    -- them, and Pawl.Engine.Event.unattach is the funnel.
    --
    -- Not emitted by Pawl.Engine.Phasing, which is CR 702.26j, the same fence
    -- BecameAttached carries.
    BecameUnattached BecameUnattached.BecameUnattached
  | -- | A permanent left the GAME rather than the battlefield, by CR 800.4a's
    -- road (its owner having left) or CR 729.4a's (a subgame having brought the
    -- card in). The ObjectId is the id it had while it existed, which is the key
    -- its CR 608.2h last known information is filed under. Not a Moved event:
    -- there is no destination zone to name, and an invented one would answer CR
    -- 700.4's "dies" with a fiction.
    --
    -- Emitted for a PHASED-IN BATTLEFIELD permanent and nothing else, by both
    -- roads -- CR 603.6c's second trigger event for the first, and CR 702.26b for
    -- the second, to which rule 729.4a states no exception.
    --
    -- Not implemented: CR 729.4a's wider ask, abilities that trigger on objects
    -- leaving a main-game ZONE, so a card a subgame took out of a hand,
    -- graveyard, library or exile does not enter this log (#2463).
    LeftTheGame ObjectId.ObjectId
  | -- | CR 701.22d: a player completed CR 701.22a's scry. Recorded after the
    -- reorder and even where nothing could move, that rule covering the actions
    -- that were impossible; CR 701.22b's zero is no scry at all. Nothing else in
    -- the log says a scry happened, the reorder crossing no zone boundary.
    Scried PlayerId.PlayerId
  | -- | CR 701.25d, Scried's twin, with CR 701.25c as its own non-event. Distinct
    -- from the Moved entries the graveyard half of CR 701.25a records and from
    -- Milled above, a surveil binning a card without milling it -- a reader
    -- folding either would miss a surveil that binned nothing.
    Surveiled PlayerId.PlayerId
  | -- | CR 706.1: a player rolled a die -- the resolving ability's controller,
    -- recorded by Pawl.Engine.Resolve's Effect.RollDie arm after CR 706.2's
    -- result is settled and bound. No result and no die kind: a reader wanting
    -- the number takes it from Pawl.Types.RollDie's own slot, CR 706.7's planar
    -- die being ignored by every effect reading a numerical result while still
    -- firing this trigger (#934).
    --
    -- ONE ENTRY PER INSTRUCTION and not per die, however many CR 706.1's count
    -- threw, where CoinFlipped records one per coin: the condition reading this
    -- event is worded "one or more dice", which scopes it to the instruction.
    DiceRolled PlayerId.PlayerId
  | -- | CR 716.2a: a permanent's class level BECAME something -- the level BEFORE
    -- and the level AFTER, CountersPut's shape and for CR 714.2b's reason, since
    -- "becomes level N" is a threshold crossing. Recorded by Pawl.Engine.Resolve's
    -- Effect.SetClassLevel arm, the only writer of Object.classLevel, and only on
    -- a TRANSITION. No cause tag: CR 716.2b makes the level a designation of the
    -- permanent rather than of the effect.
    ClassLevelSet ClassLevelChange.ClassLevelChange
  | -- | CR 702.170a: a card became a plotted card. The ObjectId is the card AS IT
    -- SITS IN EXILE, CR 400.7's new object being the one bearing the "when this
    -- card becomes plotted" ability, and the event is distinct from the Moved
    -- entry the same exile records. Both routes record it through
    -- Pawl.Engine.Plot.becomePlotted: CR 116.2k's special action and CR 702.170c's
    -- spells and abilities.
    Plotted ObjectId.ObjectId
  | -- | CR 701.44b: this permanent completed CR 701.44a's explore, recorded after
    -- the whole process and "even if some or all of those actions were
    -- impossible". The explorer's id alone, CR 701.44c making last known
    -- information answer who controlled it. Distinct from the Revealed, Moved and
    -- CountersPut entries the explore's own steps record, none of which says an
    -- explore completed.
    Explored ObjectId.ObjectId
  | -- | CR 701.43a: this permanent was EXERTED, recorded by
    -- Pawl.Engine.Combat.declareAttackers at CR 508.1g and watched by CR 701.43d's
    -- linked "when you do" trigger. The permanent's id alone; the PROHIBITION
    -- outlives the moment and a control change can separate the two, so
    -- Object.exertedBy records the player instead.
    --
    -- Distinct from the AttackerDeclared event the same step records, which every
    -- attacker writes where this one fires only when the optional cost was PAID.
    -- CR 701.43b lets a permanent be exerted more than once, so a second exert is
    -- a second event.
    Exerted ObjectId.ObjectId
  | -- | CR 701.26a: this permanent BECAME TAPPED. Appended by
    -- Pawl.Engine.Event.tap alone, the funnel every tapping route goes through,
    -- and only for a permanent that was untapped -- that rule's second sentence.
    -- CR 603.2e is the other exclusion: a permanent that ENTERS tapped never
    -- transitioned. The id alone, no card in `data/cards/` distinguishing what
    -- caused the tap.
    BecameTapped ObjectId.ObjectId
  | -- | CR 701.26b: this permanent BECAME UNTAPPED. Appended by
    -- Pawl.Engine.Event.untap and by CR 502.3's batch in
    -- Pawl.Engine.Engine.untapAll, the two roads that untap, and only for a
    -- permanent that was tapped -- that rule's second sentence. CR 603.2e is the
    -- other exclusion: a permanent that ENTERS untapped never transitioned. The
    -- id alone, BecameTapped's mirror.
    BecameUntapped ObjectId.ObjectId
  | -- | CR 106.12a: this permanent WAS TAPPED FOR MANA -- a mana ability of it
    -- whose activation cost includes {T} resolved and produced mana. Appended by
    -- Pawl.Engine.Cost.tapForManaWith alone, the funnel every mana activation
    -- goes through, after CR 405.6c's non-mana effects have run.
    --
    -- Distinct from BecameTapped above, which every tapping route writes: CR
    -- 106.12 asks what the tap was FOR, so Icy Manipulator's tap and an attack
    -- write that event and not this one. The two always co-occur here, {T}
    -- (CR 107.5) being payable only by an untapped permanent.
    --
    -- The id alone. CR 106.12a's "of a specified type" narrowing would want the
    -- produced mana here as well; no card in data/cards/ prints it.
    TappedForMana ObjectId.ObjectId
  | -- | CR 705.1: a player flipped a coin, and CR 705.2 decided whether they won
    -- it -- or left it winnerless. Recorded by both roads that flip:
    -- Pawl.Engine.Resolve's Effect.FlipCoin arm, after the outcome is settled and
    -- bound, and Pawl.Engine.Event's EntryRewrite.ChoiceByCoinFlip arm, whose only
    -- outcome is CR 705.3's. Pawl.Types.CoinFlipped says why the outcome is an
    -- optional field rather than a second constructor.
    CoinFlipped CoinFlipped.CoinFlipped
  | -- | CR 701.54d: the Ring tempted this player. Recorded by
    -- Pawl.Engine.Ring.tempt after CR 701.54a's actions, "even if some or all of
    -- those actions were impossible", so a player with no creature to choose
    -- writes one just the same. The player alone: the emblem and the Ring-bearer
    -- designation are state a reader can look at, and nothing else in the log
    -- says a temptation happened.
    RingTempted PlayerId.PlayerId
  | -- | CR 701.68d: this player blighted. Recorded by Pawl.Engine.Blight.blight
    -- once rule 701.68a's process is complete, "regardless of what events
    -- actually occurred", so a blight of zero and one whose counters a
    -- replacement kept off write one just the same -- which is why the
    -- CountersPut above cannot stand in for it. NOT written on rule 701.68b's
    -- board, where that process never runs: rule 701.68d has no counterpart to
    -- rule 701.54d's "even if some or all of those actions were impossible".
    --
    -- Not implemented: CR 701.68c's blighted creature, which is a
    -- resolution-time binding of the instructing effect rather than a payload
    -- here (#1492).
    Blighted PlayerId.PlayerId
  deriving (Eq, Ord, Show)
