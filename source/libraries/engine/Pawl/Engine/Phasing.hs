-- | CR 702.26, phasing, and the CR 502.1 / 703.4a turn-based action that is its
-- whole engine: immediately after the untap step begins, every phased-in
-- permanent with phasing the active player controls phases out, and every
-- phased-out permanent that player controlled when it phased out phases in.
--
-- Pawl.Engine.Daytime's sibling in shape as much as in position: both are a
-- keyword (CR 702.26 here, CR 702.145 there) whose entire content is a rule the
-- untap step runs, so neither mints an ability and Pawl.Engine.Keyword answers
-- both with []. Kept out of Pawl.Engine.Engine for the reason Pawl.Engine.Saga
-- gives: that module owns WHEN a turn-based action runs, not what one means.
--
-- The state this module maintains is GameState.phasedOut, and the shape of that
-- field is the load-bearing design call here. CR 702.26b says a phased-out
-- permanent "is treated as though it does not exist", excepting only rules that
-- specifically mention phased-out permanents. Pawl spells that by MOVING the
-- object out of GameState.battlefield into GameState.phasedOut rather than
-- flagging it in place, so every one of the fifty-odd readers that walks the
-- battlefield -- the projection, targeting, the state-based actions, combat,
-- cost payment, the trigger gatherer -- gets rule 702.26b's answer without
-- knowing phasing exists. Only the rules on the far side of the "except" name
-- the new field, and Pawl.Types.GameState.phasedOut enumerates all of them: this
-- module, CR 702.26k's leaves-the-game clause in
-- Pawl.Engine.Game.removeFromZones, CR 702.26n's reschedule below, and CR 514.2's
-- damage sweep -- which is on that list by rule and not by code, since
-- Pawl.Engine.Damage.removeAllDamage clears every object rather than every
-- permanent and so covers a phased-out one without naming it.
--
-- The object itself does NOT move zones, which is CR 702.26d: Object.zone stays
-- Zone.Battlefield, nothing goes through Pawl.Engine.Event's zone-change funnel,
-- and so no zone-change ability triggers and no object gets a new incarnation.
-- Counters and tokens ride along untouched for the same reason -- rule 702.26d
-- names both -- and marked damage rides along because this module writes two
-- Set/Map memberships and nothing else. Rule 702.26d does NOT mention damage;
-- what governs it is CR 514.2, which sweeps phased-out permanents along with the
-- rest and so leaves no mark to survive a turn.
--
-- CR 702.26g's indirect half is the second thing this module maintains, and it
-- is why GameState.phasedOut holds a Pawl.Types.PhasedOut rather than a bare
-- player: a row says which of rule 702.26's schedules its permanent is on, and
-- only the direct ones are read by CR 702.26a's phase-in half. Going OUT, the
-- attachment is not touched by either half -- CR 702.26i needs Object.attachedTo
-- intact -- and nothing clears it meanwhile: Pawl.Engine.Sba's CR 704.5n/704.5p
-- detach sweep classifies only battlefield permanents and so cannot see a
-- phased-out one, and Pawl.Engine.Event's zone-change funnel is what CR 702.26d
-- keeps phasing out of. Coming back, `hostRemains` is rule 702.26i's own
-- condition and the one thing that does clear it.
--
-- Either of two things starts a phase-out, and `phaseOutSet` is both of their
-- one performer: CR 502.1's turn-based action, and Effect.PhaseOut -- Reality
-- Ripple's "target artifact, creature, or land phases out" -- resolved by
-- Pawl.Engine.Resolve. The effect mints no event and goes through no funnel,
-- which is CR 702.26d.
--
-- CR 702.26n's second sentence is the third thing this module maintains, and the
-- one schedule that is not read off the untap step's own player: a permanent
-- that phased out under a player who has since LEFT phases in "during the next
-- untap step after that player's next turn would have begun". `orphanSchedule`
-- is the moment CR 800.4k decides that turn does not begin, and the
-- PhasedOut.Orphaned row it writes is what `phasingIn` then takes at whatever
-- untap step comes next. Rule 702.26n's first sentence needs nothing here:
-- CR 702.26k's clause is Pawl.Engine.Game.removeFromZones, and CR 800.4a's exile
-- clause is vacuous for a phased-out permanent, which no battlefield walk finds.
--
-- CR 702.26f, the other continuous-effect consequence of being gone, needs
-- nothing here: a "for as long as" duration (CR 611.2b) is a Condition counting
-- the battlefield, so phaseOut's Set.delete below is what ends it, and
-- Pawl.Engine.Expiry.sweepConditional DELETES rather than suspends, which is
-- rule 702.26f's second sentence. Pawl.PhasingSpec's "CR 702.26f a
-- for-as-long-as duration ends when its permanent phases out" is the proof.
--
-- Not implemented: CR 702.26e for the three arms of
-- Pawl.Engine.Projection.affects that carry no battlefield conjunct (#1866).
module Pawl.Engine.Phasing where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PhasedOut as PhasedOut
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

-- | CR 502.1 / 703.4a / 702.26a: the untap step's first turn-based action, for
-- the player whose untap step it is.
--
-- "This all happens simultaneously" is why both halves are read off the SAME
-- state before either is written. It would be observable even with one set
-- empty: phasing out first and then asking what to phase in would find the
-- permanents this very action just removed and put them straight back, so a
-- creature with phasing would never actually leave.
--
-- Nothing triggers here (CR 702.26d), nothing chooses anything, and no
-- state-based action is checked -- CR 502.4 gives no player priority during the
-- untap step, so the next check is the upkeep's. That is what lets this be a
-- pure GameState -> GameState rather than a Game action.
phasingEvent :: PlayerId -> GameState -> GameState
phasingEvent pid gs =
  let returning = phasingIn pid gs
   in foldr (phaseIn pid) (phaseOutSet pid (Set.fromList (phasingOut pid gs)) gs) returning

-- | CR 702.26b: `hosts` phase out, and -- CR 702.26g -- so does everything
-- attached to them. The whole of "phasing out" for both of the two things that
-- can start it: CR 502.1's turn-based action above, and an effect that says so
-- (Pawl.Engine.Resolve's Effect.PhaseOut arm). Shared rather than duplicated
-- because CR 702.26g's closure and CR 702.26h's tie-break are the same rules
-- whichever asked, and two copies could disagree about them.
--
-- `fallback` is the player a row names when the projection can no longer place a
-- permanent -- the active player for the turn-based action, the resolution's
-- controller for an effect. Unreachable either way: everything in `leaving` is on
-- the battlefield at this moment, which is what makes heldBy answer.
--
-- Takes the SET in one call, and must: rules 702.26g and 702.26h ask whether a
-- permanent's host is leaving in this same event, so a per-victim call could not
-- tell an Equipment whose creature is also going from one whose creature is
-- staying.
phaseOutSet :: PlayerId -> Set.Set ObjectId -> GameState -> GameState
phaseOutSet fallback hosts gs =
  let leaving = draggedAlong hosts gs
      -- CR 702.26h: an object that would phase out both ways just phases out
      -- indirectly, which is exactly "its host is leaving too" -- so this is
      -- the rule and not a tie-break invented for it. It is also CR 702.26g for
      -- everything the closure added, the two rules being one expression here.
      -- Rule 702.26h's own half -- an object named by the effect AND dragged in
      -- the same event -- is what makes the host test come FIRST rather than
      -- second, and Pawl.PhasingSpec's "CR 702.26h an object named AND dragged
      -- phases out indirectly" is the case that proves it, Clever Concealment
      -- naming a creature and its own Equipment in one announcement.
      status oid
        | maybe False (`Set.member` leaving) (hostOf oid gs) = PhasedOut.Indirectly (heldBy fallback oid gs)
        -- CR 702.26a schedules the return by who controlled the permanent when it
        -- phased out, which is not necessarily who asked: Reality Ripple aimed at
        -- an opponent's creature phases it back in at THAT player's untap step.
        -- For the turn-based action the two coincide, phasingOut having filtered
        -- on control.
        | otherwise = PhasedOut.Directly (heldBy fallback oid gs)
   in foldr (\oid -> phaseOut (status oid) oid) gs (Set.toAscList leaving)

-- CR 702.26a's first half: the phased-in permanents with phasing this player
-- controls.
--
-- The PROJECTED keywords, never the printed ones, for the reason
-- Pawl.Engine.Speed.startingEngines gives -- an effect may grant phasing, and CR
-- 613.1f's Humility takes it away. CR 702.26p makes multiple instances
-- redundant, which is why membership answers this rather than the count the
-- projection carries.
--
-- Ascending, so the writes are deterministic.
phasingOut :: PlayerId -> GameState -> [ObjectId]
phasingOut pid gs =
  let mine oid =
        Projection.hasKeyword Keyword.Phasing oid gs
          && Projection.controllerOf oid gs == Just pid
   in filter mine (Set.toAscList (GameState.battlefield gs))

-- CR 702.26g: `hosts`, plus every Aura or Equipment attached to one of them,
-- plus every Aura or Equipment attached to one of THOSE, and so on.
--
-- The closure is the rule taken at its word twice over: an Aura enchanting an
-- Aura that enchants a phasing creature is attached to a permanent that phases
-- out, so it phases out too. No board in the pool builds that chain; the
-- fixpoint costs one extra pass on the ones that do not.
--
-- Guarded on the PROJECTED subtypes, because rule 702.26g names Auras,
-- Equipment and Fortifications rather than "whatever is attached". Anything
-- else that is somehow attached is illegal by CR 704.5p and stays behind for
-- Pawl.Engine.Sba to detach. Subtype.Fortification has no constructor to name,
-- so that third clause is unreachable rather than elided.
draggedAlong :: Set.Set ObjectId -> GameState -> Set.Set ObjectId
draggedAlong hosts gs =
  let attachments = Set.filter (isAttachment gs) (GameState.battlefield gs)
   in closeOver attachments hosts gs

-- The transitive closure of `hosts` under "is attached to", taking riders only
-- from `candidates`. Shared by both halves of rule 702.26g -- the phase-out
-- draws its candidates from the battlefield, the phase-in from the indirect
-- rows -- so the two cannot disagree about what hangs off a permanent.
closeOver :: Set.Set ObjectId -> Set.Set ObjectId -> GameState -> Set.Set ObjectId
closeOver candidates hosts gs =
  let riders =
        Set.filter
          (\oid -> maybe False (`Set.member` hosts) (hostOf oid gs))
          (Set.difference candidates hosts)
   in if Set.null riders then hosts else closeOver candidates (Set.union hosts riders) gs

isAttachment :: GameState -> ObjectId -> Bool
isAttachment gs oid =
  let subtypes = Projection.subtypesOf oid gs
   in Set.member Subtype.Aura subtypes || Set.member Subtype.Equipment subtypes

-- What a permanent is attached to, when that is an object. CR 702.26g reaches
-- only object hosts: an Aura enchanting a PLAYER (CR 702.5d) has no permanent to
-- go with, and Recipient.objectOf answering Nothing is that sentence.
hostOf :: ObjectId -> GameState -> Maybe ObjectId
hostOf oid gs = Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf

-- Who controls `oid` right now, for the row about to be written. Live, because
-- the object is still on the battlefield at this moment -- CR 702.26e only takes
-- the answer away once it is gone, which is why the row stores it at all. The
-- fallback is unreachable for the same reason; `pid` is the nearest right
-- answer, being the player whose phasing event this is.
heldBy :: PlayerId -> ObjectId -> GameState -> PlayerId
heldBy pid oid gs = Maybe.fromMaybe pid (Projection.controllerOf oid gs)

-- CR 702.26a's second half: the phased-out permanents that phased out under this
-- player's control, plus -- CR 702.26g -- the ones that phased out indirectly
-- while attached to them.
--
-- Reads the stored player and not Pawl.Engine.Projection.controllerOf, because
-- rule 702.26a asks who controlled the permanent WHEN IT PHASED OUT, and CR
-- 702.26e has taken the live answer away in the meantime -- a phased-out
-- permanent is not in the set of objects a continuous effect affects.
--
-- Only the DIRECT rows are on rule 702.26a's schedule. An indirect row's stored
-- player is not a schedule at all (CR 702.26g: it "won't phase in by itself"),
-- so it rides back on the closure instead -- which is why an Aura one player
-- controls comes back with an opponent's creature and at that opponent's untap
-- step, not at its own controller's.
--
-- No keyword test on either side. A permanent that phased out because an effect
-- said so has no phasing ability, and CR 702.26a still phases it back in; the
-- keyword decides who LEAVES, never who returns.
--
-- The orphaned rows join the direct ones WHOEVER'S untap step this is, which is
-- CR 702.26n's own schedule: the turn their player would have begun did not
-- (CR 800.4k), so the rule names the next untap step there is rather than one
-- belonging to anybody. They are unioned into `onSchedule` before the closure
-- rather than beside it, so an Aura that phased out indirectly on an orphaned
-- host still rides back with that host (CR 702.26g).
phasingIn :: PlayerId -> GameState -> [ObjectId]
phasingIn pid gs =
  let rows = GameState.phasedOut gs
      onSchedule = Map.keysSet (Map.filter (\row -> row == PhasedOut.Directly pid || isOrphaned row) rows)
      indirect = Map.keysSet (Map.filter isIndirect rows)
   in Set.toAscList (closeOver indirect onSchedule gs)

isIndirect :: PhasedOut.PhasedOut -> Bool
isIndirect status = case status of
  PhasedOut.Directly _ -> False
  PhasedOut.Indirectly _ -> True
  -- CR 702.26n reschedules a DIRECT row and changes nothing else about it.
  PhasedOut.Orphaned _ -> False

isOrphaned :: PhasedOut.PhasedOut -> Bool
isOrphaned status = case status of
  PhasedOut.Directly _ -> False
  PhasedOut.Indirectly _ -> False
  PhasedOut.Orphaned _ -> True

-- | CR 702.26n's second sentence, at the moment CR 800.4k decides `pid`'s turn
-- does not begin: every row on rule 702.26a's schedule for a player who has left
-- is moved onto rule 702.26n's, and phases in at the next untap step that runs.
--
-- Called from Pawl.Engine.Engine's two CR 800.4k branches -- the seat walk and a
-- departed player's spent extra turn -- which is where "would have begun" is
-- decided, and is why this function does not test whether `pid` has left: its
-- callers are already on the branch that answered that.
--
-- The INDIRECT rows are left alone, and rule 702.26g is why: such a row is not a
-- schedule at all, so there is nothing to reschedule -- it rides back with its
-- host, whose own row this function moves if that host is orphaned too.
orphanSchedule :: PlayerId -> GameState -> GameState
orphanSchedule pid gs =
  let reschedule row = if row == PhasedOut.Directly pid then PhasedOut.Orphaned pid else row
   in gs {GameState.phasedOut = Map.map reschedule (GameState.phasedOut gs)}

-- CR 702.26b: status becomes "phased out", and -- the rule's own last sentence,
-- restating CR 506.4 -- the permanent is removed from combat.
--
-- Game.removeFromCombat is CR 506.4's one performer, so a creature that phases
-- out mid-combat stops being an attacking, blocking, blocked or unblocked
-- creature exactly as one that left the battlefield would. It is called even
-- outside combat, where it is a no-op on an id the record does not name.
--
-- Object.attachedTo is NOT cleared, which is CR 702.26i and CR 702.26g's
-- "phases in along with the permanent it's attached to" at once: the attachment
-- has to survive the trip for either sentence to be answerable when the
-- permanent comes back. Nothing else clears it meanwhile -- see the module
-- comment.
phaseOut :: PhasedOut.PhasedOut -> ObjectId -> GameState -> GameState
phaseOut status oid gs =
  Game.removeFromCombat
    oid
    gs
      { GameState.battlefield = Set.delete oid (GameState.battlefield gs),
        GameState.phasedOut = Map.insert oid status (GameState.phasedOut gs)
      }

-- CR 702.26c: status becomes "phased in", and "the game once again treats it as
-- though it exists".
--
-- The player is redundant on this side -- the row is deleted whole, and which
-- player it named decided nothing here -- and is taken anyway so the pair reads
-- as one operation. It is not the returning permanent's controller either:
-- CR 702.26g brings an indirectly phased-out Aura back at the untap step of the
-- player whose creature it is on, which need not be the player who controls the
-- Aura.
--
-- No combat counterpart to phaseOut's removal: CR 506.4 has no clause putting
-- anything back, and the untap step is not a combat phase.
--
-- CR 702.26i is the one thing that comes back CHANGED: an attachment that phased
-- out DIRECTLY phases in attached only if what it was attached to is still there,
-- and unattached otherwise. See hostRemains.
phaseIn :: PlayerId -> ObjectId -> GameState -> GameState
phaseIn _ oid gs =
  let -- CR 702.26i names only the DIRECT rows, and the restriction is the rule's
      -- and not a shortcut: an indirect row (CR 702.26g) comes back on its host's
      -- own schedule, in the same event as that host, so asking whether the host
      -- is still there would be asking about a board mid-write.
      unattaching = case phasedOutStatus oid gs of
        Just (PhasedOut.Directly _) -> not (hostRemains oid gs)
        Just (PhasedOut.Indirectly _) -> False
        -- An orphaned row IS a direct row (CR 702.26n reschedules it and
        -- nothing more), so rule 702.26i names it and it takes the same test.
        Just (PhasedOut.Orphaned _) -> not (hostRemains oid gs)
        Nothing -> False
      detach o = o {Object.attachedTo = Nothing}
   in gs
        { GameState.battlefield = Set.insert oid (GameState.battlefield gs),
          GameState.phasedOut = Map.delete oid (GameState.phasedOut gs),
          GameState.objects =
            if unattaching
              then Map.adjust detach oid (GameState.objects gs)
              else GameState.objects gs
        }

-- CR 702.26i's condition: is the object or player this permanent was attached to
-- when it phased out still there for it to phase in attached to?
--
-- Not guarded on the permanent being an Aura, Equipment or Fortification, the way
-- draggedAlong's outward trip is. Rule 702.26i names those three, and CR 704.5p
-- makes anything ELSE that is somehow attached illegal -- so detaching it is the
-- same answer rule 704.5p would reach, and asking Pawl.Engine.Projection for the
-- subtypes of a permanent that is not on the battlefield yet cannot answer at all.
--
-- "Still in the same zone" is read off Object.zone rather than off
-- GameState.battlefield membership, and the difference is a rule: CR 702.26d makes
-- phasing not a zone change, so a host that has ALSO phased out is still in the
-- battlefield zone and its Equipment comes back attached to it. A host that truly
-- left is gone from GameState.objects entirely (CR 400.7 mints a new object for
-- the destination), which is the Nothing this reads as "not there".
--
-- Attached to nothing at all answers True: rule 702.26i has nothing to say about
-- it, and there is no attachment to lose.
--
-- The PLAYER arm is rule 702.26i's "or that player is still in the game", and
-- Clever Concealment is what reaches it: a permanent attached to a player is an
-- Aura (CR 702.5d), which Reality Ripple's artifact, creature or land cannot
-- name but which "any number of target nonland permanents you control" does.
-- Pawl.PhasingSpec's "CR 702.26i an Aura attached to a PLAYER phases in still
-- attached" is the case.
hostRemains :: ObjectId -> GameState -> Bool
hostRemains oid gs = case Game.lookupObject oid gs >>= Object.attachedTo of
  Nothing -> True
  Just recipient -> case Recipient.playerOf recipient of
    -- "or that player is still in the game" -- the roster Game.stillPlaying
    -- answers, which CR 800.4 empties as players leave.
    Just pid -> pid `elem` Game.stillPlaying gs
    Nothing ->
      maybe
        False
        ((== Zone.Battlefield) . Object.zone)
        (Recipient.objectOf recipient >>= flip Game.lookupObject gs)

-- | CR 702.26b's membership test, for the rules that are on the far side of its
-- "except for rules and effects that specifically mention phased-out
-- permanents". Nothing in the closed half should need this; a reader that walks
-- GameState.battlefield already has rule 702.26b's answer.
isPhasedOut :: ObjectId -> GameState -> Bool
isPhasedOut oid gs = Map.member oid (GameState.phasedOut gs)

-- | The whole CR 702.26b row: who controlled the permanent when it phased out,
-- and which of rule 702.26's two schedules it is on. Nothing if it is not phased
-- out. Exposed for specs.
phasedOutStatus :: ObjectId -> GameState -> Maybe PhasedOut.PhasedOut
phasedOutStatus oid gs = Map.lookup oid (GameState.phasedOut gs)

-- | Who controlled a phased-out permanent when it phased out, or Nothing if it
-- is not phased out. CR 702.26a's comparand, exposed for specs.
phasedOutUnder :: ObjectId -> GameState -> Maybe PlayerId
phasedOutUnder oid gs = fmap PhasedOut.under (phasedOutStatus oid gs)
