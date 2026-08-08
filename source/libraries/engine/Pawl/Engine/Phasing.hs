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
-- the new field, and Pawl.Types.GameState.phasedOut enumerates all three: this
-- module, CR 702.26k's leaves-the-game clause in
-- Pawl.Engine.Game.removeFromZones, and CR 514.2's damage sweep -- which is on
-- that list by rule and not by code, since Pawl.Engine.Damage.removeAllDamage
-- clears every object rather than every permanent and so covers a phased-out one
-- without naming it.
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
-- player: a row says which of rule 702.26's two schedules its permanent is on,
-- and only the direct ones are read by CR 702.26a's phase-in half. The
-- attachment itself is NOT touched by either half -- CR 702.26i needs
-- Object.attachedTo intact -- and the only two paths that clear that field are
-- elsewhere: Pawl.Engine.Sba's CR 704.5n/704.5p detach sweep, which classifies
-- only battlefield permanents and so cannot see a phased-out one, and
-- Pawl.Engine.Event's zone-change funnel, which CR 702.26d keeps phasing out of.
--
-- WHAT IS NOT IMPLEMENTED, none of which the pool can reach:
--
--   * CR 702.26i, an Aura, Equipment or Fortification that phased out DIRECTLY
--     and phases in attached only if its host is still in the same zone (#1032).
--   * Effects that phase a permanent out (Teferi's Protection), which is the
--     effect-DSL half rather than this one (#929). Only CR 702.26a's own
--     schedule can phase anything today, so `phasedOut` is only ever written by
--     this module.
--   * CR 702.26e/f, the continuous-effect consequences of being gone (#930).
--   * CR 702.26n's second sentence: a permanent that phased out under a player
--     who has since LEFT the game phases in "during the next untap step after
--     that player's next turn would have begun", a schedule for a turn that never
--     happens (#931). Rule 702.26k's own clause -- such a permanent leaves the
--     game with its owner -- IS implemented, in Pawl.Engine.Game.removeFromZones.
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
  let leaving = draggedAlong (Set.fromList (phasingOut pid gs)) gs
      -- CR 702.26h: an object that would phase out both ways just phases out
      -- indirectly, which is exactly "its host is leaving too" -- so this is
      -- the rule and not a tie-break invented for it.
      status oid
        | maybe False (`Set.member` leaving) (hostOf oid gs) = PhasedOut.Indirectly (heldBy pid oid gs)
        | otherwise = PhasedOut.Directly pid
      returning = phasingIn pid gs
   in foldr (phaseIn pid) (foldr (\oid -> phaseOut (status oid) oid) gs (Set.toAscList leaving)) returning

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
phasingIn :: PlayerId -> GameState -> [ObjectId]
phasingIn pid gs =
  let rows = GameState.phasedOut gs
      onSchedule = Map.keysSet (Map.filter (== PhasedOut.Directly pid) rows)
      indirect = Map.keysSet (Map.filter isIndirect rows)
   in Set.toAscList (closeOver indirect onSchedule gs)

isIndirect :: PhasedOut.PhasedOut -> Bool
isIndirect status = case status of
  PhasedOut.Directly _ -> False
  PhasedOut.Indirectly _ -> True

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
phaseIn :: PlayerId -> ObjectId -> GameState -> GameState
phaseIn _ oid gs =
  gs
    { GameState.battlefield = Set.insert oid (GameState.battlefield gs),
      GameState.phasedOut = Map.delete oid (GameState.phasedOut gs)
    }

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
