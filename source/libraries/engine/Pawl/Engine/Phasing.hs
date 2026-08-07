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
-- WHAT IS NOT IMPLEMENTED, none of which the pool can reach:
--
--   * CR 702.26g-j, phasing out INDIRECTLY: an Aura, Equipment or Fortification
--     attached to a permanent that phases out goes with it and comes back with
--     it (#928). Sandbar Crocodile, the pool's only producer, is a vanilla
--     creature nothing can be attached to by any card in `data/cards/`.
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
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)

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
  let leaving = phasingOut pid gs
      returning = phasingIn pid gs
   in foldr (phaseIn pid) (foldr (phaseOut pid) gs leaving) returning

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

-- CR 702.26a's second half: the phased-out permanents that phased out under this
-- player's control.
--
-- Reads the stored player and not Pawl.Engine.Projection.controllerOf, because
-- rule 702.26a asks who controlled the permanent WHEN IT PHASED OUT, and CR
-- 702.26e has taken the live answer away in the meantime -- a phased-out
-- permanent is not in the set of objects a continuous effect affects.
--
-- No keyword test on this side. A permanent that phased out because an effect
-- said so has no phasing ability, and CR 702.26a still phases it back in; the
-- keyword decides who LEAVES, never who returns.
phasingIn :: PlayerId -> GameState -> [ObjectId]
phasingIn pid gs = Map.keys (Map.filter (== pid) (GameState.phasedOut gs))

-- CR 702.26b: status becomes "phased out", and -- the rule's own last sentence,
-- restating CR 506.4 -- the permanent is removed from combat.
--
-- Game.removeFromCombat is CR 506.4's one performer, so a creature that phases
-- out mid-combat stops being an attacking, blocking, blocked or unblocked
-- creature exactly as one that left the battlefield would. It is called even
-- outside combat, where it is a no-op on an id the record does not name.
phaseOut :: PlayerId -> ObjectId -> GameState -> GameState
phaseOut pid oid gs =
  Game.removeFromCombat
    oid
    gs
      { GameState.battlefield = Set.delete oid (GameState.battlefield gs),
        GameState.phasedOut = Map.insert oid pid (GameState.phasedOut gs)
      }

-- CR 702.26c: status becomes "phased in", and "the game once again treats it as
-- though it exists".
--
-- The player is redundant on this side -- the id is only ever drawn from the
-- rows already keyed to it -- and is taken anyway so the pair reads as one
-- operation and so a future caller cannot phase something in under the wrong
-- player by accident.
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

-- | Who controlled a phased-out permanent when it phased out, or Nothing if it
-- is not phased out. CR 702.26a's comparand, exposed for specs.
phasedOutUnder :: ObjectId -> GameState -> Maybe PlayerId
phasedOutUnder oid gs = Map.lookup oid (GameState.phasedOut gs)
