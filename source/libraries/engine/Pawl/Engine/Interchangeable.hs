-- | Whether two objects are the same option -- the question a prompt has to
-- answer before it may offer one of them in place of both.
--
-- The engine never makes a player's choice, so an elision is legitimate only
-- where the options are genuinely indistinguishable. CR 732.2a puts a shortcut
-- in the hands of the player with priority rather than the game, so nothing in
-- the rules AUTHORISES this; what it does say is that a shortcut is sound only
-- when its results are predictable, which is the bar met here.
module Pawl.Engine.Interchangeable where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Recipient as Recipient

-- | Whether choosing between these two objects is choosing between options the
-- game cannot tell apart. Takes the projection rather than computing it, so a
-- caller enumerating candidates pays for one Pawl.Engine.Projection.projectAll
-- and not one per pair.
--
-- CONSERVATIVE IN THREE WAYS, because a wrong answer here silently makes a
-- player's choice for them.
--
--   * The objects are compared WHOLE, by Object's derived Eq over every field
--     but the timestamp. A field added to that record is therefore compared by
--     default, and can only ever make this answer False -- the opposite posture
--     to a hand-kept list of fields that must agree, which a new field would
--     leave unread. The timestamp is the one field two distinct objects never
--     share, and with the board quiet (below) nothing reads their relative
--     order: there is no continuous effect to sequence, and the two projections
--     are required to be equal anyway.
--   * The projections must agree, which is what an Equipment or an Aura shows up
--     in -- Bonesplitter's +2/+0 makes one Llanowar Elves a 3/1 and the other a
--     1/1.
--   * The board must be QUIET. Every game-state container that can name one
--     object and not another is required to be EMPTY rather than searched for
--     these two ids, so one stored effect anywhere retires the elision for the
--     whole board.
--
-- Not implemented: a board that is not quiet. Deciding a pair against a stored
-- effect, a replacement, a delayed trigger or a combat assignment means asking
-- each of those whether it names one of the two, which is a per-type traversal
-- this does not have (#1969).
objects :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> ObjectId -> Bool
objects pcs gs a b =
  a == b
    || ( quiet gs
           && Map.lookup a pcs == Map.lookup b pcs
           && sameObject
           && not (namedByAnother a gs)
           && not (namedByAnother b gs)
       )
  where
    sameObject = case (Map.lookup a (GameState.objects gs), Map.lookup b (GameState.objects gs)) of
      (Just x, Just y) -> x {Object.timestamp = Object.timestamp y} == y
      _ -> False

-- | One candidate per interchangeability class, keeping the FIRST of each in the
-- order it was offered. Deterministic, because Pawl.Engine.Replay.defaultAnswer
-- and several callers read the head of a candidate list.
representatives :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> NonEmpty.NonEmpty ObjectId -> NonEmpty.NonEmpty ObjectId
representatives pcs gs candidates =
  case List.foldl' keep [] (NonEmpty.toList candidates) of
    -- Unreachable: keep drops nothing it has not already got a class for.
    [] -> candidates
    first : rest -> first NonEmpty.:| rest
  where
    keep kept oid = if any (objects pcs gs oid) kept then kept else kept <> [oid]

-- Whether the board stores nothing that could name one object and not another.
--
-- The fields NOT listed here, and why each is safe to leave unread: the zones
-- and GameState.objects hold every object symmetrically, and what one object
-- says about another is namedByAnother's question; GameState.players,
-- GameState.manaPool and the per-player counters are keyed by player and name no
-- object; GameState.lastKnown is about objects that have LEFT; GameState.events,
-- GameState.controlSample and GameState.battlefieldWhenTriggered are
-- bookkeeping, and pawl has neither a tap game event nor a tap trigger
-- condition, so nothing scans them because of a choice made here;
-- GameState.extraTurns names players; and the turn, phase and priority fields
-- describe the game rather than any object.
quiet :: GameState -> Bool
quiet gs =
  null (GameState.continuousEffects gs)
    && null (GameState.replacements gs)
    && null (GameState.playerEffects gs)
    && null (GameState.blockRequirements gs)
    && null (GameState.ignoredAbilities gs)
    && Seq.null (GameState.delayedTriggers gs)
    && Seq.null (GameState.pendingPreventionRiders gs)
    && Seq.null (GameState.pendingEntryEffects gs)
    && Set.null (GameState.enteringBeside gs)
    && Map.null (GameState.phasedOut gs)
    && Map.null (GameState.haunting gs)
    && Map.null (GameState.exiledWith gs)
    && Map.null (GameState.exiledUntilMonarch gs)
    && GameState.combat gs == noCombat

-- An empty Combat, so that "nothing is in combat" is one equality over every
-- field of that record rather than a list of the fields that hold an object --
-- a CONSTRUCTION, so -Wmissing-fields names a new Combat field here.
--
-- Duplicates Pawl.Engine.Combat.emptyCombat, which cannot be imported: that
-- module depends on Pawl.Engine.Cost, and Pawl.Engine.Cost depends on this one.
noCombat :: Combat.Combat
noCombat =
  Combat.MkCombat
    { Combat.attackers = Map.empty,
      Combat.blockers = Map.empty,
      Combat.struckFirst = Nothing,
      Combat.joinedUnder = Map.empty,
      Combat.attacked = Set.empty,
      Combat.declaredAttacked = Set.empty,
      Combat.blockersDeclared = False,
      Combat.defender = Nothing
    }

-- Whether any OTHER object names this one: an Aura or Equipment attached to it,
-- which CR 303.4 stores on the rider rather than on the host, and a spell or
-- ability on the stack that took it as a target (CR 601.2c) or that named it as
-- something an instruction produced. Binding's other three fields carry no
-- object -- an amount, a Seq of mode indices, and a copiable-values snapshot.
--
-- A spell's targets reach Object.bindings only as CR 601.2i finishes the cast,
-- so the window CR 601.2g opens for that same cast cannot see them; every
-- earlier spell on the stack is visible. Pawl.ManaSpec's "an Elf a spell on the
-- stack targets is a candidate of its own" case is the two-step proof.
--
-- The ATTACHMENT arm is a regression fence rather than a proved behaviour: every
-- rider in data/cards changes what it is attached to, so the projection refuses
-- the pair before this line is reached, and no mutation of it reddens the suite.
-- Overgrowth ("Whenever enchanted land is tapped for mana, its controller adds
-- an additional {G}{G}") is the shape that would need it -- the trigger is the
-- AURA's, so the enchanted land projects exactly like the one beside it -- and
-- it is unexpressible while no trigger condition watches a tap (#1970).
namedByAnother :: ObjectId -> GameState -> Bool
namedByAnother oid gs = any names (Map.toList (GameState.objects gs))
  where
    names (other, obj) =
      other /= oid
        && ( (Object.attachedTo obj >>= Recipient.objectOf) == Just oid
               || any (bindingNames oid) (Map.elems (Object.bindings obj))
           )

-- Whether one slot's binding names this object.
bindingNames :: ObjectId -> Binding.Binding -> Bool
bindingNames oid binding =
  any (\recipient -> Recipient.objectOf recipient == Just oid) (Maybe.fromMaybe Set.empty (Binding.targets binding))
    || elem oid (Maybe.fromMaybe Seq.empty (Binding.objects binding))
