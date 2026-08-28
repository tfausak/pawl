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
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PhasedOut as PhasedOut
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
--     whole board. The ID-KEYED RELATIONS are the exception, and are
--     SEARCHED instead -- namedByRelation below.
--
-- Not implemented: the containers quiet still requires empty. Deciding a pair
-- against a stored continuous effect, a replacement, a player, block or attack
-- row, a regeneration prohibition, an ignore, a delayed trigger, a prevention
-- rider, a pending entry effect or a combat assignment means asking each of
-- those whether it names one of the two, which is a per-type traversal this does
-- not have (#1969).
objects :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> ObjectId -> Bool
objects pcs gs a b =
  a == b
    || ( quiet gs
           && Map.lookup a pcs == Map.lookup b pcs
           && sameObject
           && not (namedByRelation a gs)
           && not (namedByRelation b gs)
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

-- Whether the board stores nothing that could name one object and not another,
-- BAR the id-keyed relations namedByRelation searches instead.
--
-- Both lists are HAND-KEPT, and nothing forces a new GameState field into
-- either, so the account below covers the whole record. The fields NOT read
-- anywhere here, and why each is safe to leave unread:
--
--   * The zones and GameState.objects hold every object symmetrically, and what
--     one object says about another is namedByAnother's question.
--   * GameState.players, GameState.manaPool, GameState.pendingControl,
--     GameState.activeControl (a Decider is a PlayerId), GameState.monarch,
--     GameState.drewFromEmpty, GameState.speedIncreasedThisTurn,
--     GameState.extraTurns and the per-player counters (landsPlayed,
--     drawsThisTurn, spellsCastLastTurn, castsLastTurn) are keyed by or valued
--     at a PLAYER and name no object.
--   * GameState.lastKnown is about objects that have LEFT.
--   * GameState.events, GameState.controlSample and
--     GameState.battlefieldWhenTriggered are bookkeeping, and pawl has neither a
--     tap game event nor a tap trigger condition, so nothing scans them because
--     of a choice made here.
--   * GameState.outsideObjects (CR 729.4) is keyed by ids belonging to a game
--     this one is nested inside, and its value is an owner and a printing; the
--     id supplies of the two games are disjoint (Setup.funnelBack takes the max),
--     so no key of it can be a candidate.
--   * GameState.ambientAmounts is keyed by slot and valued at a number.
--   * The turn, phase, priority, result, daytime, signal, supply and printing
--     fields describe the GAME rather than any object.
--
-- GameState.unregeneratables IS listed below, for the reason every other
-- listed field is: a row names one object and not another (CR 701.19c), so two
-- creatures alike in every characteristic are told apart by which of them a
-- prohibition covers.
--
-- GameState.enteringCounters, GameState.detachedBindings and
-- GameState.broughtIn are listed below because each holds ObjectIds, and NOT
-- because any board reaches this with one of them non-empty: the first is empty
-- outside an entry loop, and the other two are CR 729's subgame bookkeeping.
-- They are in the list rather than in the account above because requiring a
-- field empty is the direction that cannot make a player's choice, so an
-- unreachable row is the cheap side to be wrong on.
quiet :: GameState -> Bool
quiet gs =
  null (GameState.continuousEffects gs)
    && null (GameState.replacements gs)
    && null (GameState.playerEffects gs)
    && null (GameState.blockRequirements gs)
    && null (GameState.attackRequirements gs)
    && null (GameState.unregeneratables gs)
    && null (GameState.ignoredAbilities gs)
    && Seq.null (GameState.delayedTriggers gs)
    && Seq.null (GameState.pendingPreventionRiders gs)
    && Seq.null (GameState.pendingEntryEffects gs)
    && Seq.null (GameState.broughtIn gs)
    && Set.null (GameState.enteringBeside gs)
    && Set.null (GameState.enteringSubjects gs)
    && Map.null (GameState.enteringCounters gs)
    && Map.null (GameState.detachedBindings gs)
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
      Combat.declaredAttackedThisStep = Set.empty,
      Combat.declaredAttackers = Set.empty,
      Combat.declaredBlockers = Set.empty,
      Combat.blockersDeclared = False,
      Combat.defender = Nothing
    }

-- Whether one of the board's ID-KEYED RELATIONS names this object. Each is
-- a Map from an object to what the game remembers about it, so the ids a row
-- names are its KEY plus whatever ids its value holds -- and both are read here,
-- which is what makes searching them exact rather than merely likely. What quiet
-- still requires empty is either a container whose rows name objects through a
-- nested structure (a Filter, an Expiry, an effect) no traversal here can bound,
-- or one that is empty at every window this is reached from anyway.
--
--   * GameState.phasedOut (CR 702.26b), keyed by the phased-out permanent, its
--     value the player it phased out under.
--   * GameState.exiledUntilMonarch (CR 725), keyed by the exiled incarnation,
--     its value the watching player and whether the crown has moved.
--   * GameState.haunting (CR 702.55b), keyed by the haunting card in exile, its
--     value the object that card haunts.
--   * GameState.exiledWith (CR 607.2), keyed by the exiled card, its value the
--     object CR 607.2a's or CR 607.2b's link names.
--
-- phasedOut and exiledUntilMonarch can never name a CANDIDATE through the one
-- caller (Pawl.Engine.Cost's mana-source window): both key on an object not on
-- the battlefield, and neither value holds an object at all. They are searched
-- rather than required empty all the same, because "this row is about some other
-- object" is the honest reading of the rule and requiring emptiness makes an
-- unrelated phased-out permanent decide a pair it says nothing about.
--
-- The haunting arm's VALUE side is the proved one: Pawl.ManaSpec's "an Elf a
-- haunting card in exile haunts is a candidate of its own" is a haunt row naming
-- one of three otherwise identical Elves, beside "a haunt row that names none of
-- them leaves the elision standing", which is the same board with the row's
-- value moved.
--
-- The KEY side is a regression fence rather than a proof, and so is exiledWith's
-- value side. Every one of these relations keys on an object that is not on the
-- battlefield -- an exiled incarnation, or a permanent GameState.battlefield
-- excludes (CR 702.26b) -- so no key can ever be a mana-source candidate, and
-- neutering `key == oid` below leaves the whole suite green. The line stays
-- because a row does name its key; do not read the green as coverage.
namedByRelation :: ObjectId -> GameState -> Bool
namedByRelation oid gs =
  relates phasedOutNames (GameState.phasedOut gs)
    || relates monarchWatchNames (GameState.exiledUntilMonarch gs)
    || relates Set.singleton (GameState.haunting gs)
    || relates Set.singleton (GameState.exiledWith gs)
  where
    relates names = any (\(key, value) -> key == oid || Set.member oid (names value)) . Map.toList

-- The objects a GameState.phasedOut row names BEYOND its key: none, since CR
-- 702.26a's stored value is the player the permanent phased out under.
--
-- Total over PhasedOut's constructors and POSITIONAL, so a field added to any of
-- them is an arity error here rather than an ObjectId this silently stops
-- reading -- the failure mode a `_` arm or a `{}` pattern would hide.
phasedOutNames :: PhasedOut.PhasedOut -> Set.Set ObjectId
phasedOutNames row = case row of
  PhasedOut.Directly _under -> Set.empty
  PhasedOut.Indirectly _under -> Set.empty
  PhasedOut.Orphaned _under -> Set.empty

-- The objects a GameState.exiledUntilMonarch row names beyond its key: none. Its
-- value is a player and a flag. Positional for phasedOutNames' reason.
monarchWatchNames :: MonarchWatch.MonarchWatch -> Set.Set ObjectId
monarchWatchNames watch = case watch of
  MonarchWatch.MkMonarchWatch _controller _due -> Set.empty

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
-- The ATTACHMENT arm is proved rather than a fence, and Betrayal ({U} Aura,
-- "Whenever enchanted creature becomes tapped, you draw a card") is what proves
-- it: it changes nothing about its host, so the enchanted permanent projects
-- exactly like the one beside it, and only this line tells the two apart.
-- Pawl.ManaSpec's "an Elf enchanted by an Aura that changes nothing about it is
-- still a candidate of its own" is the case, and it asserts the identical
-- projection alongside the offer so the reason is pinned as well as the answer.
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
