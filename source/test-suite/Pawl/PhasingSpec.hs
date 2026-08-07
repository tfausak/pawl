{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Phasing (CR 702.26, "Phasing", and the CR 502.1 / 703.4a
-- turn-based action it is read from), the GameState.phasedOut field that carries
-- CR 702.26b's status, and the arm of Pawl.Engine.Engine.runTurnBasedActions that
-- runs the action ahead of CR 502.2's day/night check and CR 502.3's untap.
--
-- Gameplay-level throughout. Sandbar Crocodile is the whole fixture: its entire
-- printed text is "Phasing", so every assertion here is about rule 702.26 and not
-- about a card. Goblin Piker stands beside it in the cases that need a permanent
-- WITHOUT phasing, as the falsifier for an action that phased out the board.
--
-- What is NOT asserted, because no card in the pool can reach it: CR 702.26g-j's
-- indirect phasing (#928), effects that phase a permanent out (#929), CR
-- 702.26e/f's continuous-effect consequences (#930), and CR 702.26n's schedule for
-- a permanent phased out under a player who has since left the game (#931) -- CR
-- 702.26k's own clause is asserted below. CR 506.4's removal from combat is in
-- Pawl.Engine.Phasing.phaseOut and is likewise unreachable: the untap step is the
-- turn's first step, so nothing is ever in combat when the phasing action runs,
-- and only #929 can reach that sentence of rule 702.26b.
module Pawl.PhasingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Phasing as Phasing
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 502: the untap step's turn-based actions, run for `pid`. DaytimeSpec's
-- helper of the same shape, with the active player made explicit because the
-- whole of rule 502.1 turns on whose step it is.
untapStep :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
untapStep pid gs =
  S.runPure
    S.identityAnswer
    gs {GameState.activePlayer = pid}
    (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))

-- Is this permanent among the ones the game currently treats as existing? The
-- battlefield membership every other reader in the engine consults, asked
-- directly, because CR 702.26b's "treated as though it does not exist" is
-- exactly this bit.
onBattlefield :: ObjectId.ObjectId -> GameState.GameState -> Bool
onBattlefield oid gs = Set.member oid (GameState.battlefield gs)

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

zoneOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Zone.Zone
zoneOf oid gs = fmap Object.zone (Game.lookupObject oid gs)

tap :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
tap oid gs =
  gs
    { GameState.objects =
        Map.adjust (\obj -> obj {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)
    }

-- Alice controls one Sandbar Crocodile and nothing else has happened.
crocodileBoard :: Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
crocodileBoard crocodile = S.addCreature crocodile S.alice (Setup.emptyGame S.bothPlayers)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Phasing" $ do
  phaseOutSpec s registry
  phaseInSpec s registry
  controllerSpec s registry
  untapOrderSpec s registry
  nonexistenceSpec s registry
  departureSpec s registry

phaseOutSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phaseOutSpec s registry = Spec.describe s "PhaseOut" $ do
  -- CR 702.26a's first half and CR 702.26b's first two sentences, driven through
  -- the turn-based action: alice's untap step begins, her phased-in permanent
  -- with phasing phases out, and the game stops treating it as existing.
  Spec.it s "CR 702.26a/702.26b a permanent with phasing phases out during its controller's untap step" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        after = untapStep S.alice board
    Spec.assertEqWith s "it was on the battlefield before the step" (onBattlefield crocId board) True
    Spec.assertEqWith s "and is not afterwards" (onBattlefield crocId after) False
    Spec.assertEqWith s "its status is phased out" (Phasing.isPhasedOut crocId after) True
    Spec.assertEqWith s "under alice" (Phasing.phasedOutUnder crocId after) (Just S.alice)
    -- CR 702.26b as every OTHER reader sees it: a phased-out creature is not a
    -- creature alice controls, because the battlefield walk no longer finds it.
    Spec.assertEqWith s "alice controls no creature" (S.creaturesInPlay S.alice after) 0
  -- CR 702.26d's first sentence: "the phasing event doesn't actually cause a
  -- permanent to change zones". The object is the SAME object, still on the
  -- battlefield as far as its own zone says, and no zone change happened -- which
  -- is what keeps CR 603.6's zone-change triggers silent and stops CR 400.7
  -- minting a new incarnation.
  Spec.it s "CR 702.26d phasing out changes no zone" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        after = untapStep S.alice board
    Spec.assertEqWith s "the object still exists" (Maybe.isJust (Game.lookupObject crocId after)) True
    Spec.assertEqWith s "and its zone is still the battlefield" (zoneOf crocId after) (Just Zone.Battlefield)
    Spec.assertEqWith s "no object was created or destroyed" (Map.size (GameState.objects after)) (Map.size (GameState.objects board))
  -- The falsifier for an action that emptied the battlefield rather than reading
  -- rule 702.26a's "with phasing". Goblin Piker has no phasing and stays.
  Spec.it s "CR 702.26a a permanent without phasing does not phase out" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        after = untapStep S.alice board
    Spec.assertEqWith s "the Piker is still on the battlefield" (onBattlefield pikerId after) True
    Spec.assertEqWith s "and is not phased out" (Phasing.isPhasedOut pikerId after) False

phaseInSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phaseInSpec s registry = Spec.describe s "PhaseIn" $ do
  -- CR 702.26a's second half and CR 702.26c: the next untap step of the player it
  -- phased out under brings it back. Two of alice's untap steps in a row, which is
  -- the whole cycle the card is printed to do.
  Spec.it s "CR 702.26a/702.26c a phased-out permanent phases in at its controller's next untap step" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        gone = untapStep S.alice board
        back = untapStep S.alice gone
    Spec.assertEqWith s "it was phased out" (onBattlefield crocId gone) False
    Spec.assertEqWith s "and is back on the battlefield" (onBattlefield crocId back) True
    Spec.assertEqWith s "with no phased-out status left" (Phasing.isPhasedOut crocId back) False
    Spec.assertEqWith s "and alice controls a creature again" (S.creaturesInPlay S.alice back) 1
  -- "This all happens simultaneously" (CR 702.26a's third sentence). Both halves
  -- read the state before either writes, so the permanent this step phased OUT is
  -- not also a permanent this step phases IN. The falsifier for a sequential
  -- implementation, in which a creature with phasing would never leave at all.
  Spec.it s "CR 702.26a phasing out and phasing in happen simultaneously" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        after = Phasing.phasingEvent S.alice board
    Spec.assertEqWith s "one step out, and it stays out" (onBattlefield crocId after) False
  -- A third untap step phases it back out: the card alternates for as long as it
  -- is on the battlefield, which is what makes rule 702.26a a schedule rather than
  -- a one-shot.
  Spec.it s "CR 702.26a the cycle repeats every untap step" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        states = iterate (untapStep S.alice) board
    Spec.assertEqWith
      s
      "in, out, in, out, in"
      (fmap (onBattlefield crocId) (take 5 states))
      [True, False, True, False, True]

controllerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
controllerSpec s registry = Spec.describe s "Controller" $ do
  -- CR 702.26a scopes both halves to the player whose untap step it is: "that
  -- player controls" and "under that player's control". Bob's untap step does
  -- nothing to alice's Crocodile.
  Spec.it s "CR 702.26a another player's untap step phases nothing out" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        after = untapStep S.bob board
    Spec.assertEqWith s "alice's Crocodile is still on the battlefield" (onBattlefield crocId after) True
  -- The other direction, and the case a `phasedOut` set without the player in it
  -- would get wrong: once it has phased out under alice, BOB's untap step must
  -- leave it out. Only alice's brings it back.
  Spec.it s "CR 702.26a it phases in only under the player it phased out under" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        gone = untapStep S.alice board
        bobsTurn = untapStep S.bob gone
        alicesTurn = untapStep S.alice bobsTurn
    Spec.assertEqWith s "bob's untap step leaves it phased out" (onBattlefield crocId bobsTurn) False
    Spec.assertEqWith s "still under alice" (Phasing.phasedOutUnder crocId bobsTurn) (Just S.alice)
    Spec.assertEqWith s "and alice's brings it back" (onBattlefield crocId alicesTurn) True

untapOrderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
untapOrderSpec s registry = Spec.describe s "UntapOrder" $ do
  -- CR 502.1 / 703.4a put phasing FIRST, "before the active player untaps
  -- permanents" (CR 702.26a). A tapped Crocodile is therefore gone by the time CR
  -- 502.3's untap runs and does not untap; it phases in tapped on the following
  -- untap step, and THAT step's untap -- which the phase-in precedes -- untaps it.
  --
  -- This is the ordering falsifier: run the phasing action after the untap and the
  -- first assertion below flips to Untapped.
  Spec.it s "CR 502.1/702.26a a tapped permanent phases out before it would untap" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        tapped = tap crocId board
        gone = untapStep S.alice tapped
        back = untapStep S.alice gone
    Spec.assertEqWith s "it phased out still tapped" (tapStateOf crocId gone) (Just TapState.Tapped)
    Spec.assertEqWith s "having left before the untap" (onBattlefield crocId gone) False
    Spec.assertEqWith s "and phases in, then untaps, next time" (tapStateOf crocId back) (Just TapState.Untapped)
    Spec.assertEqWith s "back on the battlefield" (onBattlefield crocId back) True

nonexistenceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
nonexistenceSpec s registry = Spec.describe s "Nonexistence" $ do
  -- CR 702.26b at the three predicates that ask "is this a permanent" WITHOUT
  -- walking the battlefield to find their candidates. Each takes an id and answers
  -- about it directly, so each has to carry the test itself; a phased-out
  -- permanent's Object.zone still reads Zone.Battlefield (CR 702.26d), and a
  -- predicate reading that field instead of the set would call a creature the game
  -- treats as nonexistent a legal attacker.
  --
  -- Unreachable through the engine today, and asserted anyway: Combat.legalAttackers
  -- and legalBlockers filter Projection.controlsGiven, which walks the battlefield
  -- set, so no menu ever offers one. These are the answers if something asks
  -- off-menu -- which #929's effect-driven phasing, able to phase out a creature
  -- already in the combat record, is what would.
  Spec.it s "CR 702.26b a phased-out creature is not a legal attacker, blocker or combat damage source" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        attacking = board {GameState.activePlayer = S.alice}
        gone = untapStep S.alice attacking
    Spec.assertEqWith s "it could attack while phased in" (Combat.canAttack S.alice crocId attacking) True
    Spec.assertEqWith s "and cannot once phased out" (Combat.canAttack S.alice crocId gone) False
    Spec.assertEqWith s "nor block" (Combat.canBlock S.alice crocId gone) False
    Spec.assertEqWith s "and CR 506.4's liveness test says it has left" (Damage.onBattlefield crocId gone) False
    -- The falsifier for the two predicates reading Object.zone: that field is
    -- unchanged, so a version that consulted it would answer True to all three.
    Spec.assertEqWith s "even though its zone still says battlefield" (zoneOf crocId gone) (Just Zone.Battlefield)

departureSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
departureSpec s registry = Spec.describe s "Departure" $ do
  -- CR 702.26k: "phased-out permanents owned by a player who leaves the game also
  -- leave the game." One of the two rules on the far side of CR 702.26b's
  -- "except", so CR 800.4a's sweep has to name GameState.phasedOut and not only
  -- the battlefield. Three seats, because CR 800.1 ends a two-player game the
  -- moment one of them leaves and CR 800.4a never runs.
  --
  -- The falsifier for the row being left behind: without the deletion in
  -- Game.removeFromZones the object is gone from GameState.objects while its id
  -- still sits in `phasedOut`, keyed to a player who will never have another
  -- untap step.
  Spec.it s "CR 702.26k a phased-out permanent leaves the game with its owner" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = S.addCreature crocodile S.alice (Setup.emptyGame S.threePlayers)
        gone = untapStep S.alice board
        after = S.runPure S.identityAnswer gone (Departure.leaveGame Departure.Type.Conceded S.alice)
    Spec.assertEqWith s "it was phased out under alice" (Phasing.phasedOutUnder crocId gone) (Just S.alice)
    Spec.assertEqWith s "the game continued without her" (GameState.result after) Nothing
    Spec.assertEqWith s "the object is gone" (Maybe.isJust (Game.lookupObject crocId after)) False
    Spec.assertEqWith s "and so is its phased-out row" (Phasing.isPhasedOut crocId after) False
