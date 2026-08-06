{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Battle, rule 310: the defense a battle prints (CR 210.1 /
-- 310.4a), the defense counters CR 310.4b makes it enter with, the protector CR
-- 310.8a has its controller choose as it enters, CR 310.11a's restriction of that
-- choice to an opponent, and CR 310.10's state-based action -- listed as CR 704.5w
-- and CR 704.5x -- repairing the designation once it is illegal.
--
-- Also the pieces rule 310 needed underneath it, exercised here because this is
-- where a card reaches them: Pawl.Types.Defense, CounterKind.Defense,
-- EntryRewrite.ChooseProtector and Object.protector.
--
-- Invasion of Dominaria // Serra Faithkeeper is the whole card pool for this file,
-- and is the only battle in `data/cards`. {2}{W} Battle -- Siege, defense 5, "When
-- this Siege enters, you gain 4 life and draw a card", transforming into a 4/4
-- Angel with flying and vigilance.
--
-- It is NOT the card #302 nominates. Invasion of Kaladesh's front face is simpler
-- still, but its BACK face is a Legendary Artifact -- Vehicle with a
-- characteristic-defining power and crew, and a card file must carry both faces
-- honestly. Serra Faithkeeper is two printed keywords, so every line of this card
-- is representable and these cases exercise rule 310 rather than the card.
--
-- NOT COVERED, because none of it is built (#302): CR 310.5's attackable battle
-- and everything a protector is FOR that depends on it -- CR 310.8b, CR 310.8c and
-- CR 310.8d with CR 508.5 -- plus CR 310.6's damage removing defense counters, CR
-- 310.7 / 704.5v's defense-0 state-based action, and CR 310.11b's "when the last
-- defense counter is removed, exile it, then you may cast it transformed".
module Pawl.BattleSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Defense as Defense
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Battle" $ do
  entrySpec s registry
  protectorSpec s registry
  candidateSpec s registry
  repairSpec s registry

-- CR 310.4b and CR 310.8a both fire as the battle enters, and both are visible
-- from a cast -- which is what makes this the gameplay-level test rather than a
-- projection one.
entrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entrySpec s registry = Spec.describe s "Entry" $ do
  Spec.it s "CR 310.4b Invasion of Dominaria enters with five defense counters" $ do
    (after, oid) <- castInvasion s registry
    Spec.assertEqWith s "five defense counters" (S.counterOf CounterKind.Defense oid after) 5
  Spec.it s "CR 210.1 that five is the PRINTED number, and it is projected" $ do
    (after, oid) <- castInvasion s registry
    Spec.assertEqWith
      s
      "the projection carries the printed defense"
      (PC.defense (Projection.project oid after))
      (Just (Defense.MkDefense 5))
  Spec.it s "CR 310.11a its protector is an opponent of its controller" $ do
    (after, oid) <- castInvasion s registry
    -- Two seats leave exactly one legal protector, so this asserts CR 310.11a's
    -- restriction rather than a choice; protectorSpec below is where the choice
    -- is made observable.
    Spec.assertEqWith s "bob protects it" (protectorOf oid after) (Just S.bob)
  Spec.it s "the printed enters trigger still fires: gain 4 life and draw a card" $ do
    (after, _) <- castInvasion s registry
    let settled = S.runPure S.identityAnswer after Engine.priorityLoop
    Spec.assertEqWith s "alice gained 4" (S.lifeOf S.alice settled) (Just 24)
    Spec.assertEqWith s "and drew one" (length (Game.zoneMembers Zone.Hand S.alice settled)) 1

-- CR 310.8a's choice, made observable. Three seats, because CR 102.2's two-player
-- game leaves a Siege exactly one legal protector and a one-candidate ask decides
-- nothing.
protectorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
protectorSpec s registry = Spec.describe s "Protector" $ do
  Spec.it s "CR 310.8a the controller chooses which opponent protects it" $ do
    (toBob, oidB) <- castInvasionThreeSeated s registry (protectTo S.bob)
    (toCarol, oidC) <- castInvasionThreeSeated s registry (protectTo S.carol)
    -- Every input the same but the answer to one prompt, the shape M5.6d's
    -- defending-player proof takes: this is what a two-seat board could not
    -- distinguish from an elision.
    Spec.assertEqWith s "bob when bob is named" (protectorOf oidB toBob) (Just S.bob)
    Spec.assertEqWith s "carol when carol is named" (protectorOf oidC toCarol) (Just S.carol)
  Spec.it s "CR 310.11a the controller is never offered, even with three seats" $ do
    -- An interpreter that names the controller anyway is filtered, not obeyed:
    -- Battle.designateProtector falls back to the head of the candidate list.
    (chosen, oid) <- castInvasionThreeSeated s registry (protectTo S.alice)
    Spec.assertBool s (protectorOf oid chosen /= Just S.alice) "alice does not protect her own Siege"
    Spec.assertBool s (protectorOf oid chosen `elem` [Just S.bob, Just S.carol]) "an opponent does"

-- CR 310.8a's candidate rule at the level Pawl.Engine.Battle states it, which is
-- the arithmetic the entry choice and the CR 704.5w re-choice SHARE -- so a drift
-- between them would show here.
--
-- The projections are the REAL card's, taken off the board a cast produced, so
-- these cases cannot pass against a Siege pawl does not actually build. The
-- no-battle-types half is that same projection with its subtypes stripped, which
-- has no printing to take it from: CR 310.11 makes every battle printed so far a
-- Siege.
candidateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
candidateSpec s registry = Spec.describe s "Candidates" $ do
  Spec.it s "CR 310.11a a Siege offers its controller's opponents and not its controller" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "bob and carol"
      (Battle.protectorCandidates siege S.alice [S.alice, S.bob, S.carol])
      [S.bob, S.carol]
  Spec.it s "CR 310.8a a battle with no battle types offers only its controller" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "alice alone"
      (Battle.protectorCandidates siege {PC.subtypes = Set.empty} S.alice [S.alice, S.bob, S.carol])
      [S.alice]
  Spec.it s "CR 704.5w a departed player is not a candidate" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "carol alone, bob having left"
      (Battle.protectorCandidates siege S.alice [S.alice, S.carol])
      [S.carol]
  -- CR 310.10's second sentence, listed as CR 704.5w's and CR 704.5x's: the branch
  -- that puts the battle into its owner's graveyard. Held HERE rather than at the
  -- game level because it is
  -- unreachable there: a Siege's candidates are its controller's opponents still
  -- in the game, and a game in which its controller has no opponent left has
  -- already ended under CR 104.2a. Pawl.Engine.Sba routes an empty answer into
  -- the put-into-graveyard batch whether or not a game can reach it (#853).
  Spec.it s "CR 704.5w a Siege whose controller is alone has no candidate" $ do
    siege <- siegePC s registry
    Spec.assertEqWith s "nobody" (Battle.protectorCandidates siege S.alice [S.alice]) []
  Spec.it s "CR 704.5x a Siege protected by its own controller needs repair" $ do
    siege <- siegePC s registry
    Spec.assertBool
      s
      (Battle.needsProtector siege S.alice [S.alice, S.bob] (Just S.alice))
      "the controller is not a legal protector of their own Siege"
  Spec.it s "CR 310.10 a legal designation needs no repair" $ do
    siege <- siegePC s registry
    Spec.assertBool
      s
      (not (Battle.needsProtector siege S.alice [S.alice, S.bob] (Just S.bob)))
      "bob is legal and is left alone"

-- CR 704.5w: the designation is repaired by a state-based action once the
-- designated player is no longer in the game.
repairSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
repairSpec s registry = Spec.describe s "Repair" $ do
  Spec.it s "CR 704.5w a battle whose protector leaves the game gets a new one" $ do
    (entered, oid) <- castInvasionThreeSeated s registry (protectTo S.carol)
    Spec.assertEqWith s "carol protects it to begin with" (protectorOf oid entered) (Just S.carol)
    let gone = Departure.depart Departure.Type.Conceded S.carol entered
        repaired = S.runPure S.identityAnswer gone Sba.checkStateBasedActions
    -- bob is the only opponent left, so the re-choice is elided and its answer is
    -- forced -- which is what makes this assert the SBA firing rather than an
    -- answerer's preference.
    Spec.assertEqWith s "bob protects it now" (protectorOf oid repaired) (Just S.bob)
    Spec.assertBool s (S.onBattlefield oid repaired) "and the battle is still on the battlefield"
  Spec.it s "CR 704.5w a legal protector is not re-chosen" $ do
    (entered, oid) <- castInvasionThreeSeated s registry (protectTo S.carol)
    -- Nobody has left, so CR 704.5w does not apply and the pass must not ask
    -- again. Run under an answerer that would name BOB if it were asked: with all
    -- three seats filled the re-choice would have two candidates and could not be
    -- elided, so the designation still standing at carol is what says the
    -- state-based action declined to fire. Asserting against S.identityAnswer
    -- instead would pass whether or not it fired, since bob leaving leaves carol
    -- the only candidate either way.
    let checked = S.runPure (protectTo S.bob) entered Sba.checkStateBasedActions
    Spec.assertEqWith s "carol still protects it" (protectorOf oid checked) (Just S.carol)
  Spec.it s "CR 704.5w the repair reports that an action was performed" $ do
    (entered, _) <- castInvasionThreeSeated s registry (protectTo S.carol)
    -- CR 704.3: the check repeats until no state-based action is performed, so a
    -- pass that repaired a designation must SAY it acted. Read off
    -- performStateBasedActions' own answer, which is the value that loop reads;
    -- nothing else about the repair is visible to it.
    let gone = Departure.depart Departure.Type.Conceded S.carol entered
        (acted, _) = S.runPureWith S.identityAnswer gone Sba.performStateBasedActions
    Spec.assertBool s acted "the pass reports the repair"
  Spec.it s "CR 704.5w an unrelated departure leaves the designation alone" $ do
    (entered, oid) <- castInvasionThreeSeated s registry (protectTo S.carol)
    -- bob leaving touches nothing: carol is still a legal protector, so the
    -- condition is not met. The mirror of the repair case above.
    let gone = Departure.depart Departure.Type.Conceded S.bob entered
        checked = S.runPure S.identityAnswer gone Sba.checkStateBasedActions
    Spec.assertEqWith s "carol still protects it" (protectorOf oid checked) (Just S.carol)

-- Cast Invasion of Dominaria on the two-seat board and settle the stack, giving
-- back the state and the battle's id. A Plains sits in the library so the printed
-- trigger's draw has a card to find -- CR 704.5b would otherwise end the game
-- rather than the assertion under test.
castInvasion ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId)
castInvasion s registry = do
  plains <- S.printingOf s registry "Plains"
  invasion <- S.printingOf s registry "Invasion of Dominaria"
  let stocked = snd (S.addLibraryCard plains S.alice (S.landsInPlay plains 3))
      (gs, spellId) = S.handOne invasion stocked
      cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
      after = S.runPure S.identityAnswer cast Stack.resolveTop
  named s after

-- castInvasion's three-seat twin, under a given answerer. Three seats make this a
-- multiplayer game (CR 800.1), which is what leaves alice's Siege two legal
-- protectors instead of one.
castInvasionThreeSeated ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  (forall r. Prompt.Prompt r -> r) ->
  m (GameState.GameState, ObjectId.ObjectId)
castInvasionThreeSeated s registry answer = do
  plains <- S.printingOf s registry "Plains"
  invasion <- S.printingOf s registry "Invasion of Dominaria"
  let lands = List.foldl' (\g _ -> snd (S.addCreature plains S.alice g)) S.threePlayerGame [1 :: Int .. 3]
      stocked = snd (S.addLibraryCard plains S.alice lands)
      (spellId, handed) = S.addHandCard invasion S.alice stocked
      ready =
        handed
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      cast = S.runPure answer ready (S.cast S.alice spellId)
      after = S.runPure answer cast Stack.resolveTop
  named s after

named :: (Monad m) => Spec.Spec m n -> GameState.GameState -> m (GameState.GameState, ObjectId.ObjectId)
named s gs = case battleOf gs of
  Nothing -> do
    Spec.assertFailure s "Invasion of Dominaria did not reach the battlefield"
    pure (gs, ObjectId.MkObjectId 0)
  Just oid -> pure (gs, oid)

-- The battlefield's one battle, by the card type rule 310 keys on.
battleOf :: GameState.GameState -> Maybe ObjectId.ObjectId
battleOf gs =
  let pcs = Projection.projectAll gs
      battles = filter (\oid -> maybe False Battle.isBattle (Map.lookup oid pcs)) (Set.toAscList (GameState.battlefield gs))
   in case battles of
        [oid] -> Just oid
        _ -> Nothing

protectorOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe PlayerId.PlayerId
protectorOf oid gs = Object.protector =<< Map.lookup oid (GameState.objects gs)

-- The real Siege's projection, taken off a board a cast produced.
siegePC :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m PC.ProjectedCharacteristics
siegePC s registry = do
  (after, oid) <- castInvasion s registry
  pure (Projection.project oid after)

-- Name a protector and answer everything else the ordinary way, the shape
-- S.attackTo takes for CR 507.1's defending player.
protectTo :: PlayerId.PlayerId -> Prompt.Prompt r -> r
protectTo who p = case p of
  Prompt.ChooseProtector {} -> who
  _ -> S.identityAnswer p
