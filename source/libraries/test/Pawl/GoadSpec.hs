{-# LANGUAGE GADTs #-}

-- Covers: CR 701.15 GOAD -- Pawl.Engine.Goad, the Object.goadedBy field it
-- writes, Effect.Goad's arm in Pawl.Engine.Resolve, the two CR 508.1d
-- requirements Pawl.Engine.AttackRequirement mints from it, and the duration,
-- which is Pawl.Engine.Expiry's clearedGoads in dropAtTurnOf.
--
-- Jeering Homunculus ("When this creature enters, you may goad target creature")
-- is the fixture and the only card in the pool this file needs: rule 701.15a
-- supplies the duration and rule 701.15b the two requirements, so the card
-- states nothing the rulebook does not.
--
-- THREE SEATS, because rule 701.15b's second requirement names "a player other
-- than the controller of the permanent, spell, or ability that caused it to be
-- goaded", and two seats collapse that onto nobody. bob goads a creature ALICE
-- controls, and carol is the player the requirement is about.
--
-- TWO BOARDS, differing only in whom CR 507.1 made the defending player, because
-- pawl chooses one defending player per combat (see #175) and that is what decides
-- whether the second requirement can be obeyed at all:
--
-- \* against CAROL, both requirements are live, and they part company on the
--   PLANESWALKER -- attacking a planeswalker carol controls is not attacking
--   carol (CR 508.1b lists player, planeswalker and battle separately), so it
--   obeys the first requirement and not the second. That is the only board on
--   which the object axis is observable.
--
-- \* against BOB, the second requirement can be obeyed by no announcement CR
--   508.1b admits, so CR 508.1's "if able" leaves it unmet and the first
--   requirement alone decides the declaration. This is the case a reading that
--   made rule 701.15b's second clause a CR 508.1c RESTRICTION would get wrong:
--   it would forbid the only attacks available and make the goaded creature
--   unable to attack at all.
--
-- THE PAIRED CONTROL for every case is the same board with the Homunculus's
-- "may" DECLINED (CR 601.2c still chose the target, so the two boards differ in
-- exactly the goad). alice also controls a second creature the Homunculus never
-- named, so no assertion below can pass because the whole board was forced.
module Pawl.GoadSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Goad as Goad
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

-- Aim the Homunculus's one target slot at this permanent and answer its "may"
-- with `decision`, both PINNED rather than searched: an answerer that picked
-- whatever was legal would find alice's other creature after a mutation and keep
-- the case green.
--
-- FILTERED out of the offered set rather than built from the id: CR 115.1's pool
-- of creatures offers Recipient.ToCreature, and a hand-built Recipient.ToObject
-- of the same permanent is a DIFFERENT recipient that CR 608.2b's re-read at
-- resolution drops, silently.
goadAt :: OptionalDecision.OptionalDecision -> ObjectId.ObjectId -> Prompt.Prompt r -> r
goadAt decision victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just victim) . Recipient.objectOf) offered) sets
  Prompt.ChooseOptional {} -> decision
  _ -> S.identityAnswer p

-- Attacks with everything and announces the PLANESWALKER for every attacker,
-- which CR 508.1d then has to refuse. CombatEffectSpec's `announcing`, kept local
-- rather than hoisted (Pawl.Support rebuilds every spec in the tree).
announcing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
announcing walker p = case p of
  Prompt.ChooseAttackTarget {} -> AttackTarget.OfPlaneswalker walker
  _ -> S.aggressiveAnswer p

-- alice's two creatures, bob's Homunculus and one planeswalker each for bob and
-- carol, with the Homunculus's CR 603.6a trigger placed and resolved against
-- alice's FIRST creature under `decision`. Returns (goaded, untouched, bob's
-- planeswalker, carol's planeswalker, state).
--
-- The planeswalkers carry loyalty so CR 704.5i does not bury them the moment
-- anything settles, and so CR 508.1b really does offer two announcements.
homunculusBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  OptionalDecision.OptionalDecision ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
homunculusBoard s registry decision = do
  homunculus <- S.printingOf s registry "Jeering Homunculus"
  jace <- S.printingOf s registry "Jace Beleren"
  piker <- S.printingOf s registry "Goblin Piker"
  centaur <- S.printingOf s registry "Windseeker Centaur"
  case goadedBoard homunculus jace piker centaur decision of
    Just board -> pure board
    Nothing -> do
      _ <- Spec.assertFailure s "fixture should build"
      pure (S.noSource, S.noSource, S.noSource, S.noSource, S.threePlayerGame)

goadedBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  OptionalDecision.OptionalDecision ->
  Maybe (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
goadedBoard homunculus jace piker centaur decision =
  case S.threePlayerCombat [piker, centaur] [jace] [jace] of
    (gs0, [victim, untouched], [bobWalker], [carolWalker]) ->
      let loyal = S.addCounter CounterKind.Loyalty 3 carolWalker (S.addCounter CounterKind.Loyalty 3 bobWalker gs0)
          (_, entered) = S.entersWithTrigger homunculus S.bob loyal
          placed = S.runPure (goadAt decision victim) entered Engine.placePendingTriggers
       in Just (victim, untouched, bobWalker, carolWalker, S.runPure (goadAt decision victim) placed Stack.resolveTop)
    _ -> Nothing

-- alice mid-declaration against one chosen defending player. Stated rather than
-- run, exactly as DetainSpec's bobAttacks states it: CR 507.1's choice is a
-- turn-based action this fixture does not need to run.
attacking :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
attacking defender gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defender = Just defender},
      GameState.remaining =
        Seq.fromList
          [ Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain,
            Phase.Ending EndingStep.EndStep,
            Phase.Ending EndingStep.Cleanup
          ]
    }

announced :: GameState.GameState -> ObjectId.ObjectId -> Maybe AttackTarget.AttackTarget
announced gs oid = Map.lookup oid (Combat.Type.attackers (GameState.combat gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Goad" $ do
  objectSpec s registry
  subjectSpec s registry
  durationSpec s registry

-- CR 701.15b's SECOND requirement, on the board where it can be obeyed.
objectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
objectSpec s registry = Spec.describe s "Object" $ do
  Spec.it s "CR 701.15b whole cards: the goaded creature is sent at carol, not at carol's Jace" $ do
    (goaded, _, _, carolWalker, resolved) <- homunculusBoard s registry OptionalDecision.Exercises
    (_, _, _, controlWalker, declined) <- homunculusBoard s registry OptionalDecision.Declines
    let after = S.runPure (announcing carolWalker) (attacking S.carol resolved) (Combat.declareAttackers S.alice)
        without = S.runPure (announcing controlWalker) (attacking S.carol declined) (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "CR 701.15b the goaded creature attacks carol, not the Jace it was announced against" (announced after goaded) (Just (AttackTarget.OfPlayer S.carol))
    -- The SAME interpreter and the same board bar the "may", so the redirect is
    -- rule 701.15b and not a rule about announcements.
    Spec.assertEqWith s "with the may declined the announcement stands" (announced without goaded) (Just (AttackTarget.OfPlaneswalker controlWalker))
  Spec.it s "CR 701.15b attacking a planeswalker is not attacking a player" $ do
    (goaded, untouched, _, carolWalker, resolved) <- homunculusBoard s registry OptionalDecision.Exercises
    (control, _, _, controlWalker, declined) <- homunculusBoard s registry OptionalDecision.Declines
    let gs = attacking S.carol resolved
    Spec.assertBool
      s
      (not (Combat.legalAttackDeclarationAs S.alice [(goaded, AttackTarget.OfPlaneswalker carolWalker)] gs))
      "CR 701.15b: announcing carol's Jace does not obey 'attacks a player other than bob'"
    Spec.assertBool
      s
      (Combat.legalAttackDeclarationAs S.alice [(goaded, AttackTarget.OfPlayer S.carol)] gs)
      "announcing carol does"
    -- The creature the Homunculus never named is unconstrained, so the refusal
    -- above is about the goad rather than about the board.
    Spec.assertBool
      s
      (Combat.legalAttackDeclarationAs S.alice [(goaded, AttackTarget.OfPlayer S.carol), (untouched, AttackTarget.OfPlaneswalker carolWalker)] gs)
      "and the unnamed creature may still attack that Jace alongside it"
    Spec.assertBool
      s
      (Combat.legalAttackDeclarationAs S.alice [(control, AttackTarget.OfPlaneswalker controlWalker)] (attacking S.carol declined))
      "with the may declined, that same creature may attack the Jace"

-- CR 701.15b's FIRST requirement, and CR 508.1's "if able" on the second.
subjectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
subjectSpec s registry = Spec.describe s "Subject" $ do
  Spec.it s "CR 701.15b a goaded creature attacks each combat if able" $ do
    (_, _, _, _, resolved) <- homunculusBoard s registry OptionalDecision.Exercises
    (_, _, _, _, declined) <- homunculusBoard s registry OptionalDecision.Declines
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] (attacking S.carol resolved))) "declining to attack obeys nothing"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] (attacking S.carol declined)) "with the may declined, alice may decline altogether"
  -- THE IMPOSSIBLE CASE. bob is the defending player, so no announcement CR
  -- 508.1b admits attacks a player other than bob: CR 508.1's maximization
  -- leaves that requirement unmet rather than making every declaration illegal.
  Spec.it s "CR 508.1 the second requirement goes unmet where no announcement can obey it" $ do
    (goaded, _, bobWalker, _, resolved) <- homunculusBoard s registry OptionalDecision.Exercises
    (_, _, _, _, declined) <- homunculusBoard s registry OptionalDecision.Declines
    let gs = attacking S.bob resolved
    Spec.assertBool
      s
      (Combat.legalAttackDeclarationAs S.alice [(goaded, AttackTarget.OfPlaneswalker bobWalker)] gs)
      "CR 508.1: with only the goader to attack, attacking the goader's own Jace is legal"
    Spec.assertBool
      s
      (Combat.legalAttackDeclarationAs S.alice [(goaded, AttackTarget.OfPlayer S.bob)] gs)
      "and so is attacking the goader"
    -- The first requirement is still in force on that same board, so the two
    -- assertions above are CR 508.1's "if able" and not a goad that stopped.
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "but declining is still illegal"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] (attacking S.bob declined)) "with the may declined it is legal again"

-- CR 701.15a's duration, and CR 701.15d's deduplication.
durationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
durationSpec s registry = Spec.describe s "Duration" $ do
  Spec.it s "CR 701.15a the goad lasts until the goader's next turn" $ do
    (goaded, _, _, _, resolved) <- homunculusBoard s registry OptionalDecision.Exercises
    Spec.assertEqWith s "bob goaded it" (Goad.goadedBy goaded resolved) (Set.singleton S.bob)
    Spec.assertEqWith s "carol's turn beginning does not end it" (Goad.goadedBy goaded (Expiry.dropAtTurnOf S.carol resolved)) (Set.singleton S.bob)
    Spec.assertEqWith s "bob's does" (Goad.goadedBy goaded (Expiry.dropAtTurnOf S.bob resolved)) Set.empty
  -- CR 701.15d, at the store rather than through a second card: a set is what
  -- makes the second goad create no additional requirement, and no board with
  -- one Homunculus on it can goad twice.
  Spec.it s "CR 701.15d the same player goading again creates no additional requirement" $ do
    (goaded, _, _, _, resolved) <- homunculusBoard s registry OptionalDecision.Exercises
    let again = Goad.goad S.bob goaded resolved
        alsoCarol = Goad.goad S.carol goaded again
    Spec.assertEqWith s "bob goading again leaves one entry" (Goad.goadedBy goaded again) (Set.singleton S.bob)
    -- CR 701.15c: a DIFFERENT player does add one.
    Spec.assertEqWith s "carol goading adds hers" (Goad.goadedBy goaded alsoCarol) (Set.fromList [S.bob, S.carol])
