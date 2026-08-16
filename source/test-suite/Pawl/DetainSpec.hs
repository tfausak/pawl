{-# LANGUAGE GADTs #-}

-- Covers: CR 701.35 DETAIN -- Pawl.Engine.Detain, the Object.detainedUntil field
-- it writes, Effect.Detain's arm in Pawl.Engine.Resolve, and the three gates that
-- read it: Pawl.Engine.CombatRestriction's `detained` (CR 508.1c / CR 509.1b),
-- Pawl.Engine.Activate.activatableGiven (CR 602.2) and Pawl.Engine.Cost's
-- manaActivations (CR 605.3a). The duration is Pawl.Engine.Expiry's
-- clearedDetentions, in dropAtTurnOf.
--
-- Azorius Arrester ("When this creature enters, detain target creature an
-- opponent controls") is the fixture, and the only card in the pool this file
-- needs: rule 701.35a supplies the duration, so the card states nothing the
-- rulebook does not.
--
-- THE BOARD SHAPE that makes every case here discriminating: bob controls TWO of
-- one printing, identical in every respect, and alice's Arrester names one of
-- them. So the victim failing while the twin beside it succeeds is rule 701.35a
-- and nothing else -- not the phase, not the controller, not an empty candidate
-- list, and not a board on which nobody could have acted. Every assertion is made
-- about both, on the one board.
--
-- THREE SEATS, because the duration names a player and two seats cannot tell
-- three readings apart. Alice detains on her own turn; the detain has to outlast
-- bob's turn (the victim's controller) and carol's (merely the next one) and end
-- at alice's. Carol controls nothing else: she is here to be a turn.
--
-- Two printings, for the two halves of "its activated abilities can't be
-- activated". Llanowar Elves' "{T}: Add {G}" is a mana ability, which CR 605.3b
-- keeps off the stack and Pawl.Engine.Activate refuses on every board, so it can
-- only be observed through the mana window; Prodigal Sorcerer's "{T}: deals 1
-- damage" is the ordinary one. Both are 1/1 creatures with a {T} cost, so the
-- same board answers the combat clauses too.
module Pawl.DetainSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

-- Aim the Arrester's one target slot at this permanent, PINNED rather than
-- searched: an answerer that picked whatever was legal would find the other twin
-- after a mutation and keep the case green.
--
-- FILTERED out of the offered set rather than built from the id, which is not
-- hygiene here: CR 115.1a's pool of creatures offers Recipient.ToCreature, and a
-- hand-built Recipient.ToObject of the same permanent is a DIFFERENT recipient
-- that CR 608.2b's re-read at resolution drops -- silently, as a resolution that
-- names nobody.
detainAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
detainAt victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just victim) . Recipient.objectOf) offered) sets
  _ -> S.identityAnswer p

-- Alice's Azorius Arrester over the three-seat board, with bob's two twins
-- already settled, its CR 603.6a trigger placed and resolved against the first of
-- them. Returns (victim, control, state).
arresterBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
arresterBoard s registry name = do
  arrester <- S.printingOf s registry "Azorius Arrester"
  printing <- S.printingOf s registry name
  let (victim, control, gs) = arrested arrester printing
  pure (victim, control, gs)

-- The same board with the trigger NEVER placed, so nothing is detained and the
-- Arrester is standing there all the same. The paired control for every case
-- below: same seats, same creatures, same phase, same Arrester -- only rule
-- 701.35a is missing.
undetainedBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
undetainedBoard s registry name = do
  arrester <- S.printingOf s registry "Azorius Arrester"
  printing <- S.printingOf s registry name
  let (victim, control, g2) = twins printing
      (_, g3) = S.entersWithTrigger arrester S.alice g2
  pure (victim, control, g3)

twins :: Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
twins printing =
  let (victim, g1) = S.addCreature printing S.bob S.threePlayerGame
      (control, g2) = S.addCreature printing S.bob g1
   in (victim, control, g2)

arrested :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
arrested arrester printing =
  let (victim, control, g2) = twins printing
      (_, g3) = S.entersWithTrigger arrester S.alice g2
      placed = S.runPure (detainAt victim) g3 Engine.placePendingTriggers
   in (victim, control, S.runPure (detainAt victim) placed Stack.resolveTop)

-- Bob attacking alice, mid-declaration. Stated rather than run, exactly as
-- S.combatBoardOf states it: no turn-based action has filled the defender in.
bobAttacks :: GameState.GameState -> GameState.GameState
bobAttacks gs =
  gs
    { GameState.activePlayer = S.bob,
      GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defender = Just S.alice},
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

-- The permanents an activation action names, of either shape: CR 602.2's ordinary
-- activation and CR 605.3a's mana one go on one list, so no case can pass because
-- it looked at the wrong window.
activatableIds :: [A.Action] -> [ObjectId.ObjectId]
activatableIds =
  Maybe.mapMaybe
    ( \a -> case a of
        A.Activate oid _ -> Just oid
        A.ActivateManaAbility oid -> Just oid
        _ -> Nothing
    )

declaredAttackers :: GameState.GameState -> [ObjectId.ObjectId]
declaredAttackers gs = Map.keys (Combat.Type.attackers (GameState.combat gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Detain" $ do
  attackSpec s registry
  blockSpec s registry
  activateSpec s registry
  durationSpec s registry

-- CR 701.35a's first clause, through CR 508.1c.
attackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attackSpec s registry = Spec.describe s "Attack" $ do
  Spec.it s "CR 701.35a a detained creature can't attack, and its undetained twin can" $ do
    (victim, control, resolved) <- arresterBoard s registry "Prodigal Sorcerer"
    let gs = bobAttacks resolved
    Spec.assertBool s (not (Combat.canAttack S.bob victim gs)) "the detained creature is off CR 508.1a's candidate list"
    Spec.assertBool s (Combat.canAttack S.bob control gs) "its twin is still on it"
  -- The declaration itself, so the clause is proved where the game reads it and
  -- not only at the predicate. aggressiveAnswer attacks with everything it is
  -- offered, so exactly one attacker means exactly one was offered.
  Spec.it s "CR 508.1c the detained creature does not end up among the attackers" $ do
    (_, control, resolved) <- arresterBoard s registry "Prodigal Sorcerer"
    let after = S.runPure S.aggressiveAnswer (bobAttacks resolved) (Combat.declareAttackers S.bob)
    Spec.assertEqWith s "only the twin attacked" (declaredAttackers after) [control]
  -- The pair: the same board with the trigger never placed. Both attack, so the
  -- case above cannot be passing because bob could never have attacked at all.
  Spec.it s "with nothing detained both twins attack" $ do
    (victim, control, board) <- undetainedBoard s registry "Prodigal Sorcerer"
    let after = S.runPure S.aggressiveAnswer (bobAttacks board) (Combat.declareAttackers S.bob)
    Spec.assertEqWith s "both attacked" (Set.fromList (declaredAttackers after)) (Set.fromList [victim, control])

-- CR 701.35a's second clause, through CR 509.1b.
blockSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blockSpec s registry = Spec.describe s "Block" $ do
  Spec.it s "CR 701.35a a detained creature can't block, and its undetained twin can" $ do
    (victim, control, resolved) <- arresterBoard s registry "Prodigal Sorcerer"
    Spec.assertBool s (not (Combat.canBlock S.bob victim resolved)) "the detained creature is off CR 509.1a's candidate list"
    Spec.assertBool s (Combat.canBlock S.bob control resolved) "its twin is still on it"
  Spec.it s "with nothing detained both twins can block" $ do
    (victim, control, board) <- undetainedBoard s registry "Prodigal Sorcerer"
    Spec.assertBool s (Combat.canBlock S.bob victim board) "the one the Arrester would have named can block"
    Spec.assertBool s (Combat.canBlock S.bob control board) "and so can the other"

-- CR 701.35a's third clause, at both of the windows an activated ability is
-- offered through.
activateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
activateSpec s registry = Spec.describe s "Activate" $ do
  Spec.it s "CR 701.35a/602.2 a detained creature's non-mana ability is not offered, and its twin's is" $ do
    (victim, control, resolved) <- arresterBoard s registry "Prodigal Sorcerer"
    let offered = activatableIds (Action.legalActions S.bob resolved)
    Spec.assertBool s (notElem victim offered) "the detained Prodigal Sorcerer's ability is withheld"
    Spec.assertEqWith s "and its twin's is the one offer left" offered [control]
  -- CR 605.3b keeps a mana ability off Activate.activatableGiven entirely, so
  -- this half is a different gate answering the same rule, and needs its own
  -- board: rule 701.35a writes no CR 702.61b-style exemption.
  Spec.it s "CR 701.35a/605.3a a detained creature's MANA ability is not offered either" $ do
    (victim, control, resolved) <- arresterBoard s registry "Llanowar Elves"
    let offered = activatableIds (Action.legalActions S.bob resolved)
    Spec.assertBool s (notElem victim offered) "the detained Llanowar Elves is not a mana source"
    Spec.assertEqWith s "and its twin is the one offer left" offered [control]
  Spec.it s "with nothing detained both twins' abilities are offered" $ do
    (victim, control, sorcerers) <- undetainedBoard s registry "Prodigal Sorcerer"
    (elfVictim, elfControl, elves) <- undetainedBoard s registry "Llanowar Elves"
    Spec.assertEqWith
      s
      "both Prodigal Sorcerers"
      (Set.fromList (activatableIds (Action.legalActions S.bob sorcerers)))
      (Set.fromList [victim, control])
    Spec.assertEqWith
      s
      "both Llanowar Elves"
      (Set.fromList (activatableIds (Action.legalActions S.bob elves)))
      (Set.fromList [elfVictim, elfControl])

-- CR 701.35a's duration: "until the NEXT TURN OF THE CONTROLLER of that spell or
-- ability". Three seats, so the case can tell that reading from "until the
-- victim's controller's next turn" (bob, one handoff) and from "until the next
-- turn" (carol, two).
durationSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
durationSpec s registry = Spec.describe s "Duration" $ do
  Spec.it s "CR 701.35a the detain outlasts bob's turn and carol's, and ends at alice's" $ do
    (victim, control, resolved) <- arresterBoard s registry "Prodigal Sorcerer"
    let hand gs = S.runPure S.identityAnswer gs Engine.handoffTurn
        bobs = hand resolved
        carols = hand bobs
        alices = hand carols
    Spec.assertEqWith s "alice detained on her own turn" (GameState.activePlayer resolved) S.alice
    Spec.assertEqWith s "one handoff reaches bob" (GameState.activePlayer bobs) S.bob
    Spec.assertBool s (not (Combat.canBlock S.bob victim bobs)) "still detained on bob's turn"
    Spec.assertEqWith s "two reach carol" (GameState.activePlayer carols) S.carol
    Spec.assertBool s (not (Combat.canBlock S.bob victim carols)) "still detained on carol's turn"
    Spec.assertEqWith s "three come back to alice" (GameState.activePlayer alices) S.alice
    Spec.assertBool s (Combat.canBlock S.bob victim alices) "and the detain has ended"
    -- The twin blocks at every one of those points, so no assertion above can be
    -- passing because the board stopped being able to block at all.
    Spec.assertBool s (Combat.canBlock S.bob control bobs) "the twin blocks on bob's turn"
    Spec.assertBool s (Combat.canBlock S.bob control carols) "and on carol's"
    Spec.assertBool s (Combat.canBlock S.bob control alices) "and on alice's"
