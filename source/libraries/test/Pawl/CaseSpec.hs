{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 719's Case cards, which need no engine subsystem of their own: the
-- whole of rule 719 is Pawl.Types.Designation's Solved arm plus the two ordinary
-- abilities CR 719.3a and CR 719.3c say the keywords mean. So what this file
-- exercises is the marker's lifecycle through the modules that already carry a
-- designation -- Pawl.Engine.Resolve's Designate arm writes it,
-- Pawl.Engine.Quantity's HasDesignation arm reads it, and
-- Pawl.Types.TriggeredAbility's intervening "if" gates on it.
--
-- Case of the Ransacked Lab, MKM 45, carries the first three groups. Its three
-- clauses are all expressible: a cost reduction Baral, Chief of Compliance
-- already prints word for word, a to-solve condition that is a count over the CR
-- 608.2i log, and a Solved clause that is a triggered ability rather than a
-- cast-from-graveyard permission (#670) or a swept set of damage dealers.
--
-- Case of the Pilfered Proof, MKM 9, carries the last, and is here for its
-- FIRST clause rather than for rule 719: a CR 603.1b two-condition trigger whose
-- branches name one subject. Its Solved clause is CR 702.169b's static reading
-- and a CR 614.1a token replacement, so the two Cases cover both shapes rule
-- 702.169 gives "Solved --".
--
-- CR 719.1 and CR 719.2 are frame rules with no rules meaning, so nothing here
-- asserts about the layout.
--
-- What is NOT proven here is CR 719.3a's "and this Case is not solved". The card
-- carries the limb because the rule prints it, but deleting it leaves every case
-- below green: Object.designations is a Set and Pawl.Engine.Resolve's Designate
-- arm writes only on a transition, so a to-solve trigger that fired again at a
-- later end step would change nothing and emit nothing. That is the same fence
-- Resolve's own comment records for renown and monstrosity, and it is a fence
-- here too rather than coverage.
--
-- CR 719.3c makes the Solved ability not EXIST while the Case is unsolved, and
-- the card writes it as an ability that exists and declines to trigger (CR
-- 603.4). The two are observably identical on this producer: CR 719.3b makes
-- solved permanent while the permanent is on the battlefield, so the intervening
-- clause cannot flip between trigger and resolution, and nothing in pawl can ask
-- a permanent which abilities it has.
module Pawl.CaseSpec where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.FaceDownState as FaceDownState
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Case" $ do
  solveSpec s registry
  solvedAbilitySpec s registry
  reductionSpec s registry
  pilferedProofSpec s registry

-- alice's board: the Case on the battlefield, Forests enough for every cast the
-- caller makes, `fogs` Fogs in hand and a stocked library, in a main phase with
-- priority. Fog is the instant throughout because its resolution touches nothing
-- these assertions read -- a combat-damage prevention effect in a main phase --
-- so a hand or library count moves only when the Case's own ability moves it.
--
-- The Case is placed rather than cast: CR 719 says nothing about how a Case
-- arrives, and its abilities are not entry triggers.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
board ransackedLab forest fog fogs =
  let (caseId, withCase) = S.addPermanent ransackedLab S.alice (S.landsInPlay forest 12)
      stocked = List.foldl' (\g _ -> snd (S.addLibraryCard fog S.alice g)) withCase [1 .. 10 :: Int]
      (fogIds, filled) = List.foldl' (\(ids, g) _ -> let (i, g2) = S.addHandCard fog S.alice g in (ids <> [i], g2)) ([], stocked) [1 .. fogs]
   in ( caseId,
        fogIds,
        filled
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- One cast, resolved along with anything it triggered.
castOne :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castOne oid gs = resolveAll (settle (S.runPure S.identityAnswer gs (S.cast S.alice oid)))

castEach :: [ObjectId.ObjectId] -> GameState.GameState -> GameState.GameState
castEach oids gs = List.foldl' (flip castOne) gs oids

settle :: GameState.GameState -> GameState.GameState
settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

resolveAll :: GameState.GameState -> GameState.GameState
resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop

-- CR 603.2b's event for alice's end step, with the phase moved to match -- the
-- shape Pawl.TriggerSpec and Pawl.RoomSpec fire every step trigger with -- and
-- then the trigger placed and resolved.
throughEndStep :: GameState.GameState -> GameState.GameState
throughEndStep gs =
  resolveAll
    . settle
    $ Event.recordEvent
      (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice))
      (gs {GameState.phase = Phase.Ending EndingStep.EndStep})

-- CR 719.3b's marker itself, which no characteristic reports.
solvedness :: ObjectId.ObjectId -> GameState.GameState -> Bool
solvedness oid gs = maybe False (Set.member Designation.Solved . Object.designations) (Game.lookupObject oid gs)

librarySize :: GameState.GameState -> Int
librarySize gs = length (Game.zoneMembers Zone.Library S.alice gs)

-- CR 719.3a: "At the beginning of your end step, if [condition] and this Case is
-- not solved, this Case becomes solved."
solveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
solveSpec s registry = Spec.describe s "To solve" $ do
  -- The two boards differ in ONE thing: whether the fourth Fog was cast. Three
  -- casts is the falsifier for a to-solve trigger that ignores its condition and
  -- solves at the first end step it sees.
  Spec.it s "CR 719.3a the condition gates the solve" $ do
    ransackedLab <- S.printingOf s registry "Case of the Ransacked Lab"
    forest <- S.printingOf s registry "Forest"
    fog <- S.printingOf s registry "Fog"
    let solvedAfter n =
          let (caseId, fogIds, gs) = board ransackedLab forest fog n
           in solvedness caseId (throughEndStep (castEach fogIds gs))
    Spec.assertEqWith s "three instants: still unsolved" (solvedAfter 3) False
    Spec.assertEqWith s "four instants: solved" (solvedAfter 4) True
  -- The other half of rule 719.3a's sentence: the solve happens AT THE BEGINNING
  -- OF THE END STEP, not on the cast that satisfies the condition. Without this
  -- the case above passes for an implementation that solves as soon as the
  -- fourth spell is cast.
  Spec.it s "CR 719.3a the fourth cast does not solve it by itself" $ do
    ransackedLab <- S.printingOf s registry "Case of the Ransacked Lab"
    forest <- S.printingOf s registry "Forest"
    fog <- S.printingOf s registry "Fog"
    let (caseId, fogIds, gs) = board ransackedLab forest fog 4
        cast = castEach fogIds gs
    Spec.assertEqWith s "four casts, no end step yet" (solvedness caseId cast) False
    Spec.assertEqWith s "and the end step is what solves it" (solvedness caseId (throughEndStep cast)) True

-- CR 719.3c / CR 702.169c: "Solved -- [Ability text]" for a triggered ability
-- means the ability triggers only if the Case is solved.
solvedAbilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
solvedAbilitySpec s registry = Spec.describe s "Solved" $ do
  -- Both directions on ONE board, which is what keeps the negative from passing
  -- because the Case never entered or the Fog never resolved: the same Case, the
  -- same instant and the same seat draw nothing before the solve and one card
  -- after it.
  Spec.it s "CR 719.3c the solved ability functions only once the Case is solved" $ do
    ransackedLab <- S.printingOf s registry "Case of the Ransacked Lab"
    forest <- S.printingOf s registry "Forest"
    fog <- S.printingOf s registry "Fog"
    let (caseId, fogIds, gs) = board ransackedLab forest fog 5
        (unsolvedCasts, lastFog) = splitAt 4 fogIds
        cast = castEach unsolvedCasts gs
        solved = throughEndStep cast
        after = castEach lastFog solved
    Spec.assertEqWith s "the library is untouched by four casts while unsolved" (librarySize cast) (librarySize gs)
    Spec.assertEqWith s "and those four casts are what solved it" (solvedness caseId solved) True
    Spec.assertEqWith s "the fifth cast, now solved, draws one" (librarySize after) (librarySize gs - 1)
    -- The draw's other end: the hand spent a Fog and took a card back, so it is
    -- the same size it was. Named as literals so the two sides cannot coincide
    -- silently.
    Spec.assertEqWith s "one Fog left in hand before the fifth cast" (S.handSize S.alice cast) 1
    Spec.assertEqWith s "and one card in hand after it, the one drawn" (S.handSize S.alice after) 1
  -- CR 719.3b: "Once a permanent becomes solved, it stays solved until it leaves
  -- the battlefield." THE assertion that proves the designation is stored state
  -- rather than a re-read of the to-solve condition: the next turn's log is
  -- empty (Engine.beginTurnOf clears it), so a Case whose Solved ability were
  -- gated on the four-spell count directly would go quiet here.
  Spec.it s "CR 719.3b the Case stays solved into a turn that casts nothing" $ do
    ransackedLab <- S.printingOf s registry "Case of the Ransacked Lab"
    forest <- S.printingOf s registry "Forest"
    fog <- S.printingOf s registry "Fog"
    let (caseId, fogIds, gs) = board ransackedLab forest fog 5
        (thisTurn, nextTurnFog) = splitAt 4 fogIds
        solved = throughEndStep (castEach thisTurn gs)
        handed =
          (Engine.beginTurnOf S.alice solved)
            { GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        after = castEach nextTurnFog handed
    Spec.assertEqWith s "no spell has been cast this turn" (length (S.eventsOf handed)) 0
    Spec.assertEqWith s "the Case is still solved" (solvedness caseId handed) True
    Spec.assertEqWith s "and its ability still draws" (librarySize after) (librarySize handed - 1)

-- The Case's first clause, which is not a CR 719 rule at all -- ordinary printed
-- text, and the same PlayerEffect.ReduceSpellCost Baral, Chief of Compliance
-- prints. Here to prove the transcription rather than the capability: a card
-- whose reduction decoded wrong would still pass every case above, since Fog's
-- {G} has no generic symbol to reduce.
reductionSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
reductionSpec s registry = Spec.describe s "The cost reduction" $ do
  -- Two boards differing in exactly one thing: whether the Case is on the
  -- battlefield. One Island either way, so an unreduced {1}{U} is unaffordable
  -- and a reduced one is not.
  Spec.it s "instant and sorcery spells you cast cost {1} less to cast" $ do
    ransackedLab <- S.printingOf s registry "Case of the Ransacked Lab"
    island <- S.printingOf s registry "Island"
    skeins <- S.printingOf s registry "Vision Skeins"
    let (bare, spellId) = S.handOne skeins (S.landsInPlay island 1)
        withCase = snd (S.addPermanent ransackedLab S.alice bare)
    Spec.assertEqWith s "one Island alone cannot pay {1}{U}" (S.castable S.alice spellId bare) False
    Spec.assertEqWith s "with the Case out it can" (S.castable S.alice spellId withCase) True

-- CR 603.1b: ONE triggered ability with two trigger conditions, whose branches
-- name the same subject -- "Whenever a Detective you control enters or is turned
-- face up, put a +1/+1 counter on IT". Event.eventBindings answers the union of
-- what the branches bind off the matched event, and CR 400.7e's `became` is what
-- both of them bind, so the payload finds the subject either way.
--
-- Basilica Stalker, MKM 78 -- {5}{B} 3/4 Vampire Detective with flying, a combat
-- damage trigger and disguise {4}{B} -- is the Detective throughout, and reaches
-- both branches off one card: CR 702.168b lists a 2/2 with ward {2} and no
-- subtypes, so it is NOT a Detective as it enters and IS one as CR 702.168d
-- turns it face up.
pilferedProofSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
pilferedProofSpec s registry = Spec.describe s "Case of the Pilfered Proof" $ do
  -- CR 708.7's branch. THE ASSERTION IS BY OBJECT IDENTITY and against three
  -- permanents, because a partial fix has two other places to put the counter:
  -- CR 113.7a's source slot names the Case, and the bystander Detective is what
  -- a filter-only implementation would reach.
  Spec.it s "CR 708.7 the counter lands on the Detective that was turned face up" $ do
    proof <- S.printingOf s registry "Case of the Pilfered Proof"
    stalker <- S.printingOf s registry "Basilica Stalker"
    swamp <- S.printingOf s registry "Swamp"
    let (caseId, bystander, base) = proofBoard proof stalker swamp 8
        (gs, handId) = S.handOne stalker base
        cast = S.runPure S.identityAnswer gs (Cast.castSpell S.manaPerformer S.alice handId (S.printingName stalker) disguised >> Stack.resolveTop)
    case enteredOne gs cast of
      Nothing -> Spec.assertFailure s "the disguise cast did not reach the battlefield"
      Just hidden -> do
        let down = resolveAll (settle cast)
            up = resolveAll (settle (S.runPure S.identityAnswer down (FaceDown.turnFaceUp S.manaPerformer S.alice TurnUpProcedure.Disguise hidden)))
        -- The precondition the turn-up assertion rests on, and the reason one
        -- card reaches both branches: entering face down triggered nothing.
        Spec.assertEqWith s "CR 702.168b a face-down 2/2 has no subtypes, so entering put no counter on it" (S.counterOf CounterKind.PlusOnePlusOne hidden down) 0
        Spec.assertEqWith s "CR 603.1b the Detective that was turned face up has the +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne hidden up) 1
        Spec.assertEqWith s "the Detective that was already out has none" (S.counterOf CounterKind.PlusOnePlusOne bystander up) 0
        Spec.assertEqWith s "and CR 113.7a's source, the Case itself, has none" (S.counterOf CounterKind.PlusOnePlusOne caseId up) 0
  -- The other branch, and its "you control" limb, as ONE pair of boards differing
  -- in the seat the Detective entered under. Same card, same board, same moment.
  Spec.it s "CR 603.1b the counter lands on the Detective that entered, and on nobody else's" $ do
    proof <- S.printingOf s registry "Case of the Pilfered Proof"
    stalker <- S.printingOf s registry "Basilica Stalker"
    swamp <- S.printingOf s registry "Swamp"
    let (caseId, bystander, base) = proofBoard proof stalker swamp 4
        under pid = let (oid, g) = S.entersWithTrigger stalker pid base in (oid, resolveAll (settle g))
        (mineId, mine) = under S.alice
        (theirsId, theirs) = under S.bob
    Spec.assertEqWith s "alice's Detective entering takes the +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne mineId mine) 1
    Spec.assertEqWith s "the Detective that was already out takes none" (S.counterOf CounterKind.PlusOnePlusOne bystander mine) 0
    Spec.assertEqWith s "nor does CR 113.7a's source, the Case itself" (S.counterOf CounterKind.PlusOnePlusOne caseId mine) 0
    Spec.assertEqWith s "and bob's Detective entering takes none" (S.counterOf CounterKind.PlusOnePlusOne theirsId theirs) 0
  -- CR 702.169b's static reading of "Solved --", over CR 614.1a's "those tokens
  -- plus a Clue token are created instead". Thraben Inspector's entry makes one
  -- Clue; the Case, once solved, makes it two.
  --
  -- Two boards differing in ONE thing: whether a third Detective is on the
  -- battlefield when the end step arrives, which is rule 719.3a's condition.
  Spec.it s "CR 702.169b a solved Case adds a Clue to a token creation" $ do
    proof <- S.printingOf s registry "Case of the Pilfered Proof"
    stalker <- S.printingOf s registry "Basilica Stalker"
    swamp <- S.printingOf s registry "Swamp"
    inspector <- S.printingOf s registry "Thraben Inspector"
    let clue = CardName.MkCardName (Text.pack "Clue Token")
        run detectives =
          let (caseId, withCase) = S.addPermanent proof S.alice (S.landsInPlay swamp 4)
              filled = List.foldl' (\g _ -> snd (S.addPermanent stalker S.alice g)) withCase [1 .. detectives :: Int]
              ready = filled {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
              solved = throughEndStep ready
              after = resolveAll (settle (snd (S.entersWithTrigger inspector S.alice solved)))
           in (solvedness caseId solved, S.countOnBattlefieldByName clue S.alice after)
    Spec.assertEqWith s "CR 614.1a solved: those tokens plus a Clue token" (snd (run 3)) 2
    Spec.assertEqWith s "unsolved: the investigate's one Clue and no more" (snd (run 2)) 1
    Spec.assertEqWith s "CR 719.3a three Detectives solve it" (fst (run 3)) True
    Spec.assertEqWith s "two do not" (fst (run 2)) False

-- alice's board for the Case: the Case itself, ONE Detective already on the
-- battlefield, and `lands` Swamps. The Detective is placed rather than entered,
-- so nothing has triggered and it carries no counter -- which is what makes the
-- by-identity assertions above able to tell it from the subject.
proofBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
proofBoard proof detective swamp lands =
  let (caseId, withCase) = S.addPermanent proof S.alice (S.landsInPlay swamp lands)
      (bystander, withBystander) = S.addPermanent detective S.alice withCase
   in ( caseId,
        bystander,
        withBystander
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 702.168b's facing, listing and all: a disguise cast is face down with ward
-- {2}, which CR 708.2a's bare default does not carry.
disguised :: Facing.Facing
disguised = Facing.FaceDown FaceDownState.MkFaceDownState {FaceDownState.reason = FaceDownReason.Disguised, FaceDownState.listed = FaceDownCharacteristics.disguisedValue}

-- The permanent a move added to the battlefield, or Nothing when it added none
-- or several -- which names the new incarnation without asking what card is
-- under it, the whole point for a face-down one.
enteredOne :: GameState.GameState -> GameState.GameState -> Maybe ObjectId.ObjectId
enteredOne before after =
  case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
    [oid] -> Just oid
    _ -> Nothing
