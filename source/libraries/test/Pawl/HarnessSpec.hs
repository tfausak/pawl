{-# LANGUAGE GADTs #-}

-- Covers CR 701.64's harness and CR 702.186's infinity (∞), which need no
-- engine subsystem of their own: harnessed is Pawl.Types.Designation's
-- Harnessed arm, written by Effect.Designate (CR 701.64a's "if this permanent
-- isn't harnessed" is its transition guard), and an ∞ ability is the static
-- grant CR 702.186b says it means, gated on Quantity.HasDesignation -- the
-- shape a Class level or a Case's "Solved --" already takes. The Mind Stone
-- carries both.
module Pawl.HarnessSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan

spec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Harness" $ do
  -- Two boards differing in ONE thing: whether alice activated "{5}{W}, {T}:
  -- Harness The Mind Stone" before her end step. Both hold the same six Plains
  -- and the same Hill Giant, so the ∞ trigger's target is on offer either way.
  -- A flickered Giant is a new object (CR 400.7), so its old id leaving the
  -- battlefield is what says the trigger resolved.
  Spec.it s "CR 702.186b the infinity ability exists only once CR 701.64a has harnessed the Stone" $ do
    stone <- S.printingOf s registry "The Mind Stone"
    plains <- S.printingOf s registry "Plains"
    hillGiant <- S.printingOf s registry "Hill Giant"
    case Face.activatedAbilities (S.combinedFace stone) of
      [_, harness] -> do
        let (stoneId, gs1) = S.addPermanent stone S.alice (S.landsInPlay plains 6)
            (giantId, gs2) = S.addPermanent hillGiant S.alice gs1
            ready = gs2 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
            harnessed = S.settleSba (S.runPure S.identityAnswer ready (do Activate.activateAbility S.alice stoneId harness; Stack.resolveTop))
            giantName = CardName.MkCardName (Text.pack "Hill Giant")
            unharnessedAfter = throughEndStep giantId ready
            harnessedAfter = throughEndStep giantId harnessed
        Spec.assertBool s (Set.member giantId (GameState.battlefield unharnessedAfter)) "unharnessed: the Giant is untouched at the end step"
        Spec.assertBool s (not (Set.member giantId (GameState.battlefield harnessedAfter))) "harnessed: the end-step trigger flickered the Giant"
        Spec.assertEqWith s "and returned it to the battlefield" (S.countOnBattlefieldByName giantName S.alice harnessedAfter) 1
      _ -> Spec.assertFailure s "The Mind Stone should print two activated abilities"

-- CR 603.2b's event for alice's end step, with the phase moved to match -- the
-- shape Pawl.CaseSpec fires its end-step triggers with -- and then the trigger
-- placed and resolved, aimed at `target`.
throughEndStep :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
throughEndStep target gs =
  S.runPure (aimedAt target) stepped (Engine.settleForPriority >> Engine.priorityLoop)
  where
    stepped =
      Event.recordEvent
        (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice))
        (gs {GameState.phase = Phase.Ending EndingStep.EndStep, GameState.priority = Just S.alice})

aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt oid p = case p of
  Prompt.AnnounceTargets _ _ _ slots -> fmap (const 1) slots
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (\r -> Recipient.objectOf r == Just oid) sets
  _ -> S.identityAnswer p
