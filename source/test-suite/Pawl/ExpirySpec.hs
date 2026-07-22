-- Covers Pawl.Expiry and Pawl.Type.Expiry: the printed Duration -> stored Expiry
-- arming (CR 611.2), the sweeps that end a duration (CR 514.2, 611.2a, 611.2b),
-- and the two gate cards (Master Thief, Hag of Inner Weakness).
module Pawl.ExpirySpec where

import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Expiry as Expiry.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.StateCondition as StateCondition
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- A stored continuous effect with a chosen expiry, over a stand-in target.
-- Object id 998 is the stand-in source (Support.withEffectAt's posture);
-- nothing here reads the source's characteristics.
effectWith :: Expiry.Type.Expiry -> GameState.GameState -> GameState.GameState
effectWith expiry gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 998,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = expiry,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Flying,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton (ObjectId.MkObjectId 999))
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- A stand-in source and state for the three arms that don't consult either --
-- only Duration.ForAsLongAs reads them.
armGs :: GameState.GameState
armGs = Setup.emptyGame S.bothPlayers

armSource :: ObjectId.ObjectId
armSource = ObjectId.MkObjectId 999

armTests :: Tasty.TestTree
armTests =
  Tasty.testGroup
    "Arm"
    [ HU.testCase "CR 514.2 an until-end-of-turn duration arms to AtCleanup" $
        HU.assertEqual "armed" (Just Expiry.Type.AtCleanup) (Expiry.arm S.alice armSource Duration.UntilEndOfTurn armGs),
      HU.testCase "CR 611.2a an indefinite duration arms to Never" $
        HU.assertEqual "armed" (Just Expiry.Type.Never) (Expiry.arm S.alice armSource Duration.Indefinite armGs),
      HU.testCase "CR 611.2a / 109.5 'until your next turn' bakes the controller" $
        HU.assertEqual "armed" (Just (Expiry.Type.AtTurnOf S.alice)) (Expiry.arm S.alice armSource Duration.UntilYourNextTurn armGs)
    ]

handoffTests :: Tasty.TestTree
handoffTests =
  Tasty.testGroup
    "DropAtHandoff"
    [ HU.testCase "CR 611.2a an AtTurnOf effect ends as that player's turn begins, not before" $
        let gs0 = Setup.emptyGame S.bothPlayers
            -- alice is the active player; the effect ends at ALICE's next turn.
            armed = effectWith (Expiry.Type.AtTurnOf S.alice) gs0
            bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
            alicesTurn = S.runPure S.identityAnswer bobsTurn Engine.handoffTurn
         in do
              HU.assertEqual "alice is active when it is created" S.alice (GameState.activePlayer armed)
              HU.assertEqual "it survives the creating turn's handoff" 1 (length (GameState.continuousEffects bobsTurn))
              HU.assertEqual "bob is active" S.bob (GameState.activePlayer bobsTurn)
              HU.assertEqual "it ends as alice's next turn begins" [] (GameState.continuousEffects alicesTurn),
      HU.testCase "CR 514.2 does not touch an AtTurnOf effect" $
        let gs0 = Setup.emptyGame S.bothPlayers
            armed = effectWith (Expiry.Type.AtTurnOf S.alice) gs0
         in HU.assertEqual "survives cleanup" 1 (length (GameState.continuousEffects (Expiry.dropAtCleanup armed))),
      HU.testCase "CR 611.2a the sweep is scoped to the player whose turn began" $
        let gs0 = Setup.emptyGame S.bothPlayers
            armed = effectWith (Expiry.Type.AtTurnOf S.bob) gs0
            bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
         in HU.assertEqual "bob's turn ends bob's effect" [] (GameState.continuousEffects bobsTurn)
    ]

cleanupTests :: Cards.Cards -> Tasty.TestTree
cleanupTests cards =
  Tasty.testGroup
    "DropAtCleanup"
    [ HU.testCase "CR 514.2 cleanup drops an AtCleanup continuous effect and keeps a Never one" $
        let gs0 = Setup.emptyGame S.bothPlayers
            gs1 = effectWith Expiry.Type.Never (effectWith Expiry.Type.AtCleanup gs0)
            after = Expiry.dropAtCleanup gs1
         in do
              HU.assertEqual "two stored before" 2 (length (GameState.continuousEffects gs1))
              HU.assertEqual "one survives" [Expiry.Type.Never] (map ContinuousEffect.expiry (GameState.continuousEffects after)),
      HU.testCase "CR 514.2 the same sweep drops an AtCleanup floating replacement" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (oid, gs1) = S.addPiker cards S.alice gs0
            shielded = S.addRegenShield oid gs1
            after = Expiry.dropAtCleanup shielded
         in do
              HU.assertEqual "one shield before" 1 (length (GameState.replacements shielded))
              HU.assertEqual "none after" [] (map ActiveReplacement.expiry (GameState.replacements after))
    ]

-- A stored continuous effect whose expiry is a live condition over `src`,
-- affecting `target`. The Master Thief shape, hand-built so the sweep can be
-- tested before the card exists.
whileEffect :: ObjectId.ObjectId -> ObjectId.ObjectId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
whileEffect src target you gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = src,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.Type.While you StateCondition.YouControlSource,
            ContinuousEffect.modification = Modification.SetController you,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

conditionalTests :: Cards.Cards -> Tasty.TestTree
conditionalTests cards =
  let board =
        let gs0 = Setup.emptyGame S.bothPlayers
            (srcId, gs1) = S.addPiker cards S.alice gs0
            (targetId, gs2) = S.addCreature (Cards.warMammothPrinting cards) S.bob gs1
         in (srcId, targetId, whileEffect srcId targetId S.alice gs2)
   in Tasty.testGroup
        "Conditional"
        [ HU.testCase "CR 611.2b YouControlSource holds while the source is controlled" $
            let (srcId, _, gs) = board
             in HU.assertBool "holds" (Event.stateHolds S.alice srcId StateCondition.YouControlSource gs),
          HU.testCase "CR 613.1b it stops holding when another player gains control of the source" $
            let (srcId, _, gs) = board
                stolen = S.giveControl srcId S.bob gs
             in HU.assertBool "no longer holds" (not (Event.stateHolds S.alice srcId StateCondition.YouControlSource stolen)),
          HU.testCase "CR 400.7 it stops holding when the source leaves the battlefield" $
            let (srcId, _, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
             in HU.assertBool "no longer holds" (not (Event.stateHolds S.alice srcId StateCondition.YouControlSource gone)),
          HU.testCase "CR 611.2b arm returns Nothing when the condition is already false" $
            let (srcId, _, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
             in HU.assertEqual
                  "never starts"
                  Nothing
                  (Expiry.arm S.alice srcId (Duration.ForAsLongAs StateCondition.YouControlSource) gone),
          HU.testCase "CR 611.2b arm returns a While when the condition holds now" $
            let (srcId, _, gs) = board
             in HU.assertEqual
                  "starts"
                  (Just (Expiry.Type.While S.alice StateCondition.YouControlSource))
                  (Expiry.arm S.alice srcId (Duration.ForAsLongAs StateCondition.YouControlSource) gs),
          HU.testCase "CR 611.2b the sweep DELETES the effect once the condition fails" $
            let (srcId, targetId, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
                (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
             in do
                  HU.assertEqual "alice held it while the source stood" (Just S.alice) (Projection.controllerOf targetId gs)
                  HU.assertBool "the sweep reports a change" changed
                  HU.assertEqual "the effect is gone, not masked" [] (GameState.continuousEffects swept)
                  HU.assertEqual "control reverted" (Just S.bob) (Projection.controllerOf targetId swept),
          HU.testCase "CR 611.2b a sweep that changes nothing reports False" $
            let (_, _, gs) = board
                (changed, _) = Engine.runGamePure S.identityAnswer gs Expiry.sweepConditional
             in HU.assertBool "no change" (not changed),
          HU.testCase "CR 704.3 settleForPriority runs the sweep" $
            let (srcId, targetId, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
                settled = S.runPure S.identityAnswer gone Engine.settleForPriority
             in HU.assertEqual "control reverted at the settle" (Just S.bob) (Projection.controllerOf targetId settled)
        ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.ExpirySpec" [armTests, cleanupTests cards, handoffTests, conditionalTests cards]
