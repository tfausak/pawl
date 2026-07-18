-- Covers Pawl.Projection: the single-effect layer fold -- CR 613 layer order and
-- CR 613.7 within-layer timestamp order, with no CR 613.8 dependency (M3c). Uses
-- directly-constructed continuous effects so the engine is proven before any card
-- wiring; real-card behavior lands in later tasks and DamageSpec.
module Pawl.ProjectionSpec where

import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Layer as Layer
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- alice has a Forest for mana, a Piker on the battlefield, and Giant Growth in
-- hand, in her main phase. Cast Giant Growth (identityAnswer targets the only
-- creature), then resolve it.
giantGrowthOnPiker :: (ObjectId.ObjectId, GameState.GameState)
giantGrowthOnPiker =
  let base = S.landsInPlay Card.forestPrinting 1
      (pikerId, withPiker) = S.addPiker S.alice base
      (gs, ggId) = S.handOne Card.giantGrowthPrinting withPiker
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice ggId))
      resolved = Stack.resolveTop cast
   in (pikerId, resolved)

-- Append a stored continuous effect affecting exactly `oid`, at timestamp `ts`.
withEffect :: ObjectId.ObjectId -> Timestamp.Timestamp -> Modification.Modification -> GameState.GameState -> GameState.GameState
withEffect oid ts m gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 998,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.duration = Duration.UntilEndOfTurn,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Projection"
    [ HU.testCase "layer classification matches CR 613.1" $ do
        HU.assertEqual "grant is layer 6" Layer.Ability (Projection.layer (Modification.GainKeyword Keyword.Deathtouch))
        HU.assertEqual "lose-all is layer 6" Layer.Ability (Projection.layer Modification.LoseAllAbilities)
        HU.assertEqual "set base is 7b" Layer.SetPT (Projection.layer (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)))
        HU.assertEqual "modify is 7c" Layer.ModifyPT (Projection.layer (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))),
      HU.testCase "no effects: the projection is the base printing (Piker is 2/1)" $
        let (oid, gs) = S.addPiker S.bob (S.mountainsInPlay 1)
         in do
              HU.assertEqual "power" (Just 2) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs)
              HU.assertBool "no keywords" (Set.null (Projection.keywordsOf oid gs)),
      HU.testCase "CR 613.3 layer 7c +3/+3 raises a Piker to 5/4" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
         in do
              HU.assertEqual "power" (Just 5) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 layer 6 GainKeyword adds deathtouch" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.GainKeyword Keyword.Deathtouch) gs0
         in HU.assertBool "has deathtouch" (Projection.hasKeyword Keyword.Deathtouch oid gs),
      HU.testCase "CR 613 layer 7b SetBasePowerToughness makes a Piker 1/1" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)) gs0
         in do
              HU.assertEqual "power" (Just 1) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 sublayer order: 7b then 7c, a set-1/1 Piker with +3/+3 is 4/4" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            -- Deliberately give 7c the EARLIER timestamp to prove layer beats
            -- timestamp: 7b still applies first.
            gs1 = withEffect oid (Timestamp.MkTimestamp 50) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)) gs1
         in do
              HU.assertEqual "power 1 then +3" (Just 4) (Projection.powerOf oid gs)
              HU.assertEqual "toughness 1 then +3" (Just 4) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613.7 within layer 6, timestamp order: later grant survives an earlier lose-all" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs1 = withEffect oid (Timestamp.MkTimestamp 10) Modification.LoseAllAbilities gs0
            gs = withEffect oid (Timestamp.MkTimestamp 20) (Modification.GainKeyword Keyword.Deathtouch) gs1
         in HU.assertBool "grant wins" (Projection.hasKeyword Keyword.Deathtouch oid gs),
      HU.testCase "CR 613.7 within layer 6, timestamp order: earlier grant is erased by a later lose-all" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs1 = withEffect oid (Timestamp.MkTimestamp 10) (Modification.GainKeyword Keyword.Deathtouch) gs0
            gs = withEffect oid (Timestamp.MkTimestamp 20) Modification.LoseAllAbilities gs1
         in HU.assertBool "lose-all wins" (not (Projection.hasKeyword Keyword.Deathtouch oid gs)),
      HU.testCase "a P/T modification never gives P/T to a land" $
        let gs0 = S.mountainsInPlay 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
         in HU.assertEqual "still no power" Nothing (Projection.powerOf landId gs),
      HU.testCase "CR 611 Giant Growth stores a +3/+3 effect; the Piker is 5/4" $
        let (pikerId, gs) = giantGrowthOnPiker
         in do
              HU.assertEqual "one stored effect" 1 (length (GameState.continuousEffects gs))
              HU.assertEqual "power" (Just 5) (Projection.powerOf pikerId gs)
              HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf pikerId gs),
      HU.testCase "CR 601.2c Giant Growth is uncastable with no creature to target" $
        let (gs, ggId) = S.handOne Card.giantGrowthPrinting (S.landsInPlay Card.forestPrinting 1)
         in HU.assertBool "no legal target, not castable" (not (Cast.castable S.alice ggId gs)),
      HU.testCase "CR 514.2 an until-end-of-turn effect wears off at cleanup" $
        let (pikerId, cast) = giantGrowthOnPiker
            -- Run the cleanup step's turn-based actions; the +3/+3 must be gone.
            afterCleanup = snd (Engine.runGamePure S.identityAnswer cast (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
         in do
              HU.assertEqual "effect dropped" [] (GameState.continuousEffects afterCleanup)
              HU.assertEqual "Piker back to base power" (Just 2) (Projection.powerOf pikerId afterCleanup)
              HU.assertEqual "Piker back to base toughness" (Just 1) (Projection.toughnessOf pikerId afterCleanup)
    ]
