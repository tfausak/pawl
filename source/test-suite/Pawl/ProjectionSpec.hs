-- Covers Pawl.Projection: the single-effect layer fold -- CR 613 layer order and
-- CR 613.7 within-layer timestamp order, with no CR 613.8 dependency (M3c). Uses
-- directly-constructed continuous effects so the engine is proven before any card
-- wiring; real-card behavior lands in later tasks and DamageSpec.
module Pawl.ProjectionSpec where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Sba as Sba
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Layer as Layer
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
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

-- The object timestamp of the (single) Humility on the battlefield.
humilityTimestamp :: GameState.GameState -> Timestamp.Timestamp
humilityTimestamp gs =
  let isHum oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard p -> Printing.card p == Printing.card Card.humilityPrinting
      hums = filter isHum (Set.toList (GameState.battlefield gs))
      stampOf oid = fmap Object.timestamp (Game.lookupObject oid gs)
   in case Maybe.mapMaybe stampOf hums of
        t : _ -> t
        [] -> Timestamp.MkTimestamp 0

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
              HU.assertEqual "Piker back to base toughness" (Just 1) (Projection.toughnessOf pikerId afterCleanup),
      HU.testCase "CR 613 Humility makes every creature 1/1 with no abilities" $
        let (flyerId, gs0) = S.addCreature Card.birdMaidenPrinting S.bob (S.mountainsInPlay 1)
            gs = S.withHumility gs0
         in do
              HU.assertEqual "power 1" (Just 1) (Projection.powerOf flyerId gs)
              HU.assertEqual "toughness 1" (Just 1) (Projection.toughnessOf flyerId gs)
              HU.assertBool "no flying" (not (Projection.hasKeyword Keyword.Flying flyerId gs)),
      HU.testCase "CR 704.5g Humility's toughness drop makes an already-damaged creature die" $
        let (mammothId, gs0) = S.addCreature Card.warMammothPrinting S.bob (S.mountainsInPlay 1)
            damaged = S.markDamage mammothId 2 gs0
            underHumility = S.withHumility damaged
            afterSba = Sba.checkStateBasedActions underHumility
         in do
              HU.assertEqual "survives at 3/3 with 2 marked" (Just 3) (Projection.toughnessOf mammothId damaged)
              HU.assertEqual "no creature survives once toughness is 1" 0 (S.creaturesInPlay S.bob afterSba),
      HU.testCase "CR 613 layer order: Giant Growth on a Humility'd Piker is 4/4" $
        let base = S.landsInPlay Card.forestPrinting 1
            (pikerId, withPiker) = S.addPiker S.alice base
            withHum = S.withHumility withPiker
            (gs, ggId) = S.handOne Card.giantGrowthPrinting withHum
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice ggId))
            resolved = Stack.resolveTop cast
         in do
              -- Layer 7b (set 1/1) before 7c (+3/+3): 1 then +3 = 4.
              HU.assertEqual "power" (Just 4) (Projection.powerOf pikerId resolved)
              HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf pikerId resolved),
      HU.testCase "CR 611 Serpent's Gift grants deathtouch to its target" $
        -- {2}{G} needs 3 total mana; 3 Forests, not 2 (a brief fixture bug --
        -- 2 Forests only pay {1}{G}, leaving the spell uncast and the assertion
        -- vacuously true off the base card's native trample).
        let base = S.landsInPlay Card.forestPrinting 3
            (mammothId, withMammoth) = S.addCreature Card.warMammothPrinting S.alice base
            (gs, sgId) = S.handOne Card.serpentsGiftPrinting withMammoth
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice sgId))
            resolved = Stack.resolveTop cast
         in do
              HU.assertBool "keeps trample" (Projection.hasKeyword Keyword.Trample mammothId resolved)
              HU.assertBool "gains deathtouch" (Projection.hasKeyword Keyword.Deathtouch mammothId resolved),
      HU.testCase "CR 613.7 layer 6: a grant older than Humility is erased; newer survives" $
        -- War Mammoth and Humility on the battlefield; a directly-built
        -- Serpent's-Gift effect (GainKeyword Deathtouch, the same value the card
        -- creates) whose timestamp straddles Humility's object timestamp, to
        -- witness BOTH orders of CR 613.7 in layer 6. h-1 and h+1 make the
        -- relative order exact, not a guess.
        let (mammothId, gs0) = S.addCreature Card.warMammothPrinting S.bob (S.mountainsInPlay 1)
            withHum = S.withHumility gs0
            Timestamp.MkTimestamp h = humilityTimestamp withHum
            olderGrant = withEffect mammothId (Timestamp.MkTimestamp (h - 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
            newerGrant = withEffect mammothId (Timestamp.MkTimestamp (h + 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
         in do
              HU.assertBool "grant before Humility: erased" (not (Projection.hasKeyword Keyword.Deathtouch mammothId olderGrant))
              HU.assertBool "grant after Humility: survives" (Projection.hasKeyword Keyword.Deathtouch mammothId newerGrant),
      HU.testCase "projected type line: a Piker is a Creature - Goblin Warrior" $
        let (oid, gs) = S.addPiker S.bob (S.mountainsInPlay 1)
         in do
              HU.assertBool "is a creature" (Projection.isCreatureOf oid gs)
              HU.assertEqual "card types" (Set.singleton CardType.Creature) (Projection.cardTypesOf oid gs)
              HU.assertEqual "subtypes" (Set.fromList [Subtype.Goblin, Subtype.Warrior]) (Projection.subtypesOf oid gs),
      HU.testCase "projected type line: a Mountain is a Land - Mountain, not a creature" $
        let gs = S.mountainsInPlay 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
         in do
              HU.assertBool "not a creature" (not (Projection.isCreatureOf landId gs))
              HU.assertEqual "subtypes" (Set.singleton Subtype.Mountain) (Projection.subtypesOf landId gs),
      HU.testCase "CR 613.1d layer 4: the three type-changing modifications are Type" $ do
        HU.assertEqual "set land subtype" Layer.Type (Projection.layer (Modification.SetLandSubtype Subtype.Mountain))
        HU.assertEqual "add land subtype" Layer.Type (Projection.layer (Modification.AddLandSubtype Subtype.Swamp))
        HU.assertEqual "add card type" Layer.Type (Projection.layer (Modification.AddCardType CardType.Creature)),
      HU.testCase "CR 613.1d AddLandSubtype gives a Forest the Swamp subtype" $
        let gs0 = S.landsInPlay Card.forestPrinting 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.AddLandSubtype Subtype.Swamp) gs0
         in HU.assertEqual "Forest and Swamp" (Set.fromList [Subtype.Forest, Subtype.Swamp]) (Projection.subtypesOf landId gs),
      HU.testCase "CR 305.7 SetLandSubtype sets a Forest to only Mountain" $
        let gs0 = S.landsInPlay Card.forestPrinting 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.SetLandSubtype Subtype.Mountain) gs0
         in HU.assertEqual "only Mountain" (Set.singleton Subtype.Mountain) (Projection.subtypesOf landId gs),
      HU.testCase "CR 613.1d AddCardType makes a land a creature" $
        let gs0 = S.landsInPlay Card.forestPrinting 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.AddCardType CardType.Creature) gs0
         in HU.assertBool "now a creature" (Projection.isCreatureOf landId gs),
      HU.testCase "CR 202.3 SetBasePowerToughness ManaValue sets a Piker to its mana value ({1}{R} = 2)" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness Quantity.ManaValue Quantity.ManaValue) gs0
         in do
              HU.assertEqual "power = mana value" (Just 2) (Projection.powerOf oid gs)
              HU.assertEqual "toughness = mana value" (Just 2) (Projection.toughnessOf oid gs)
    ]
