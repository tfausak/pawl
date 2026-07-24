-- Covers Pawl.Projection: the single-effect layer fold -- CR 613 layer order and
-- CR 613.7 within-layer timestamp order, with no CR 613.8 dependency (M3c). Uses
-- directly-constructed continuous effects so the engine is proven before any card
-- wiring; real-card behavior lands in later tasks and DamageSpec.
module Pawl.ProjectionSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Replacement as Replacement
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Exclusion as Exclusion
import qualified Pawl.Type.Expiry as Expiry
-- Pawl.Type.Filter aliased Filter.Type: the evaluator Pawl.Filter already claims
-- the alias Filter above (documented phase exception).
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Layer as Layer
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- alice has a Forest for mana, a Piker on the battlefield, and Giant Growth in
-- hand, in her main phase. Cast Giant Growth (identityAnswer targets the only
-- creature), then resolve it.
giantGrowthOnPiker :: Cards.Cards -> (ObjectId.ObjectId, GameState.GameState)
giantGrowthOnPiker cards =
  let base = S.landsInPlay (Cards.forestPrinting cards) 1
      (pikerId, withPiker) = S.addPiker cards S.alice base
      (gs, ggId) = S.handOne (Cards.giantGrowthPrinting cards) withPiker
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice ggId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (pikerId, resolved)

-- Append a stored continuous effect over a dynamic set, at timestamp `ts`.
withDynamicEffect :: Affected.Affected -> Timestamp.Timestamp -> Modification.Modification -> GameState.GameState -> GameState.GameState
withDynamicEffect aff ts m gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 997,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = aff
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

-- The object timestamp of the (single) Humility on the battlefield.
humilityTimestamp :: Cards.Cards -> GameState.GameState -> Timestamp.Timestamp
humilityTimestamp cards gs =
  let isHum oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard p -> Printing.card p == Printing.card (Cards.humilityPrinting cards)
          Source.OfToken _ -> False
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
      hums = filter isHum (Set.toList (GameState.battlefield gs))
      stampOf oid = fmap Object.timestamp (Game.lookupObject oid gs)
   in case Maybe.mapMaybe stampOf hums of
        t : _ -> t
        [] -> Timestamp.MkTimestamp 0

-- Blood Moon, Urborg, and a Forest on the battlefield. `urborgFirst` controls
-- the timestamp order (fresh timestamps ascend with placement), to prove the
-- outcome is order-INDEPENDENT (CR 613.8 dependency overrides CR 613.7).
bloodMoonUrborg :: Cards.Cards -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
bloodMoonUrborg cards urborgFirst =
  let base = Setup.emptyGame S.bothPlayers
      (forestId, g1) = S.addCreature (Cards.forestPrinting cards) S.alice base
      place g =
        if urborgFirst
          then
            let (u, g') = S.addCreature (Cards.urborgPrinting cards) S.alice g
                (_, g'') = S.addCreature (Cards.bloodMoonPrinting cards) S.alice g'
             in (u, g'')
          else
            let (_, g') = S.addCreature (Cards.bloodMoonPrinting cards) S.alice g
                (u, g'') = S.addCreature (Cards.urborgPrinting cards) S.alice g'
             in (u, g'')
      (urborgId, gs) = place g1
   in (forestId, urborgId, gs)

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Projection"
    [ HU.testCase "layer classification matches CR 613.1" $ do
        HU.assertEqual "grant is layer 6" Layer.Ability (Projection.layer (Modification.GainKeyword Keyword.Deathtouch))
        HU.assertEqual "lose-all is layer 6" Layer.Ability (Projection.layer Modification.LoseAllAbilities)
        HU.assertEqual "set base is 7b" Layer.SetPT (Projection.layer (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)))
        HU.assertEqual "modify is 7c" Layer.ModifyPT (Projection.layer (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))),
      HU.testCase "no effects: the projection is the base printing (Piker is 2/1)" $
        let (oid, gs) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
         in do
              HU.assertEqual "power" (Just 2) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs)
              HU.assertBool "no keywords" (Set.null (Projection.keywordsOf oid gs)),
      HU.testCase "CR 613.3 layer 7c +3/+3 raises a Piker to 5/4" $
        let (oid, gs0) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
         in do
              HU.assertEqual "power" (Just 5) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 layer 6 GainKeyword adds deathtouch" $
        let (oid, gs0) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.GainKeyword Keyword.Deathtouch) gs0
         in HU.assertBool "has deathtouch" (Projection.hasKeyword Keyword.Deathtouch oid gs),
      HU.testCase "CR 613 layer 7b SetBasePowerToughness makes a Piker 1/1" $
        let (oid, gs0) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)) gs0
         in do
              HU.assertEqual "power" (Just 1) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 sublayer order: 7b then 7c, a set-1/1 Piker with +3/+3 is 4/4" $
        let (oid, gs0) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
            -- Deliberately give 7c the EARLIER timestamp to prove layer beats
            -- timestamp: 7b still applies first.
            gs1 = S.withEffectAt oid (Timestamp.MkTimestamp 50) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)) gs1
         in do
              HU.assertEqual "power 1 then +3" (Just 4) (Projection.powerOf oid gs)
              HU.assertEqual "toughness 1 then +3" (Just 4) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613.7 within layer 6, timestamp order: later grant survives an earlier lose-all" $
        let (oid, gs0) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
            gs1 = S.withEffectAt oid (Timestamp.MkTimestamp 10) Modification.LoseAllAbilities gs0
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 20) (Modification.GainKeyword Keyword.Deathtouch) gs1
         in HU.assertBool "grant wins" (Projection.hasKeyword Keyword.Deathtouch oid gs),
      HU.testCase "CR 613.7 within layer 6, timestamp order: earlier grant is erased by a later lose-all" $
        let (oid, gs0) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
            gs1 = S.withEffectAt oid (Timestamp.MkTimestamp 10) (Modification.GainKeyword Keyword.Deathtouch) gs0
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 20) Modification.LoseAllAbilities gs1
         in HU.assertBool "lose-all wins" (not (Projection.hasKeyword Keyword.Deathtouch oid gs)),
      HU.testCase "a P/T modification never gives P/T to a land" $
        let gs0 = S.mountainsInPlay cards 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
         in HU.assertEqual "still no power" Nothing (Projection.powerOf landId gs),
      HU.testCase "CR 611 Giant Growth stores a +3/+3 effect; the Piker is 5/4" $
        let (pikerId, gs) = giantGrowthOnPiker cards
         in do
              HU.assertEqual "one stored effect" 1 (length (GameState.continuousEffects gs))
              HU.assertEqual "power" (Just 5) (Projection.powerOf pikerId gs)
              HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf pikerId gs),
      HU.testCase "CR 601.2c Giant Growth is uncastable with no creature to target" $
        let (gs, ggId) = S.handOne (Cards.giantGrowthPrinting cards) (S.landsInPlay (Cards.forestPrinting cards) 1)
         in HU.assertBool "no legal target, not castable" (not (Cast.castable S.alice ggId gs)),
      HU.testCase "CR 514.2 an until-end-of-turn effect wears off at cleanup" $
        let (pikerId, cast) = giantGrowthOnPiker cards
            -- Run the cleanup step's turn-based actions; the +3/+3 must be gone.
            afterCleanup = snd (Engine.runGamePure S.identityAnswer cast (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
         in do
              HU.assertEqual "effect dropped" [] (GameState.continuousEffects afterCleanup)
              HU.assertEqual "Piker back to base power" (Just 2) (Projection.powerOf pikerId afterCleanup)
              HU.assertEqual "Piker back to base toughness" (Just 1) (Projection.toughnessOf pikerId afterCleanup),
      HU.testCase "CR 613 Humility makes every creature 1/1 with no abilities" $
        let (flyerId, gs0) = S.addCreature (Cards.birdMaidenPrinting cards) S.bob (S.mountainsInPlay cards 1)
            gs = S.withHumility cards gs0
         in do
              HU.assertEqual "power 1" (Just 1) (Projection.powerOf flyerId gs)
              HU.assertEqual "toughness 1" (Just 1) (Projection.toughnessOf flyerId gs)
              HU.assertBool "no flying" (not (Projection.hasKeyword Keyword.Flying flyerId gs)),
      HU.testCase "CR 613 layer 6: Humility strips a creature's activated abilities" $
        let (sorcId, g0) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            gs = S.withHumility cards g0
         in HU.assertEqual "no abilities under Humility" [] (Projection.abilitiesOf sorcId gs),
      HU.testCase "without Humility the ability is present" $
        let (sorcId, gs) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual "one ability" 1 (length (Projection.abilitiesOf sorcId gs)),
      HU.testCase "CR 704.5g Humility's toughness drop makes an already-damaged creature die" $
        let (mammothId, gs0) = S.addCreature (Cards.warMammothPrinting cards) S.bob (S.mountainsInPlay cards 1)
            damaged = S.markDamage mammothId 2 gs0
            underHumility = S.withHumility cards damaged
            afterSba = S.settleSba underHumility
         in do
              HU.assertEqual "survives at 3/3 with 2 marked" (Just 3) (Projection.toughnessOf mammothId damaged)
              HU.assertEqual "no creature survives once toughness is 1" 0 (S.creaturesInPlay S.bob afterSba),
      HU.testCase "CR 613 layer order: Giant Growth on a Humility'd Piker is 4/4" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (pikerId, withPiker) = S.addPiker cards S.alice base
            withHum = S.withHumility cards withPiker
            (gs, ggId) = S.handOne (Cards.giantGrowthPrinting cards) withHum
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice ggId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              -- Layer 7b (set 1/1) before 7c (+3/+3): 1 then +3 = 4.
              HU.assertEqual "power" (Just 4) (Projection.powerOf pikerId resolved)
              HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf pikerId resolved),
      HU.testCase "CR 611 Serpent's Gift grants deathtouch to its target" $
        -- {2}{G} needs 3 total mana; 3 Forests, not 2 (a brief fixture bug --
        -- 2 Forests only pay {1}{G}, leaving the spell uncast and the assertion
        -- vacuously true off the base card's native trample).
        let base = S.landsInPlay (Cards.forestPrinting cards) 3
            (mammothId, withMammoth) = S.addCreature (Cards.warMammothPrinting cards) S.alice base
            (gs, sgId) = S.handOne (Cards.serpentsGiftPrinting cards) withMammoth
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice sgId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertBool "keeps trample" (Projection.hasKeyword Keyword.Trample mammothId resolved)
              HU.assertBool "gains deathtouch" (Projection.hasKeyword Keyword.Deathtouch mammothId resolved),
      HU.testCase "CR 613.7 layer 6: a grant older than Humility is erased; newer survives" $
        -- War Mammoth and Humility on the battlefield; a directly-built
        -- Serpent's-Gift effect (GainKeyword Deathtouch, the same value the card
        -- creates) whose timestamp straddles Humility's object timestamp, to
        -- witness BOTH orders of CR 613.7 in layer 6. h-1 and h+1 make the
        -- relative order exact, not a guess.
        let (mammothId, gs0) = S.addCreature (Cards.warMammothPrinting cards) S.bob (S.mountainsInPlay cards 1)
            withHum = S.withHumility cards gs0
            Timestamp.MkTimestamp h = humilityTimestamp cards withHum
            olderGrant = S.withEffectAt mammothId (Timestamp.MkTimestamp (h - 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
            newerGrant = S.withEffectAt mammothId (Timestamp.MkTimestamp (h + 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
         in do
              HU.assertBool "grant before Humility: erased" (not (Projection.hasKeyword Keyword.Deathtouch mammothId olderGrant))
              HU.assertBool "grant after Humility: survives" (Projection.hasKeyword Keyword.Deathtouch mammothId newerGrant),
      HU.testCase "projected type line: a Piker is a Creature - Goblin Warrior" $
        let (oid, gs) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
         in do
              HU.assertBool "is a creature" (Projection.isCreatureOf oid gs)
              HU.assertEqual "card types" (Set.singleton CardType.Creature) (Projection.cardTypesOf oid gs)
              HU.assertEqual "subtypes" (Set.fromList [Subtype.Goblin, Subtype.Warrior]) (Projection.subtypesOf oid gs),
      HU.testCase "projected type line: a Mountain is a Land - Mountain, not a creature" $
        let gs = S.mountainsInPlay cards 1
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
      HU.testCase "CR 613.1c layer 3: ChangeSubtypeWord is Text" $
        HU.assertEqual "text layer" Layer.Text (Projection.layer (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island)),
      HU.testCase "CR 612.1 ChangeSubtypeWord rewrites a Forest's subtype to Island" $
        let gs0 = S.landsInPlay (Cards.forestPrinting cards) 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord Subtype.Forest Subtype.Island) gs0
         in HU.assertEqual "only Island" (Set.singleton Subtype.Island) (Projection.subtypesOf landId gs),
      HU.testCase "CR 612.2 ChangeSubtypeWord for an absent type is a no-op" $
        let gs0 = S.landsInPlay (Cards.forestPrinting cards) 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) gs0
         in HU.assertEqual "still Forest" (Set.singleton Subtype.Forest) (Projection.subtypesOf landId gs),
      HU.testCase "CR 613.1d AddLandSubtype gives a Forest the Swamp subtype" $
        let gs0 = S.landsInPlay (Cards.forestPrinting cards) 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.AddLandSubtype Subtype.Swamp) gs0
         in HU.assertEqual "Forest and Swamp" (Set.fromList [Subtype.Forest, Subtype.Swamp]) (Projection.subtypesOf landId gs),
      HU.testCase "CR 305.7 SetLandSubtype sets a Forest to only Mountain" $
        let gs0 = S.landsInPlay (Cards.forestPrinting cards) 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.SetLandSubtype Subtype.Mountain) gs0
         in HU.assertEqual "only Mountain" (Set.singleton Subtype.Mountain) (Projection.subtypesOf landId gs),
      HU.testCase "CR 613.1d AddCardType makes a land a creature" $
        let gs0 = S.landsInPlay (Cards.forestPrinting cards) 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.AddCardType CardType.Creature) gs0
         in HU.assertBool "now a creature" (Projection.isCreatureOf landId gs),
      HU.testCase "CR 202.3 SetBasePowerToughness ManaValue sets a Piker to its mana value ({1}{R} = 2)" $
        let (oid, gs0) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness Quantity.ManaValue Quantity.ManaValue) gs0
         in do
              HU.assertEqual "power = mana value" (Just 2) (Projection.powerOf oid gs)
              HU.assertEqual "toughness = mana value" (Just 2) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 affected-set reads the partial: a layer-4 creature-add is seen by a layer-6 Matching (HasCardType Creature) grant" $
        let gs0 = S.landsInPlay (Cards.forestPrinting cards) 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            -- Layer 4 makes the land a creature; layer 6 grants flying to all
            -- creatures. The grant reaches the land ONLY because the affected set
            -- is evaluated after layer 4.
            gs1 = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.AddCardType CardType.Creature) gs0
            gs = withDynamicEffect (Affected.Matching Exclusion.IncludesSource (Filter.Type.HasCardType CardType.Creature)) (Timestamp.MkTimestamp 200) (Modification.GainKeyword Keyword.Flying) gs1
         in HU.assertBool "land gained flying because it became a creature" (Projection.hasKeyword Keyword.Flying landId gs),
      HU.testCase "CR 305.7/613.8 Blood Moon strips Urborg: Urborg is only a Mountain (Blood Moon older)" $
        let (_, urborgId, gs) = bloodMoonUrborg cards False
         in HU.assertEqual "Urborg subtypes" (Set.singleton Subtype.Mountain) (Projection.subtypesOf urborgId gs),
      HU.testCase "CR 305.7/613.8 Blood Moon strips Urborg: Urborg is only a Mountain (Urborg older)" $
        let (_, urborgId, gs) = bloodMoonUrborg cards True
         in HU.assertEqual "Urborg subtypes, order-independent" (Set.singleton Subtype.Mountain) (Projection.subtypesOf urborgId gs),
      HU.testCase "CR 613.8 Urborg's stripped ability adds no Swamp to a Forest (Blood Moon older)" $
        let (forestId, _, gs) = bloodMoonUrborg cards False
         in HU.assertEqual "Forest stays a Forest" (Set.singleton Subtype.Forest) (Projection.subtypesOf forestId gs),
      HU.testCase "CR 613.8 Urborg's stripped ability adds no Swamp to a Forest (Urborg older)" $
        let (forestId, _, gs) = bloodMoonUrborg cards True
         in HU.assertEqual "Forest stays a Forest, order-independent" (Set.singleton Subtype.Forest) (Projection.subtypesOf forestId gs),
      HU.testCase "CR 612 hacking Blood Moon Mountain->Island: nonbasic lands become Islands (hack newer)" $
        let base = Setup.emptyGame S.bothPlayers
            (nonbasicId, g1) = S.addCreature (Cards.urborgPrinting cards) S.alice base
            (bloodMoonId, g2) = S.addCreature (Cards.bloodMoonPrinting cards) S.alice g1
            gs = S.withEffectAt bloodMoonId (Timestamp.MkTimestamp 500) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) g2
         in HU.assertEqual "nonbasic land is now Island" (Set.singleton Subtype.Island) (Projection.subtypesOf nonbasicId gs),
      HU.testCase "CR 612 hacking Blood Moon is order-independent (hack older)" $
        let base = Setup.emptyGame S.bothPlayers
            (nonbasicId, g1) = S.addCreature (Cards.urborgPrinting cards) S.alice base
            (bloodMoonId, g2) = S.addCreature (Cards.bloodMoonPrinting cards) S.alice g1
            -- Timestamp 1 is older than Blood Moon's own object timestamp; the
            -- outcome must not change.
            gs = S.withEffectAt bloodMoonId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) g2
         in HU.assertEqual "nonbasic land is Island, order-independent" (Set.singleton Subtype.Island) (Projection.subtypesOf nonbasicId gs),
      HU.testCase "CR 305.2 Opalescence makes Humility a creature: legal creature target and SBA-killable" $
        let base = Setup.emptyGame S.bothPlayers
            (humilityId, g1) = S.addCreature (Cards.humilityPrinting cards) S.alice base
            -- Opalescence AFTER Humility, so Opalescence's 7b (mana value 4) wins
            -- the timestamp race: Humility is a 4/4 creature.
            (_, g2) = S.addCreature (Cards.opalescencePrinting cards) S.alice g1
         in do
              HU.assertBool "Humility is a creature" (Projection.isCreatureOf humilityId g2)
              HU.assertEqual "base P/T = its mana value" (Just 4) (Projection.toughnessOf humilityId g2)
              let damaged = S.markDamage humilityId 4 g2
                  afterSba = S.settleSba damaged
              HU.assertBool "lethal damage destroys the animated enchantment" (not (Set.member humilityId (GameState.battlefield afterSba))),
      HU.testCase "CR 613 Humility + Opalescence: a real creature is 1/1 with no abilities" $
        let base = Setup.emptyGame S.bothPlayers
            (pikerId, g1) = S.addPiker cards S.alice base
            (_, g2) = S.addCreature (Cards.humilityPrinting cards) S.alice g1
            (_, gs) = S.addCreature (Cards.opalescencePrinting cards) S.alice g2
         in do
              HU.assertEqual "power 1" (Just 1) (Projection.powerOf pikerId gs)
              HU.assertEqual "toughness 1" (Just 1) (Projection.toughnessOf pikerId gs)
              HU.assertBool "no abilities" (Set.null (Projection.keywordsOf pikerId gs)),
      HU.testCase "CR 613.7 Humility + Opalescence: Humility is 4/4 when Opalescence is newer" $
        let base = Setup.emptyGame S.bothPlayers
            (humilityId, g1) = S.addCreature (Cards.humilityPrinting cards) S.alice base
            (_, gs) = S.addCreature (Cards.opalescencePrinting cards) S.alice g1
         in HU.assertEqual "Opalescence's mana-value 7b wins" (Just 4) (Projection.powerOf humilityId gs),
      HU.testCase "CR 613.7 Humility + Opalescence: Humility is 1/1 when Humility is newer" $
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addCreature (Cards.opalescencePrinting cards) S.alice base
            (humilityId, gs) = S.addCreature (Cards.humilityPrinting cards) S.alice g1
         in HU.assertEqual "Humility's 1/1 7b wins" (Just 1) (Projection.powerOf humilityId gs),
      -- Opalescence's non-Aura qualifier is unenforced; Aura subtype unmodeled (#114)
      HU.testCase "CR 305.2 Opalescence is not itself a creature (\"each other\")" $
        let base = Setup.emptyGame S.bothPlayers
            (opalId, g1) = S.addCreature (Cards.opalescencePrinting cards) S.alice base
            (_, gs) = S.addCreature (Cards.humilityPrinting cards) S.alice g1
         in HU.assertBool "Opalescence stays a non-creature enchantment" (not (Projection.isCreatureOf opalId gs)),
      HU.testCase "CR 613.7 within layer 4, timestamp order (EXPIRES at CR 613.8b, #11)" $
        -- A Piker made a Land by B (layer 4, TheseObjects), and A = AddLandSubtype
        -- Swamp over Matching (HasCardType Land) (layer 4). With A OLDER than B, timestamp order applies
        -- A before B, so A does not yet see the Piker as a land and adds no Swamp.
        -- The CR 613.8b-correct answer is that A depends on B (B changes what A
        -- applies to), so B applies first and the Piker WOULD gain Swamp. When the
        -- topological resolver lands, flip this assertion to assert the Swamp.
        let (pikerId, gs0) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
            gsA = withDynamicEffect (Affected.Matching Exclusion.IncludesSource (Filter.Type.HasCardType CardType.Land)) (Timestamp.MkTimestamp 10) (Modification.AddLandSubtype Subtype.Swamp) gs0
            gs = S.withEffectAt pikerId (Timestamp.MkTimestamp 20) (Modification.AddCardType CardType.Land) gsA
         in HU.assertBool "timestamp-only: no Swamp yet (known-incomplete, tracked)" (not (Set.member Subtype.Swamp (Projection.subtypesOf pikerId gs))),
      HU.testCase "CR 614: Rest in Peace projects its graveyard->exile replacement" $
        let (rip, gs) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual
              "one redirect replacement"
              [ReplacementEffect.ZoneChangeR (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Anyones) Zone.Exile]
              (Projection.replacementsOf rip gs),
      HU.testCase "a vanilla creature projects no replacements" $
        let (piker, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual "none" [] (Projection.replacementsOf piker gs),
      HU.testCase "CR 122.1a a +1/+1 counter adds +1/+1 (layer 7c)" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
         in do
              HU.assertEqual "power 2 + 1" (Just 3) (Projection.powerOf oid gs)
              HU.assertEqual "toughness 1 + 1" (Just 2) (Projection.toughnessOf oid gs),
      HU.testCase "CR 122.1a a -1/-1 counter subtracts 1/1" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs0
         in do
              HU.assertEqual "power 2 - 1" (Just 1) (Projection.powerOf oid gs)
              HU.assertEqual "toughness 1 - 1" (Just 0) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613.4c a +1/+1 counter and Giant Growth stack in layer 7c" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 9) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs1
         in do
              HU.assertEqual "power 2 + 1 + 3" (Just 6) (Projection.powerOf oid gs)
              HU.assertEqual "toughness 1 + 1 + 3" (Just 5) (Projection.toughnessOf oid gs),
      HU.testCase "CR 108.4 a SetController effect overrides owner; last timestamp wins" $
        let (oid, base) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            install pid g =
              let (ts, g1) = Game.freshTimestamp g
                  eff =
                    ContinuousEffect.MkContinuousEffect
                      { ContinuousEffect.source = oid,
                        ContinuousEffect.timestamp = ts,
                        ContinuousEffect.expiry = Expiry.AtCleanup,
                        ContinuousEffect.modification = Modification.SetController pid,
                        ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
                      }
               in g1 {GameState.continuousEffects = eff : GameState.continuousEffects g1}
            gs = install S.alice (install S.bob base) -- bob first (earlier), then alice (later) -> alice wins
            owned = base
         in do
              HU.assertEqual "owner controls with no effect" (Just S.bob) (Projection.controllerOf oid owned)
              HU.assertEqual "the effect grants control" (Just S.alice) (Projection.controllerOf oid gs)
              HU.assertEqual "alice controls oid" [oid] (Projection.controls S.alice gs)
              HU.assertEqual "bob controls nothing" [] (Projection.controls S.bob gs),
      HU.testCase "a copy binding seeds the fold with the copied object's copiable P/T" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs1) = S.addPiker cards S.alice gs0
            -- The Piker's copiable value (base 2/1) computed via the new function.
            snapshot = Projection.copiableCharacteristics pikerId gs1
            -- A second, unrelated creature (another Piker) we turn into a "copy":
            (cloneId, gs2) = S.addPiker cards S.alice gs1
            stamped = gs2 {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setCopy snapshot (Object.bindings o)}) cloneId (GameState.objects gs2)}
         in do
              HU.assertEqual "copy projects the snapshot power" (Just 2) (Projection.powerOf cloneId stamped)
              HU.assertEqual "copy projects the snapshot toughness" (Just 1) (Projection.toughnessOf cloneId stamped)
              HU.assertBool "copy is a creature" (Projection.isCreatureOf cloneId stamped),
      HU.testCase "an object with no copy binding projects its own base P/T" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs1) = S.addPiker cards S.alice gs0
         in do
              HU.assertEqual "base power" (Just 2) (Projection.powerOf pikerId gs1)
              HU.assertEqual "base toughness" (Just 1) (Projection.toughnessOf pikerId gs1),
      HU.testCase "legalCopyTargets is battlefield creatures excluding self" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs1) = S.addPiker cards S.alice gs0
            (cloneId, gs2) = S.addPiker cards S.alice gs1
         in HU.assertEqual "excludes self, includes the other creature" [pikerId] (Replacement.legalCopyTargets Set.empty cloneId gs2),
      HU.testCase "viewOfObject reads a projected creature's characteristics" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            view = Projection.viewOfObject oid gs
         in do
              HU.assertBool "is a creature" (Set.member CardType.Creature (Filter.cardTypes view))
              HU.assertEqual "controller" (Just S.alice) (Filter.controller view),
      HU.testCase "viewOfCard reads a printed basic land's supertypes off the battlefield" $
        let card = Printing.card (Cards.mountainPrinting cards)
            view = Projection.viewOfCard card
         in do
              HU.assertBool "is a land" (Set.member CardType.Land (Filter.cardTypes view))
              HU.assertBool "is basic" (Set.member Supertype.Basic (Filter.supertypes view))
              HU.assertEqual "no power off battlefield" Nothing (Filter.power view)
              HU.assertEqual "no controller off battlefield" Nothing (Filter.controller view),
      HU.testCase "CR 114.4 an emblem's anthem buffs the controller's creatures from the command zone" $
        let (creature, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            withEmblem = S.runPure S.identityAnswer gs0 (Resolve.applyEffect creature S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (S.anthemEmblemCard cards)))
         in HU.assertEqual "piker is 2/1 -> 3/2" (Just 3) (Projection.powerOf creature withEmblem),
      HU.testCase "CR 114.4 the anthem is scoped to the controller's creatures" $
        let (mine, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (theirs, gs1) = S.addCreature (Cards.pikerPrinting cards) S.bob gs0
            withEmblem = S.runPure S.identityAnswer gs1 (Resolve.applyEffect mine S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (S.anthemEmblemCard cards)))
         in do
              HU.assertEqual "alice's creature buffed" (Just 3) (Projection.powerOf mine withEmblem)
              HU.assertEqual "bob's creature untouched" (Just 2) (Projection.powerOf theirs withEmblem),
      HU.testCase "CR 114.5 the emblem survives a battlefield wipe and buffs a fresh token" $
        let (creature, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            withEmblem = S.runPure S.identityAnswer gs0 (Resolve.applyEffect creature S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (S.anthemEmblemCard cards)))
            wiped = withEmblem {GameState.battlefield = mempty, GameState.objects = Map.filterWithKey (\oid _ -> Set.member oid (GameState.command withEmblem)) (GameState.objects withEmblem)}
            (token, afterToken) = S.addCreature (Cards.pikerPrinting cards) S.alice wiped
         in HU.assertEqual "emblem still buffs the new creature" (Just 3) (Projection.powerOf token afterToken),
      HU.testCase "CR 613.1 projectUpTo stops before the bound layer" $
        -- A layer-7c modification is invisible to a projection bounded at
        -- ModifyPT, and visible to an unbounded one. The bound is the whole
        -- termination argument for a projected count, so it gets its own test
        -- independent of any count.
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs1) = S.addPiker cards S.alice gs0
            gs = S.withEffect pikerId (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs1
            cands = Projection.gather gs
         in do
              HU.assertEqual
                "unbounded sees the pump"
                (Just 5)
                (PC.power (Projection.projectFrom cands pikerId gs))
              HU.assertEqual
                "bounded at 7c does not"
                (Just 2)
                (PC.power (Projection.projectUpTo Layer.ModifyPT cands pikerId gs))
    ]
