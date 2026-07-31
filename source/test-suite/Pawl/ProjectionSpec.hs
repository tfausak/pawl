-- Pattern matching on Pawl.Types.Prompt, a GADT, in aimAtObject below.
{-# LANGUAGE GADTs #-}

-- Covers Pawl.Projection: the layer fold -- CR 613 layer order, CR 613.7
-- within-layer timestamp order, and the CR 613.8 dependency reorder that
-- overrides it. Mostly directly-constructed continuous effects, so the engine is
-- proven independently of any card wiring; the card-level proofs live alongside.
-- Also Pawl.Subtype, the CR 205.3i land-type classification the layer-4
-- SetLandSubtype arm folds with.
module Pawl.ProjectionSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Activate as Activate
import qualified Pawl.Binding as Binding
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
-- Pawl.Types.Filter aliased Filter.Type: the evaluator Pawl.Filter already claims
-- the alias Filter above (documented phase exception). Pawl.Types.Subtype is
-- aliased Subtype.Type below for the same reason, against Pawl.Subtype.

import qualified Pawl.Registry as Registry
import qualified Pawl.Replacement as Replacement
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Subtype as Subtype
import qualified Pawl.Support as S
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layer as Layer
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype.Type
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- alice has a Forest for mana, a Piker on the battlefield, and Giant Growth in
-- hand, in her main phase. Cast Giant Growth (identityAnswer targets the only
-- creature), then resolve it.
giantGrowthOnPiker :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
giantGrowthOnPiker forest piker giantGrowth =
  let base = S.landsInPlay forest 1
      (pikerId, withPiker) = S.addCreature piker S.alice base
      (gs, ggId) = S.handOne giantGrowth withPiker
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

-- Aims every target slot at one object, deferring the rest to S.identityAnswer
-- (ModalSpec.chooseModeAt's shape). Liquimetal Coating's "target permanent" admits
-- every permanent on the board, so the choice has to be answered rather than
-- forced by construction.
aimAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject oid)) sets
  _ -> S.identityAnswer p

-- The object timestamp of the (single) Humility on the battlefield.
humilityTimestamp :: Printing.Printing -> GameState.GameState -> Timestamp.Timestamp
humilityTimestamp humility gs =
  let isHum oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard p -> Printing.card p == Printing.card humility
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
bloodMoonUrborg :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
bloodMoonUrborg forest urborg bloodMoon urborgFirst =
  let base = Setup.emptyGame S.bothPlayers
      (forestId, g1) = S.addCreature forest S.alice base
      place g =
        if urborgFirst
          then
            let (u, g') = S.addCreature urborg S.alice g
                (_, g'') = S.addCreature bloodMoon S.alice g'
             in (u, g'')
          else
            let (_, g') = S.addCreature bloodMoon S.alice g
                (u, g'') = S.addCreature urborg S.alice g'
             in (u, g'')
      (urborgId, gs) = place g1
   in (forestId, urborgId, gs)

-- Ashaya, Blood Moon, a Goblin Piker and a Goblin Piker TOKEN, all under alice,
-- plus a basic Forest for the CDA to count. `ashayaFirst` controls the timestamp
-- order (fresh timestamps ascend with placement).
--
-- The mirror image of bloodMoonUrborg. There, the LATER-applying effect (Urborg's)
-- is the one that gets stripped, so it never applies. Here it is the other way
-- round: Ashaya's layer-4 type change is what puts her (and every other nontoken
-- creature you control) INTO Blood Moon's affected set, so Blood Moon depends on
-- Ashaya under CR 613.8a and must apply second -- in either timestamp order.
ashayaBloodMoon :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
ashayaBloodMoon forest piker ashaya bloodMoon ashayaFirst =
  let base = Setup.emptyGame S.bothPlayers
      (forestId, g1) = S.addCreature forest S.alice base
      (pikerId, g2) = S.addCreature piker S.alice g1
      (tokenId, g3) = S.addToken (Printing.card piker) S.alice g2
      place g =
        if ashayaFirst
          then
            let (a, g') = S.addCreature ashaya S.alice g
                (_, g'') = S.addCreature bloodMoon S.alice g'
             in (a, g'')
          else
            let (_, g') = S.addCreature bloodMoon S.alice g
                (a, g'') = S.addCreature ashaya S.alice g'
             in (a, g'')
      (ashayaId, gs) = place g3
   in (forestId, pikerId, tokenId, ashayaId, gs)

tests :: Registry.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Projection"
    [ HU.testCase "layer classification matches CR 613.1" $ do
        HU.assertEqual "grant is layer 6" Layer.Ability (Projection.layer (Modification.GainKeyword Keyword.Deathtouch))
        HU.assertEqual "lose-all is layer 6" Layer.Ability (Projection.layer Modification.LoseAllAbilities)
        HU.assertEqual "set base is 7b" Layer.SetPT (Projection.layer (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)))
        HU.assertEqual "modify is 7c" Layer.ModifyPT (Projection.layer (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))),
      -- CR 613.1b: layer 2 is where control-changing effects apply, whether the new
      -- controller was baked at resolution (SetController) or is derived from the
      -- effect's source (SetControllerToSource).
      HU.testCase "CR 613.1b: SetControllerToSource is a layer-2 modification" $
        HU.assertEqual "layer 2" Layer.Control (Projection.layer Modification.SetControllerToSource),
      HU.testCase "no effects: the projection is the base printing (Piker is 2/1)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        HU.assertEqual "power" (Just 2) (Projection.powerOf oid gs)
        HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs)
        HU.assertBool "no keywords" (Map.null (Projection.keywordsOf oid gs)),
      HU.testCase "CR 613.3 layer 7c +3/+3 raises a Piker to 5/4" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
        HU.assertEqual "power" (Just 5) (Projection.powerOf oid gs)
        HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 layer 6 GainKeyword adds deathtouch" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.GainKeyword Keyword.Deathtouch) gs0
        HU.assertBool "has deathtouch" (Projection.hasKeyword Keyword.Deathtouch oid gs),
      -- CR 702.164b's own example: "If a creature with toxic 2 gains toxic 1 due
      -- to another effect, its total toxic value is 3." The two abilities are
      -- distinct, so they sum rather than shadow each other.
      HU.testCase "CR 702.164b total toxic value is the SUM of a creature's toxic abilities" $ do
        stalker <- Registry.printing registry "Branchblight Stalker"
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs0) = S.addCreature stalker S.bob (S.landsInPlay mountain 1)
            (plain, gs1) = S.addCreature piker S.bob gs0
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.GainKeyword (Keyword.Toxic 1)) gs1
        HU.assertEqual "printed toxic 2 alone" 2 (Projection.totalToxic oid gs1)
        HU.assertEqual "toxic 2 plus a granted toxic 1" 3 (Projection.totalToxic oid gs)
        HU.assertEqual "a creature without toxic has a total toxic value of zero" 0 (Projection.totalToxic plain gs),
      -- Rule 702.164 has no redundancy clause -- contrast CR 702.3c and 702.9c,
      -- which say in so many words that multiple instances of defender and of
      -- flying ARE redundant. So two toxic 1 abilities are two abilities, and
      -- CR 702.164b sums both. The falsifier is a projection that keeps keywords
      -- in a Set, where the second toxic 1 collapses into the first.
      --
      -- The flying half of the same test is the control: multiplicity is
      -- tracked for every keyword, and CR 702.9c redundancy is a fact about what
      -- READERS ask (hasKeyword), not about what the projection stores.
      HU.testCase "CR 702.164b two toxic abilities with the SAME N both count" $ do
        stalker <- Registry.printing registry "Branchblight Stalker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs0) = S.addCreature stalker S.bob (S.landsInPlay mountain 1)
            grant ts = S.withEffectAt oid (Timestamp.MkTimestamp ts)
            gs =
              grant 101 (Modification.GainKeyword (Keyword.Toxic 1))
                . grant 100 (Modification.GainKeyword (Keyword.Toxic 1))
                $ gs0
        HU.assertEqual "toxic 2 plus TWO granted toxic 1s" 4 (Projection.totalToxic oid gs)
        let flown =
              grant 103 (Modification.GainKeyword Keyword.Flying)
                . grant 102 (Modification.GainKeyword Keyword.Flying)
                $ gs
        HU.assertBool "CR 702.9c: two flying grants still just fly" (Projection.hasKeyword Keyword.Flying oid flown)
        HU.assertEqual "and do not disturb the total toxic value" 4 (Projection.totalToxic oid flown),
      HU.testCase "CR 613 layer 7b SetBasePowerToughness makes a Piker 1/1" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)) gs0
        HU.assertEqual "power" (Just 1) (Projection.powerOf oid gs)
        HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 sublayer order: 7b then 7c, a set-1/1 Piker with +3/+3 is 4/4" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
            -- Deliberately give 7c the EARLIER timestamp to prove layer beats
            -- timestamp: 7b still applies first.
            gs1 = S.withEffectAt oid (Timestamp.MkTimestamp 50) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)) gs1
        HU.assertEqual "power 1 then +3" (Just 4) (Projection.powerOf oid gs)
        HU.assertEqual "toughness 1 then +3" (Just 4) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613.7 within layer 6, timestamp order: later grant survives an earlier lose-all" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
            gs1 = S.withEffectAt oid (Timestamp.MkTimestamp 10) Modification.LoseAllAbilities gs0
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 20) (Modification.GainKeyword Keyword.Deathtouch) gs1
        HU.assertBool "grant wins" (Projection.hasKeyword Keyword.Deathtouch oid gs),
      HU.testCase "CR 613.7 within layer 6, timestamp order: earlier grant is erased by a later lose-all" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
            gs1 = S.withEffectAt oid (Timestamp.MkTimestamp 10) (Modification.GainKeyword Keyword.Deathtouch) gs0
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 20) Modification.LoseAllAbilities gs1
        HU.assertBool "lose-all wins" (not (Projection.hasKeyword Keyword.Deathtouch oid gs)),
      HU.testCase "a P/T modification never gives P/T to a land" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs0 = S.landsInPlay mountain 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
        HU.assertEqual "still no power" Nothing (Projection.powerOf landId gs),
      HU.testCase "CR 611 Giant Growth stores a +3/+3 effect; the Piker is 5/4" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        giantGrowth <- Registry.printing registry "Giant Growth"
        let (pikerId, gs) = giantGrowthOnPiker forest piker giantGrowth
        HU.assertEqual "one stored effect" 1 (length (GameState.continuousEffects gs))
        HU.assertEqual "power" (Just 5) (Projection.powerOf pikerId gs)
        HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf pikerId gs),
      HU.testCase "CR 601.2c Giant Growth is uncastable with no creature to target" $ do
        forest <- Registry.printing registry "Forest"
        giantGrowth <- Registry.printing registry "Giant Growth"
        let (gs, ggId) = S.handOne giantGrowth (S.landsInPlay forest 1)
        HU.assertBool "no legal target, not castable" (not (Cast.castable S.alice ggId gs)),
      HU.testCase "CR 514.2 an until-end-of-turn effect wears off at cleanup" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        giantGrowth <- Registry.printing registry "Giant Growth"
        let (pikerId, cast) = giantGrowthOnPiker forest piker giantGrowth
            -- Run the cleanup step's turn-based actions; the +3/+3 must be gone.
            afterCleanup = snd (Engine.runGamePure S.identityAnswer cast (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
        HU.assertEqual "effect dropped" [] (GameState.continuousEffects afterCleanup)
        HU.assertEqual "Piker back to base power" (Just 2) (Projection.powerOf pikerId afterCleanup)
        HU.assertEqual "Piker back to base toughness" (Just 1) (Projection.toughnessOf pikerId afterCleanup),
      HU.testCase "CR 613 Humility makes every creature 1/1 with no abilities" $ do
        birdMaiden <- Registry.printing registry "Bird Maiden"
        mountain <- Registry.printing registry "Mountain"
        humility <- Registry.printing registry "Humility"
        let (flyerId, gs0) = S.addCreature birdMaiden S.bob (S.landsInPlay mountain 1)
            gs = S.withHumility humility gs0
        HU.assertEqual "power 1" (Just 1) (Projection.powerOf flyerId gs)
        HU.assertEqual "toughness 1" (Just 1) (Projection.toughnessOf flyerId gs)
        HU.assertBool "no flying" (not (Projection.hasKeyword Keyword.Flying flyerId gs)),
      HU.testCase "CR 613 layer 6: Humility strips a creature's activated abilities" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        humility <- Registry.printing registry "Humility"
        let (sorcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            gs = S.withHumility humility g0
        HU.assertEqual "no abilities under Humility" [] (Projection.abilitiesOf sorcId gs),
      HU.testCase "without Humility the ability is present" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (sorcId, gs) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual "one ability" 1 (length (Projection.abilitiesOf sorcId gs)),
      HU.testCase "CR 704.5g Humility's toughness drop makes an already-damaged creature die" $ do
        warMammoth <- Registry.printing registry "War Mammoth"
        mountain <- Registry.printing registry "Mountain"
        humility <- Registry.printing registry "Humility"
        let (mammothId, gs0) = S.addCreature warMammoth S.bob (S.landsInPlay mountain 1)
            damaged = S.markDamage mammothId 2 gs0
            underHumility = S.withHumility humility damaged
            afterSba = S.settleSba underHumility
        HU.assertEqual "survives at 3/3 with 2 marked" (Just 3) (Projection.toughnessOf mammothId damaged)
        HU.assertEqual "no creature survives once toughness is 1" 0 (S.creaturesInPlay S.bob afterSba),
      HU.testCase "CR 613 layer order: Giant Growth on a Humility'd Piker is 4/4" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        humility <- Registry.printing registry "Humility"
        giantGrowth <- Registry.printing registry "Giant Growth"
        let base = S.landsInPlay forest 1
            (pikerId, withPiker) = S.addCreature piker S.alice base
            withHum = S.withHumility humility withPiker
            (gs, ggId) = S.handOne giantGrowth withHum
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice ggId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- Layer 7b (set 1/1) before 7c (+3/+3): 1 then +3 = 4.
        HU.assertEqual "power" (Just 4) (Projection.powerOf pikerId resolved)
        HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf pikerId resolved),
      HU.testCase "CR 611 Serpent's Gift grants deathtouch to its target" $ do
        -- {2}{G} needs 3 total mana; 3 Forests, not 2 (a brief fixture bug --
        -- 2 Forests only pay {1}{G}, leaving the spell uncast and the assertion
        -- vacuously true off the base card's native trample).
        forest <- Registry.printing registry "Forest"
        warMammoth <- Registry.printing registry "War Mammoth"
        serpentsGift <- Registry.printing registry "Serpent's Gift"
        let base = S.landsInPlay forest 3
            (mammothId, withMammoth) = S.addCreature warMammoth S.alice base
            (gs, sgId) = S.handOne serpentsGift withMammoth
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice sgId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertBool "keeps trample" (Projection.hasKeyword Keyword.Trample mammothId resolved)
        HU.assertBool "gains deathtouch" (Projection.hasKeyword Keyword.Deathtouch mammothId resolved),
      HU.testCase "CR 613.7 layer 6: a grant older than Humility is erased; newer survives" $ do
        -- War Mammoth and Humility on the battlefield; a directly-built
        -- Serpent's-Gift effect (GainKeyword Deathtouch, the same value the card
        -- creates) whose timestamp straddles Humility's object timestamp, to
        -- witness BOTH orders of CR 613.7 in layer 6. h-1 and h+1 make the
        -- relative order exact, not a guess.
        warMammoth <- Registry.printing registry "War Mammoth"
        mountain <- Registry.printing registry "Mountain"
        humility <- Registry.printing registry "Humility"
        let (mammothId, gs0) = S.addCreature warMammoth S.bob (S.landsInPlay mountain 1)
            withHum = S.withHumility humility gs0
            Timestamp.MkTimestamp h = humilityTimestamp humility withHum
            olderGrant = S.withEffectAt mammothId (Timestamp.MkTimestamp (h - 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
            newerGrant = S.withEffectAt mammothId (Timestamp.MkTimestamp (h + 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
        HU.assertBool "grant before Humility: erased" (not (Projection.hasKeyword Keyword.Deathtouch mammothId olderGrant))
        HU.assertBool "grant after Humility: survives" (Projection.hasKeyword Keyword.Deathtouch mammothId newerGrant),
      HU.testCase "projected type line: a Piker is a Creature - Goblin Warrior" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        HU.assertBool "is a creature" (Projection.isCreatureOf oid gs)
        HU.assertEqual "card types" (Set.singleton CardType.Creature) (Projection.cardTypesOf oid gs)
        HU.assertEqual "subtypes" (Set.fromList [Subtype.Type.Goblin, Subtype.Type.Warrior]) (Projection.subtypesOf oid gs),
      HU.testCase "projected type line: a Mountain is a Land - Mountain, not a creature" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
        HU.assertBool "not a creature" (not (Projection.isCreatureOf landId gs))
        HU.assertEqual "subtypes" (Set.singleton Subtype.Type.Mountain) (Projection.subtypesOf landId gs),
      HU.testCase "CR 613.1d layer 4: the three type-changing modifications are Type" $ do
        HU.assertEqual "set land subtype" Layer.Type (Projection.layer (Modification.SetLandSubtype Subtype.Type.Mountain))
        HU.assertEqual "add land subtype" Layer.Type (Projection.layer (Modification.AddLandSubtype Subtype.Type.Swamp))
        HU.assertEqual "add card type" Layer.Type (Projection.layer (Modification.AddCardType CardType.Creature)),
      HU.testCase "CR 613.1c layer 3: ChangeSubtypeWord is Text" $
        HU.assertEqual "text layer" Layer.Text (Projection.layer (Modification.ChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island)),
      HU.testCase "CR 612.1 ChangeSubtypeWord rewrites a Forest's subtype to Island" $ do
        forest <- Registry.printing registry "Forest"
        let gs0 = S.landsInPlay forest 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord Subtype.Type.Forest Subtype.Type.Island) gs0
        HU.assertEqual "only Island" (Set.singleton Subtype.Type.Island) (Projection.subtypesOf landId gs),
      HU.testCase "CR 612.2 ChangeSubtypeWord for an absent type is a no-op" $ do
        forest <- Registry.printing registry "Forest"
        let gs0 = S.landsInPlay forest 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island) gs0
        HU.assertEqual "still Forest" (Set.singleton Subtype.Type.Forest) (Projection.subtypesOf landId gs),
      HU.testCase "CR 613.1d AddLandSubtype gives a Forest the Swamp subtype" $ do
        forest <- Registry.printing registry "Forest"
        let gs0 = S.landsInPlay forest 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.AddLandSubtype Subtype.Type.Swamp) gs0
        HU.assertEqual "Forest and Swamp" (Set.fromList [Subtype.Type.Forest, Subtype.Type.Swamp]) (Projection.subtypesOf landId gs),
      HU.testCase "CR 305.7 SetLandSubtype sets a Forest to only Mountain" $ do
        forest <- Registry.printing registry "Forest"
        let gs0 = S.landsInPlay forest 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.SetLandSubtype Subtype.Type.Mountain) gs0
        HU.assertEqual "only Mountain" (Set.singleton Subtype.Type.Mountain) (Projection.subtypesOf landId gs),
      HU.testCase "CR 613.1d AddCardType makes a land a creature" $ do
        forest <- Registry.printing registry "Forest"
        let gs0 = S.landsInPlay forest 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.AddCardType CardType.Creature) gs0
        HU.assertBool "now a creature" (Projection.isCreatureOf landId gs),
      HU.testCase "CR 202.3 SetBasePowerToughness ManaValue sets a Piker to its mana value ({1}{R} = 2)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness Quantity.ManaValue Quantity.ManaValue) gs0
        HU.assertEqual "power = mana value" (Just 2) (Projection.powerOf oid gs)
        HU.assertEqual "toughness = mana value" (Just 2) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 affected-set reads the partial: a layer-4 creature-add is seen by a layer-6 Matching (HasCardType Creature) grant" $ do
        forest <- Registry.printing registry "Forest"
        let gs0 = S.landsInPlay forest 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            -- Layer 4 makes the land a creature; layer 6 grants flying to all
            -- creatures. The grant reaches the land ONLY because the affected set
            -- is evaluated after layer 4.
            gs1 = S.withEffectAt landId (Timestamp.MkTimestamp 100) (Modification.AddCardType CardType.Creature) gs0
            gs = withDynamicEffect (Affected.Matching (Filter.Type.HasCardType CardType.Creature)) (Timestamp.MkTimestamp 200) (Modification.GainKeyword Keyword.Flying) gs1
        HU.assertBool "land gained flying because it became a creature" (Projection.hasKeyword Keyword.Flying landId gs),
      HU.testCase "CR 305.7/613.8 Blood Moon strips Urborg: Urborg is only a Mountain (Blood Moon older)" $ do
        forest <- Registry.printing registry "Forest"
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (_, urborgId, gs) = bloodMoonUrborg forest urborg bloodMoon False
        HU.assertEqual "Urborg subtypes" (Set.singleton Subtype.Type.Mountain) (Projection.subtypesOf urborgId gs),
      HU.testCase "CR 305.7/613.8 Blood Moon strips Urborg: Urborg is only a Mountain (Urborg older)" $ do
        forest <- Registry.printing registry "Forest"
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (_, urborgId, gs) = bloodMoonUrborg forest urborg bloodMoon True
        HU.assertEqual "Urborg subtypes, order-independent" (Set.singleton Subtype.Type.Mountain) (Projection.subtypesOf urborgId gs),
      HU.testCase "CR 613.8 Urborg's stripped ability adds no Swamp to a Forest (Blood Moon older)" $ do
        forest <- Registry.printing registry "Forest"
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (forestId, _, gs) = bloodMoonUrborg forest urborg bloodMoon False
        HU.assertEqual "Forest stays a Forest" (Set.singleton Subtype.Type.Forest) (Projection.subtypesOf forestId gs),
      HU.testCase "CR 613.8 Urborg's stripped ability adds no Swamp to a Forest (Urborg older)" $ do
        forest <- Registry.printing registry "Forest"
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (forestId, _, gs) = bloodMoonUrborg forest urborg bloodMoon True
        HU.assertEqual "Forest stays a Forest, order-independent" (Set.singleton Subtype.Type.Forest) (Projection.subtypesOf forestId gs),
      -- Ashaya + Blood Moon. Both effects are layer 4 (CR 613.1d) and neither is
      -- a characteristic-defining ability (CR 604.3a(3): both directly affect
      -- OTHER objects), so CR 613.8a clauses (a) and (c) are satisfied and the
      -- pair is eligible to depend.
      --
      -- CR 613.8a clause (b), evaluated where the rule evaluates it -- against the
      -- state as the layer begins, with CR 613.8c re-asking after each application:
      --
      --   * Blood Moon DEPENDS on Ashaya. Applying Ashaya adds the card type Land
      --     to alice's nontoken creatures, which changes "what it applies to" for
      --     an effect whose set is "nonbasic lands".
      --   * Ashaya does NOT depend on Blood Moon. Applying Blood Moon rewrites land
      --     subtypes and (CR 305.7) strips rules text from NONBASIC LANDS; at that
      --     moment Ashaya is a Legendary Creature -- Elemental and no creature of
      --     alice's is a land, so nothing about Ashaya's text, existence, set or
      --     result changes.
      --
      -- So this is a one-way dependency, NOT the dependency LOOP of CR 613.8b's
      -- last sentence, and the answer the CR gives is Ashaya-then-Blood-Moon in
      -- BOTH timestamp orders. That is what the paired order tests below pin: were
      -- the engine to fall back to CR 613.7 timestamp order (which is what the
      -- loop clause prescribes), the Blood-Moon-older board would leave the Piker
      -- a Forest instead of a Mountain and the two would disagree.
      HU.testCase "CR 613.8a Ashaya animates a nontoken creature into a land (Ashaya older)" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (_, pikerId, _, _, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
        HU.assertBool "the Piker is a land" (Set.member CardType.Land (Projection.cardTypesOf pikerId gs))
        HU.assertBool "and still a creature (CR 205.1b: adding a type keeps the others)" (Projection.isCreatureOf pikerId gs),
      HU.testCase "CR 613.8b Blood Moon depends on Ashaya, so it applies second (Ashaya older)" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (_, pikerId, _, _, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
        HU.assertBool "the animated Piker is a Mountain" (Set.member Subtype.Type.Mountain (Projection.subtypesOf pikerId gs))
        HU.assertBool "and not the Forest Ashaya made it" (not (Set.member Subtype.Type.Forest (Projection.subtypesOf pikerId gs))),
      -- The proving test for Projection.effectUnits, and it fails without it:
      -- Ashaya's one ability has two layer-4 parts, Blood Moon depends only on the
      -- first (the card-type add), and ordered per MODIFICATION an older Blood Moon
      -- applies between them -- leaving the Piker a Mountain AND the Forest the
      -- second part then re-adds. CR 613.8 orders effects, not modifications.
      HU.testCase "CR 613.8b the dependency overrides timestamp order (Blood Moon older)" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (_, pikerId, _, _, gs) = ashayaBloodMoon forest piker ashaya bloodMoon False
        HU.assertBool "still a land" (Set.member CardType.Land (Projection.cardTypesOf pikerId gs))
        HU.assertBool "still a Mountain, order-independent" (Set.member Subtype.Type.Mountain (Projection.subtypesOf pikerId gs))
        HU.assertBool "still not a Forest, order-independent" (not (Set.member Subtype.Type.Forest (Projection.subtypesOf pikerId gs))),
      HU.testCase "CR 305.7 Ashaya's own type change reaches herself, and Blood Moon then reaches her" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (_, _, _, ashayaId, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
        HU.assertBool "Ashaya is a land" (Set.member CardType.Land (Projection.cardTypesOf ashayaId gs))
        HU.assertBool "Ashaya is a Mountain" (Set.member Subtype.Type.Mountain (Projection.subtypesOf ashayaId gs)),
      -- CR 111.3: a token is not a card, and nothing in CR 613 changes that -- so
      -- Not IsToken reads no projected aspect and no ordering turns on it.
      HU.testCase "Ashaya's 'nontoken creatures' excludes a token creature" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (_, pikerId, tokenId, _, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
        HU.assertBool "the nontoken Piker was animated" (Set.member CardType.Land (Projection.cardTypesOf pikerId gs))
        HU.assertBool "the token copy of it was not" (not (Set.member CardType.Land (Projection.cardTypesOf tokenId gs)))
        HU.assertBool "so Blood Moon never reaches the token" (not (Set.member Subtype.Type.Mountain (Projection.subtypesOf tokenId gs))),
      -- CR 604.3 / 613.4a: Ashaya's own */* is a characteristic-defining ability
      -- counting lands you control, and it counts the lands her OTHER ability
      -- just made -- layer 4 is applied before layer 7a. Without Blood Moon that
      -- is the Forest, the animated Piker and Ashaya herself; the token is
      -- excluded from the animation and so is not a land either.
      HU.testCase "CR 613.4a Ashaya's CDA counts the lands her layer-4 ability made" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addCreature forest S.alice base
            (_, g2) = S.addCreature piker S.alice g1
            (_, g3) = S.addToken (Printing.card piker) S.alice g2
            (ashayaId, gs) = S.addCreature ashaya S.alice g3
        HU.assertEqual "Forest + animated Piker + Ashaya" (Just 3) (Projection.powerOf ashayaId gs)
        HU.assertEqual "toughness the same" (Just 3) (Projection.toughnessOf ashayaId gs),
      -- CR 305.7's second sentence, in full: "It loses all abilities generated
      -- from its rules text, ITS OLD LAND TYPES, and any copiable effects
      -- affecting that land". Only the LAND types go; the fourth sentence spells
      -- out what stays -- "Setting a land's subtype doesn't add or remove any
      -- card types (such as creature) or supertypes". A creature type is neither
      -- a land type nor a card type, so it survives untouched.
      -- CR 205.3i's list, directly: the classification the arm above folds with,
      -- and the boundary that makes "keeps its creature types" mean anything.
      -- Desert is in the list too, and is why this is not the same question as
      -- Pawl.Mana.subtypeMana's CR 305.6 one: "Of that list, Forest, Island,
      -- Mountain, Plains, and Swamp are the basic land types", so a Desert is a
      -- land type that grants no intrinsic mana ability. Pawl.ManaSpec pins the
      -- other half of that pair.
      HU.testCase "CR 205.3i a land type is a land type and a creature type is not"
        . HU.assertEqual "Forest, Mountain and Desert in, Goblin and Wall out" [True, True, True, False, False]
        $ fmap Subtype.isLandType [Subtype.Type.Forest, Subtype.Type.Mountain, Subtype.Type.Desert, Subtype.Type.Goblin, Subtype.Type.Wall],
      HU.testCase "CR 305.7 a Blood Moon'd creature-land keeps its creature types" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (_, pikerId, _, ashayaId, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
        -- Goblin Piker is a Creature - Goblin Warrior; Ashaya's Forest and the
        -- Piker's printed Goblin/Warrior are the two kinds in one set, and only
        -- the first is a land type.
        HU.assertEqual "the Forest went, Goblin and Warrior stayed" (Set.fromList [Subtype.Type.Mountain, Subtype.Type.Goblin, Subtype.Type.Warrior]) (Projection.subtypesOf pikerId gs)
        HU.assertEqual "and Ashaya keeps Elemental" (Set.fromList [Subtype.Type.Mountain, Subtype.Type.Elemental]) (Projection.subtypesOf ashayaId gs),
      -- CR 305.7's FIRST clause -- "It loses all abilities generated from its
      -- rules text" -- reaches a characteristic-defining ability like any other:
      -- CR 604.3 makes a CDA a static ability, and CR 613.6's rescue does not
      -- apply because Ashaya's */* would first apply at layer 7a, after the layer
      -- 4 that takes it away.
      --
      -- Her power is then Nothing rather than the 0 CR 208.5 asks for ("If a
      -- creature somehow has no value for its power, its power is 0"), which is
      -- the read-point gap #65 already tracks -- not something this fixture is
      -- claiming is right.
      HU.testCase "CR 305.7 Blood Moon takes Ashaya's CDA with the rest of her rules text" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let (_, _, _, ashayaId, gs) = ashayaBloodMoon forest piker ashaya bloodMoon True
        HU.assertEqual "no CDA left to define power" Nothing (Projection.powerOf ashayaId gs)
        HU.assertEqual "nor toughness" Nothing (Projection.toughnessOf ashayaId gs),
      -- The remaining two ability kinds the strip has to reach, at the projection
      -- rather than through a game: Corpsejack Menace's counter-doubling
      -- replacement effect and Goblin Piker's (empty) trigger list are read off
      -- PC.replacementEffects and PC.triggeredAbilities, and CR 305.7 empties
      -- both. The gameplay proof of the replacement half is in
      -- Pawl.ReplacementSpec.
      HU.testCase "CR 305.7 Blood Moon takes an animated creature's replacement effect" $ do
        piker <- Registry.printing registry "Goblin Piker"
        corpsejackMenace <- Registry.printing registry "Corpsejack Menace"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let base = Setup.emptyGame S.bothPlayers
            (corpsejackId, g1) = S.addCreature corpsejackMenace S.alice base
            (_, g2) = S.addCreature piker S.alice g1
            (_, g3) = S.addCreature ashaya S.alice g2
        HU.assertEqual "it has one before Blood Moon" 1 (length (Projection.replacementsOf corpsejackId g3))
        let (_, gs) = S.addCreature bloodMoon S.alice g3
        HU.assertBool "the Menace is a Mountain now" (Set.member Subtype.Type.Mountain (Projection.subtypesOf corpsejackId gs))
        HU.assertEqual "and its rules text is gone" [] (Projection.replacementsOf corpsejackId gs),
      HU.testCase "CR 612 hacking Blood Moon Mountain->Island: nonbasic lands become Islands (hack newer)" $ do
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let base = Setup.emptyGame S.bothPlayers
            (nonbasicId, g1) = S.addCreature urborg S.alice base
            (bloodMoonId, g2) = S.addCreature bloodMoon S.alice g1
            gs = S.withEffectAt bloodMoonId (Timestamp.MkTimestamp 500) (Modification.ChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island) g2
        HU.assertEqual "nonbasic land is now Island" (Set.singleton Subtype.Type.Island) (Projection.subtypesOf nonbasicId gs),
      HU.testCase "CR 612 hacking Blood Moon is order-independent (hack older)" $ do
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let base = Setup.emptyGame S.bothPlayers
            (nonbasicId, g1) = S.addCreature urborg S.alice base
            (bloodMoonId, g2) = S.addCreature bloodMoon S.alice g1
            -- Timestamp 1 is older than Blood Moon's own object timestamp; the
            -- outcome must not change.
            gs = S.withEffectAt bloodMoonId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Type.Mountain Subtype.Type.Island) g2
        HU.assertEqual "nonbasic land is Island, order-independent" (Set.singleton Subtype.Type.Island) (Projection.subtypesOf nonbasicId gs),
      HU.testCase "Opalescence makes Humility a creature: legal creature target and SBA-killable" $ do
        humility <- Registry.printing registry "Humility"
        opalescence <- Registry.printing registry "Opalescence"
        let base = Setup.emptyGame S.bothPlayers
            (humilityId, g1) = S.addCreature humility S.alice base
            -- Opalescence AFTER Humility, so Opalescence's 7b (mana value 4) wins
            -- the timestamp race: Humility is a 4/4 creature.
            (_, g2) = S.addCreature opalescence S.alice g1
        HU.assertBool "Humility is a creature" (Projection.isCreatureOf humilityId g2)
        HU.assertEqual "base P/T = its mana value" (Just 4) (Projection.toughnessOf humilityId g2)
        let damaged = S.markDamage humilityId 4 g2
            afterSba = S.settleSba damaged
        HU.assertBool "lethal damage destroys the animated enchantment" (not (Set.member humilityId (GameState.battlefield afterSba))),
      -- Opalescence's card text says "each OTHER enchantment" (no rule number --
      -- CR 305.2 is the one-land-per-turn rule and is unrelated): Opalescence
      -- does not animate itself. Since #163 that is the Not IsSource conjunct in
      -- the card's own affected-set Filter, evaluated against the candidate
      -- View's identity -- so it is the data file, not an engine field, that has
      -- to carry it.
      HU.testCase "Opalescence does not animate itself" $ do
        humility <- Registry.printing registry "Humility"
        opalescence <- Registry.printing registry "Opalescence"
        let base = Setup.emptyGame S.bothPlayers
            (opalescenceId, g1) = S.addCreature opalescence S.alice base
            -- A second enchantment, so the effect is demonstrably live: it
            -- animates Humility in the same state where it skips itself.
            (humilityId, gs) = S.addCreature humility S.alice g1
        HU.assertBool "the other enchantment IS animated" (Projection.isCreatureOf humilityId gs)
        HU.assertBool "Opalescence is not" (not (Projection.isCreatureOf opalescenceId gs)),
      -- CR 613.6: "If an effect starts to apply in one layer and/or sublayer, it
      -- will continue to be applied to the same set of objects in each other
      -- applicable layer and/or sublayer, even if the ability generating the
      -- effect is removed during this process."
      --
      -- March of the Machines is the card that needs it, and the reason nothing
      -- before it did. Its affected set is "each NONCREATURE artifact" and its own
      -- layer-4 part makes every object in that set a creature, so a set
      -- re-derived at layer 7b would be empty: the animated artifact would have no
      -- power or toughness at all (not 0/0 -- Nothing), which CR 704.5f would not
      -- even fire on. Opalescence never noticed because its filter reads card
      -- types it does not change.
      HU.testCase "CR 613.6: March of the Machines animates an artifact AND still sets its P/T" $ do
        march <- Registry.printing registry "March of the Machines"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        let base = Setup.emptyGame S.bothPlayers
            (equip, g1) = S.addCreature bonesplitter S.alice base
            (_, gs) = S.addCreature march S.alice g1
        HU.assertBool "Bonesplitter is a creature (layer 4)" (Projection.isCreatureOf equip gs)
        HU.assertEqual "power equal to its mana value, {1} (layer 7b)" (Just 1) (Projection.powerOf equip gs)
        HU.assertEqual "and toughness the same" (Just 1) (Projection.toughnessOf equip gs),
      -- The other half of the same rule, and the half a per-layer re-derivation
      -- gets right by accident: an artifact that was ALREADY a creature is not in
      -- the set when March starts to apply, so it is in the set at NEITHER layer.
      -- Its P/T must stay printed rather than becoming its mana value.
      HU.testCase "CR 613.6: an artifact that was already a creature is in no part of March's set" $ do
        march <- Registry.printing registry "March of the Machines"
        myr <- Registry.printing registry "Darksteel Myr"
        let base = Setup.emptyGame S.bothPlayers
            (myrId, g1) = S.addCreature myr S.alice base
            (_, gs) = S.addCreature march S.alice g1
        HU.assertEqual "still its printed 0 power, not its {3} mana value" (Just 0) (Projection.powerOf myrId gs)
        HU.assertEqual "and its printed 1 toughness" (Just 1) (Projection.toughnessOf myrId gs),
      -- Living Plane is March of the Machines' green cousin: the same layer-4
      -- animation and layer-7b base P/T, but over LANDS and at a literal 1/1
      -- rather than a mana value. "That are still lands" is CR 205.1b, which
      -- AddCardType satisfies without a clause of its own -- adding a card type
      -- never removes one.
      HU.testCase "Living Plane makes a land a 1/1 creature that is still a land" $ do
        livingPlane <- Registry.printing registry "Living Plane"
        forest <- Registry.printing registry "Forest"
        let base = Setup.emptyGame S.bothPlayers
            (land, g1) = S.addCreature forest S.alice base
            (self, gs) = S.addCreature livingPlane S.alice g1
        HU.assertBool "the Forest is a creature (layer 4)" (Projection.isCreatureOf land gs)
        HU.assertBool "and still a land" (Set.member CardType.Land (Projection.cardTypesOf land gs))
        HU.assertEqual "power 1 (layer 7b)" (Just 1) (Projection.powerOf land gs)
        HU.assertEqual "toughness 1" (Just 1) (Projection.toughnessOf land gs)
        HU.assertBool "the enchantment itself is no land, so it animates nothing but lands" (not (Projection.isCreatureOf self gs)),
      -- The whole card, cast: March of the Machines' own reminder text is
      -- "(Equipment that's a creature can't equip a creature.)" -- CR 301.5c, whose
      -- state-based action is CR 704.5p. So the two halves meet here: the layer-7b
      -- part that CR 613.6 rescues gives the Equipment its P/T, and the layer-4
      -- part that gave it the creature type also knocks it off the creature it was
      -- equipping.
      HU.testCase "CR 613.6 + CR 704.5p whole card: casting March animates an equipped Bonesplitter, which falls off" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        march <- Registry.printing registry "March of the Machines"
        let base = S.landsInPlay island 4 -- {3}{U}
            (creature, g1) = S.addCreature piker S.alice base
            (equip, g2) = S.addCreature bonesplitter S.alice g1
            attached = S.attach equip creature g2
            (withSpell, spellId) = S.handOne march attached
            cast = snd (Engine.runGamePure S.identityAnswer withSpell (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            after = snd (Engine.runGamePure S.identityAnswer resolved Engine.settleForPriority)
        HU.assertEqual "equipped, the Piker was 4/1" (Just 4) (Projection.powerOf creature attached)
        HU.assertEqual "the Equipment is a 1/1 creature" (Just 1) (Projection.powerOf equip after)
        HU.assertBool "it is still on the battlefield" (Set.member equip (GameState.battlefield after))
        HU.assertEqual "but unattached" (Just Nothing) (fmap Object.attachedTo (Game.lookupObject equip after))
        HU.assertEqual "so the Piker is back to 2 power" (Just 2) (Projection.powerOf creature after),
      HU.testCase "CR 613 Humility + Opalescence: a real creature is 1/1 with no abilities" $ do
        piker <- Registry.printing registry "Goblin Piker"
        humility <- Registry.printing registry "Humility"
        opalescence <- Registry.printing registry "Opalescence"
        let base = Setup.emptyGame S.bothPlayers
            (pikerId, g1) = S.addCreature piker S.alice base
            (_, g2) = S.addCreature humility S.alice g1
            (_, gs) = S.addCreature opalescence S.alice g2
        HU.assertEqual "power 1" (Just 1) (Projection.powerOf pikerId gs)
        HU.assertEqual "toughness 1" (Just 1) (Projection.toughnessOf pikerId gs)
        HU.assertBool "no abilities" (Map.null (Projection.keywordsOf pikerId gs)),
      HU.testCase "CR 613.7 Humility + Opalescence: Humility is 4/4 when Opalescence is newer" $ do
        humility <- Registry.printing registry "Humility"
        opalescence <- Registry.printing registry "Opalescence"
        let base = Setup.emptyGame S.bothPlayers
            (humilityId, g1) = S.addCreature humility S.alice base
            (_, gs) = S.addCreature opalescence S.alice g1
        HU.assertEqual "Opalescence's mana-value 7b wins" (Just 4) (Projection.powerOf humilityId gs),
      HU.testCase "CR 613.7 Humility + Opalescence: Humility is 1/1 when Humility is newer" $ do
        humility <- Registry.printing registry "Humility"
        opalescence <- Registry.printing registry "Opalescence"
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addCreature opalescence S.alice base
            (humilityId, gs) = S.addCreature humility S.alice g1
        HU.assertEqual "Humility's 1/1 7b wins" (Just 1) (Projection.powerOf humilityId gs),
      -- Opalescence's own text says "each other NON-AURA enchantment". Card text, not
      -- a rule -- CR 305.2 is the one-land-per-turn rule and does not bear on this.
      HU.testCase "Opalescence does not animate an Aura" $ do
        opalescence <- Registry.printing registry "Opalescence"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        restInPeace <- Registry.printing registry "Rest in Peace"
        let base = Setup.emptyGame S.bothPlayers
            (_, withOpal) = S.addCreature opalescence S.alice base
            (auraId, withAura) = S.addCreature unholyStrength S.alice withOpal
            (ripId, gs) = S.addCreature restInPeace S.alice withAura
        HU.assertBool "the Aura stays a non-creature" (not (Projection.isCreatureOf auraId gs))
        HU.assertBool "a non-Aura enchantment IS animated" (Projection.isCreatureOf ripId gs),
      -- CR 613.1b puts control-changing effects in layer 2 and CR 613.1f puts
      -- ability-removing effects in layer 6, so layer 2 is applied FIRST: by the
      -- time Humility's LoseAllAbilities is applied, Control Magic's grant has
      -- already moved the creature. Nothing can reverse that order -- CR 613.8a
      -- scopes dependency to effects "applied in the same layer (and, if
      -- applicable, sublayer)", so a layer-6 effect is never pulled ahead of a
      -- layer-2 one -- and CR 613.6 keeps an effect applying "even if the
      -- ability generating the effect is removed during this process". An
      -- ability-stripped control grant therefore still holds what it took.
      --
      -- Humility cannot in fact reach Control Magic at all: its affected set is
      -- "each creature" and the Aura is an enchantment, which the test above is
      -- the other half of (the pool's one enchantment animator excludes Auras).
      -- So the assertion that Humility is LIVE -- the stolen Piker is 1/1 -- is
      -- what stops this passing for the trivial reason.
      HU.testCase "CR 613.1b before CR 613.1f: Humility does not hand back a Control Magic'd creature" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        humility <- Registry.printing registry "Humility"
        let base = Setup.emptyGame S.bothPlayers
            (creature, g1) = S.addCreature piker S.bob base
            (aura, g2) = S.addCreature controlMagic S.alice g1
            stolen = S.attach aura creature g2
            gs = S.withHumility humility stolen
        HU.assertEqual "before Humility, alice controls it" (Just S.alice) (Projection.controllerOf creature stolen)
        HU.assertEqual "and after Humility she still does" (Just S.alice) (Projection.controllerOf creature gs)
        HU.assertBool "it is in alice's controls" (elem creature (Projection.controls S.alice gs))
        HU.assertBool "and not in bob's" (notElem creature (Projection.controls S.bob gs))
        HU.assertEqual "Humility is live: the stolen Piker is 1/1" (Just 1) (Projection.powerOf creature gs)
        HU.assertBool "the Aura is no creature, so Humility's set never held it" (not (Projection.isCreatureOf aura gs)),
      -- The same board built the other way round: Humility is already out when
      -- the Aura arrives. CR 613.1 orders effects in different layers by LAYER,
      -- and CR 613.7's timestamps only order effects within one -- so which
      -- permanent entered first cannot change the answer.
      HU.testCase "CR 613.1: Humility first, then Control Magic, reaches the same controller" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        humility <- Registry.printing registry "Humility"
        let base = Setup.emptyGame S.bothPlayers
            (creature, g1) = S.addCreature piker S.bob base
            underHumility = S.withHumility humility g1
            (aura, withAura) = S.addCreature controlMagic S.alice underHumility
            gs = S.attach aura creature withAura
        HU.assertEqual "bob's until the Aura attaches" (Just S.bob) (Projection.controllerOf creature underHumility)
        HU.assertEqual "alice's once it does" (Just S.alice) (Projection.controllerOf creature gs)
        HU.assertEqual "Humility is live: the stolen Piker is 1/1" (Just 1) (Projection.powerOf creature gs),
      -- The leg that shows Humility is not moving controllers on its own.
      HU.testCase "CR 613.1f: Humility alone changes no controller" $ do
        piker <- Registry.printing registry "Goblin Piker"
        humility <- Registry.printing registry "Humility"
        let base = Setup.emptyGame S.bothPlayers
            (creature, g1) = S.addCreature piker S.bob base
            gs = S.withHumility humility g1
        HU.assertEqual "still bob's" (Just S.bob) (Projection.controllerOf creature gs)
        HU.assertEqual "though Humility is live: 1/1" (Just 1) (Projection.powerOf creature gs),
      -- The layer-2-before-layer-6 claim pinned where no card can pin it.
      -- Nothing in the pool strips an AURA's abilities -- Humility's set is
      -- "each creature", and the pool's one enchantment animator excludes Auras
      -- by its own text -- so this drops a bare layer-6 LoseAllAbilities on the
      -- Aura itself, the stored shape the layer-6 tests above already use.
      --
      -- Honest about what it observes: ProjectedCharacteristics carries no
      -- static-ability field, so the strip is a no-op on this Aura today and
      -- this test cannot watch one land. What it pins is the DIRECTION -- gating
      -- the control-grant walk on layer-6 ability removal breaks it, and per CR
      -- 613.1b/613.1f that gate would be wrong.
      HU.testCase "CR 613.1b: a layer-6 strip on the Aura does not undo its layer-2 grant" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = Setup.emptyGame S.bothPlayers
            (creature, g1) = S.addCreature piker S.bob base
            (aura, g2) = S.addCreature controlMagic S.alice g1
            stolen = S.attach aura creature g2
            gs = S.withEffectAt aura (Timestamp.MkTimestamp 500) Modification.LoseAllAbilities stolen
        HU.assertEqual "the Aura still holds the creature" (Just S.alice) (Projection.controllerOf creature gs),
      -- The boundary of the claim above, and the direction where it flips.
      -- Layer 6 is applied BEFORE layer 7 (CR 613.1f, CR 613.1g), so an ability
      -- removed in layer 6 generates no layer-7 effect -- and CR 613.6 does not
      -- rescue it, because layer 7c is the only layer that part would ever have
      -- started to apply in.
      --
      -- Opalescence animates Bad Moon (layer 4), Humility strips every creature
      -- (layer 6), so Bad Moon's "black creatures get +1/+1" (layer 7c) must not
      -- apply and bob's black Skeletons is Humility's 1/1.
      --
      -- The same board without Humility is asserted alongside, so this cannot pass
      -- for the trivial reason that the pump never reaches the Skeletons at all.
      HU.testCase "CR 613.1f/613.1g layer 6 before layer 7c: a stripped Bad Moon pumps nothing" $ do
        skeletons <- Registry.printing registry "Drudge Skeletons"
        badMoon <- Registry.printing registry "Bad Moon"
        humility <- Registry.printing registry "Humility"
        opalescence <- Registry.printing registry "Opalescence"
        let base = Setup.emptyGame S.bothPlayers
            (skelId, g1) = S.addCreature skeletons S.bob base
            (badMoonId, g2) = S.addCreature badMoon S.alice g1
            (_, unstripped) = S.addCreature opalescence S.alice g2
            (_, g3) = S.addCreature humility S.alice g2
            (_, gs) = S.addCreature opalescence S.alice g3
        HU.assertEqual "animated but unstripped, Bad Moon pumps: 2/2" (Just 2) (Projection.powerOf skelId unstripped)
        HU.assertBool "Bad Moon is a creature, so Humility's set holds it" (Projection.isCreatureOf badMoonId gs)
        HU.assertEqual "Humility's 1/1, not 2/2" (Just 1) (Projection.powerOf skelId gs)
        HU.assertEqual "and the same on toughness" (Just 1) (Projection.toughnessOf skelId gs),
      -- The narrowness of that gate, in the direction it must NOT reach: the
      -- question is whether BAD MOON's abilities were removed, not whether a
      -- LoseAllAbilities is on the battlefield. Humility's set is "each creature"
      -- and an un-animated Bad Moon is a plain enchantment, so it keeps its
      -- ability -- and CR 613.4's sublayers do the rest, 7b setting the base to
      -- 1/1 before 7c adds the +1/+1. The real ruling is that Humility plus Bad
      -- Moon makes a black creature 2/2.
      HU.testCase "CR 613.4 an un-animated Bad Moon is not stripped, so a Humility'd black creature is 2/2" $ do
        skeletons <- Registry.printing registry "Drudge Skeletons"
        badMoon <- Registry.printing registry "Bad Moon"
        humility <- Registry.printing registry "Humility"
        let base = Setup.emptyGame S.bothPlayers
            (skelId, g1) = S.addCreature skeletons S.bob base
            (badMoonId, g2) = S.addCreature badMoon S.alice g1
            gs = S.withHumility humility g2
        HU.assertBool "no animator, so Bad Moon is no creature" (not (Projection.isCreatureOf badMoonId gs))
        HU.assertEqual "1/1 from Humility's 7b, then +1/+1 from Bad Moon's 7c" (Just 2) (Projection.powerOf skelId gs)
        HU.assertEqual "and the same on toughness" (Just 2) (Projection.toughnessOf skelId gs),
      -- The third boundary, and the one CR 613.6 draws rather than CR 613.1f/g:
      -- an ability with a part BELOW layer 6 has already started to apply when
      -- layer 6 removes it, so "it will continue to be applied to the same set of
      -- objects in each other applicable layer" and its layer-7 part stands.
      --
      -- Opalescence animates March of the Machines, so Humility's "each creature"
      -- reaches March and strips it -- but March's layer-4 AddCardType already
      -- made Mindslaver a creature, so March's layer-7b "with power and toughness
      -- each equal to its mana value" still applies. March enters last, so its 7b
      -- has the later timestamp (CR 613.7) and beats Humility's 1/1: Mindslaver is
      -- 6/6, not 1/1. The contrast with Bad Moon two tests up is the whole rule --
      -- Bad Moon's ONLY part is layer 7c, so it never started to apply at all.
      HU.testCase "CR 613.6: a stripped March of the Machines still sets P/T, because its layer-4 part started" $ do
        mindslaver <- Registry.printing registry "Mindslaver"
        humility <- Registry.printing registry "Humility"
        opalescence <- Registry.printing registry "Opalescence"
        march <- Registry.printing registry "March of the Machines"
        let base = Setup.emptyGame S.bothPlayers
            (slaverId, g1) = S.addCreature mindslaver S.alice base
            (_, g2) = S.addCreature humility S.alice g1
            (_, g3) = S.addCreature opalescence S.alice g2
            (marchId, gs) = S.addCreature march S.alice g3
        HU.assertBool "Opalescence animated March, so Humility's set holds it" (Projection.isCreatureOf marchId gs)
        HU.assertBool "March's layer-4 part still animates the artifact" (Projection.isCreatureOf slaverId gs)
        HU.assertEqual "and its layer-7b part still sets the mana value, 6" (Just 6) (Projection.powerOf slaverId gs)
        HU.assertEqual "and the same on toughness" (Just 6) (Projection.toughnessOf slaverId gs),
      -- The last thing the gate must not reach. CR 122.1a makes a +1/+1 counter's
      -- +1/+1 a rule of the GAME rather than an ability of the permanent it sits
      -- on, so there is nothing on the creature for CR 613.1f to remove and the
      -- layer-7c effect stands after Humility's layer-7b 1/1. Projection.gather
      -- emits the counter as a synthetic candidate with the creature as its
      -- source, which is exactly the shape the layer-6 gate keys on -- so this
      -- pins that counters are excluded from it.
      HU.testCase "CR 122.1a a +1/+1 counter survives Humility: 1/1 becomes 2/2" $ do
        piker <- Registry.printing registry "Goblin Piker"
        humility <- Registry.printing registry "Humility"
        let base = Setup.emptyGame S.bothPlayers
            (pikerId, g1) = S.addCreature piker S.bob base
            g2 = S.addCounter CounterKind.PlusOnePlusOne 1 pikerId g1
            gs = S.withHumility humility g2
        HU.assertEqual "power 1 + 1" (Just 2) (Projection.powerOf pikerId gs)
        HU.assertEqual "toughness 1 + 1" (Just 2) (Projection.toughnessOf pikerId gs),
      -- CR 613.8b: "An effect dependent on one or more other effects waits to
      -- apply until just after all of those effects have been applied." A Piker
      -- made a Land by B (layer 4, TheseObjects), and A = AddLandSubtype Swamp
      -- over Matching (HasCardType Land), also layer 4, with A OLDER than B.
      --
      -- A depends on B by CR 613.8a clause (b): applying B changes what A applies
      -- to. So B goes first despite its later timestamp, and the Piker gains the
      -- Swamp. Timestamp order alone would apply A to a Piker that is not a land
      -- yet and add nothing -- which is what this test asserted, and documented as
      -- known-incomplete, until #11 closed.
      HU.testCase "CR 613.8b within layer 4, a dependency overrides timestamp order" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (pikerId, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
            gsA = withDynamicEffect (Affected.Matching (Filter.Type.HasCardType CardType.Land)) (Timestamp.MkTimestamp 10) (Modification.AddLandSubtype Subtype.Type.Swamp) gs0
            gs = S.withEffectAt pikerId (Timestamp.MkTimestamp 20) (Modification.AddCardType CardType.Land) gsA
        HU.assertBool "the newer land-maker applied first, so the Swamp lands" (Set.member Subtype.Type.Swamp (Projection.subtypesOf pikerId gs)),
      -- The other direction, which is CR 613.7 surviving underneath CR 613.8: with
      -- no dependency between them, two same-layer effects are still applied in
      -- timestamp order. Here B makes the Piker a land at timestamp 20 and A adds
      -- a Swamp to a FIXED set (the Piker) at timestamp 10 -- A's set names an
      -- object id, so applying B cannot change it, so A does not depend on B and
      -- nothing is reordered.
      --
      -- Each SetLandSubtype retires the LAND type its predecessor left and no
      -- more (CR 305.7), so the Piker's printed creature types ride through both
      -- and the Swamp/Forest pair alone carries the ordering claim.
      HU.testCase "CR 613.7 within layer 4, no dependency leaves timestamp order alone" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        let (pikerId, gs0) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
            gsA = S.withEffectAt pikerId (Timestamp.MkTimestamp 10) (Modification.SetLandSubtype Subtype.Type.Swamp) gs0
            gs = S.withEffectAt pikerId (Timestamp.MkTimestamp 20) (Modification.SetLandSubtype Subtype.Type.Forest) gsA
        HU.assertEqual "the later SetLandSubtype wins, and only the land types moved" (Set.fromList [Subtype.Type.Forest, Subtype.Type.Goblin, Subtype.Type.Warrior]) (Projection.subtypesOf pikerId gs),
      -- CR 613.8b's last sentence: "If several dependent effects form a dependency
      -- loop, then this rule is ignored and the effects IN THE DEPENDENCY LOOP are
      -- applied in timestamp order." Only the loop's own members escape the
      -- dependency rule; an effect that merely waits on the loop keeps waiting.
      --
      -- Three effects on a Forest, all layer 4:
      --
      --   A (t=20) "each noncreature ... gains Swamp"     applies; reads types
      --   B (t=30) "each non-Swamp ... becomes a creature" applies; reads subtypes
      --   C (t=10) "each Swamp ... gains Mountain"         does NOT apply yet
      --
      -- A and B each stop the other from applying, so they are a two-effect loop.
      -- C depends on A -- A's Swamp is what would let C apply -- but nothing
      -- depends on C, so C is NOT in the loop. C also has the earliest timestamp,
      -- which is the whole point: a fallback that took the earliest of everything
      -- pending would spend C first, while it still does not apply, and the
      -- Mountain would never land. Restricted to the cycle, A goes first, and C
      -- gets its turn afterwards with the Swamp in place.
      HU.testCase "CR 613.8b a dependency loop lets only its own members ignore the rule" $ do
        forest <- Registry.printing registry "Forest"
        let gs0 = S.landsInPlay forest 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            withC = withDynamicEffect (Affected.Matching (Filter.Type.HasSubtype Subtype.Type.Swamp)) (Timestamp.MkTimestamp 10) (Modification.AddLandSubtype Subtype.Type.Mountain) gs0
            withA = withDynamicEffect (Affected.Matching (Filter.Type.Not (Filter.Type.HasCardType CardType.Creature))) (Timestamp.MkTimestamp 20) (Modification.AddLandSubtype Subtype.Type.Swamp) withC
            gs = withDynamicEffect (Affected.Matching (Filter.Type.Not (Filter.Type.HasSubtype Subtype.Type.Swamp))) (Timestamp.MkTimestamp 30) (Modification.AddCardType CardType.Creature) withA
            subtypes = Projection.subtypesOf landId gs
        HU.assertBool "A applied: the Forest is a Swamp" (Set.member Subtype.Type.Swamp subtypes)
        HU.assertBool "and C, which was only waiting on the loop, still got its turn" (Set.member Subtype.Type.Mountain subtypes)
        HU.assertBool "B lost its window once A applied, so this is no creature" (not (Projection.isCreatureOf landId gs)),
      -- CR 613.8b with real cards, and the pair that retired #11's expiry trigger:
      -- Liquimetal Coating ("{T}: Target permanent becomes an artifact in addition
      -- to its other types until end of turn") and March of the Machines ("Each
      -- noncreature artifact is an artifact creature with power and toughness each
      -- equal to its mana value"). Both apply in layer 4.
      --
      -- March depends on the Coating: applying the Coating's effect makes its
      -- target an artifact, which changes whether March applies to it. The Coating
      -- does not depend on March -- its CR 611.2c set names an object id, and no
      -- type change moves an id in or out of that. So the Coating goes first even
      -- though March, already on the battlefield, is older.
      --
      -- The end state is the whole rule in one board: the Forest is an artifact
      -- (Coating), therefore a noncreature artifact when March is asked, therefore
      -- an artifact creature with base P/T equal to its mana value -- and a land
      -- has no mana cost, so that is 0/0 and CR 704.5f buries it.
      HU.testCase "CR 613.8b whole cards: Liquimetal Coating + March of the Machines kills the land it points at" $ do
        forest <- Registry.printing registry "Forest"
        march <- Registry.printing registry "March of the Machines"
        coating <- Registry.printing registry "Liquimetal Coating"
        let gs0 = S.landsInPlay forest 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            (_, g1) = S.addCreature march S.alice gs0
            (coatingId, g2) = S.addCreature coating S.alice g1
            ability = case Card.Type.activatedAbilities (Printing.card coating) of
              ab : _ -> Just ab
              [] -> Nothing
        case ability of
          Nothing -> HU.assertFailure "Liquimetal Coating should print one activated ability"
          Just coat -> do
            let ready = g2 {GameState.priority = Just S.alice}
                activated = snd (Engine.runGamePure (aimAtObject landId) ready (Activate.activateAbility S.alice coatingId coat))
                coated = snd (Engine.runGamePure (aimAtObject landId) activated Stack.resolveTop)
            HU.assertBool "the Forest is an artifact now" (Set.member CardType.Artifact (Projection.cardTypesOf landId coated))
            HU.assertBool "and March therefore animates it" (Projection.isCreatureOf landId coated)
            HU.assertEqual "at its mana value, which for a land is 0" (Just 0) (Projection.powerOf landId coated)
            let settled = snd (Engine.runGamePure (aimAtObject landId) coated Engine.settleForPriority)
            HU.assertBool "so CR 704.5f buries it" (not (Set.member landId (GameState.battlefield settled))),
      HU.testCase "CR 614: Rest in Peace projects its graveyard->exile replacement" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        let (rip, gs) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual
          "one redirect replacement"
          [ReplacementEffect.ZoneChangeR (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Anyones ZoneChangeSubject.AnyObject) Zone.Exile]
          (Projection.replacementsOf rip gs),
      HU.testCase "a vanilla creature projects no replacements" $ do
        pikerPrinting <- Registry.printing registry "Goblin Piker"
        let (piker, gs) = S.addCreature pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual "none" [] (Projection.replacementsOf piker gs),
      HU.testCase "CR 122.1a a +1/+1 counter adds +1/+1 (layer 7c)" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        let base = S.landsInPlay forest 0
            (oid, gs0) = S.addCreature piker S.bob base
            gs = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
        HU.assertEqual "power 2 + 1" (Just 3) (Projection.powerOf oid gs)
        HU.assertEqual "toughness 1 + 1" (Just 2) (Projection.toughnessOf oid gs),
      HU.testCase "CR 122.1a a -1/-1 counter subtracts 1/1" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        let base = S.landsInPlay forest 0
            (oid, gs0) = S.addCreature piker S.bob base
            gs = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs0
        HU.assertEqual "power 2 - 1" (Just 1) (Projection.powerOf oid gs)
        HU.assertEqual "toughness 1 - 1" (Just 0) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613.4c a +1/+1 counter and Giant Growth stack in layer 7c" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        let base = S.landsInPlay forest 0
            (oid, gs0) = S.addCreature piker S.bob base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
            gs = S.withEffectAt oid (Timestamp.MkTimestamp 9) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs1
        HU.assertEqual "power 2 + 1 + 3" (Just 6) (Projection.powerOf oid gs)
        HU.assertEqual "toughness 1 + 1 + 3" (Just 5) (Projection.toughnessOf oid gs),
      HU.testCase "CR 108.4 a SetController effect overrides owner; last timestamp wins" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
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
        HU.assertEqual "owner controls with no effect" (Just S.bob) (Projection.controllerOf oid owned)
        HU.assertEqual "the effect grants control" (Just S.alice) (Projection.controllerOf oid gs)
        HU.assertEqual "alice controls oid" [oid] (Projection.controls S.alice gs)
        HU.assertEqual "bob controls nothing" [] (Projection.controls S.bob gs),
      HU.testCase "a copy binding seeds the fold with the copied object's copiable P/T" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs1) = S.addCreature piker S.alice gs0
            -- The Piker's copiable value (base 2/1) computed via the new function.
            snapshot = Projection.copiableCharacteristics pikerId gs1
            -- A second, unrelated creature (another Piker) we turn into a "copy":
            (cloneId, gs2) = S.addCreature piker S.alice gs1
            stamped = gs2 {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setCopy snapshot (Object.bindings o)}) cloneId (GameState.objects gs2)}
        HU.assertEqual "copy projects the snapshot power" (Just 2) (Projection.powerOf cloneId stamped)
        HU.assertEqual "copy projects the snapshot toughness" (Just 1) (Projection.toughnessOf cloneId stamped)
        HU.assertBool "copy is a creature" (Projection.isCreatureOf cloneId stamped),
      HU.testCase "an object with no copy binding projects its own base P/T" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs1) = S.addCreature piker S.alice gs0
        HU.assertEqual "base power" (Just 2) (Projection.powerOf pikerId gs1)
        HU.assertEqual "base toughness" (Just 1) (Projection.toughnessOf pikerId gs1),
      HU.testCase "legalCopyTargets is battlefield creatures excluding self" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs1) = S.addCreature piker S.alice gs0
            (cloneId, gs2) = S.addCreature piker S.alice gs1
        HU.assertEqual "excludes self, includes the other creature" [pikerId] (Replacement.legalCopyTargets Set.empty cloneId gs2),
      HU.testCase "viewOfObject reads a projected creature's characteristics" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            view = Projection.viewOfObject oid gs
        HU.assertBool "is a creature" (Set.member CardType.Creature (Filter.cardTypes view))
        HU.assertEqual "controller" (Just S.alice) (Filter.controller view),
      HU.testCase "viewOfCard reads a printed basic land's supertypes off the battlefield" $ do
        mountain <- Registry.printing registry "Mountain"
        let card = Printing.card mountain
            view = Projection.viewOfCard card
        HU.assertBool "is a land" (Set.member CardType.Land (Filter.cardTypes view))
        HU.assertBool "is basic" (Set.member Supertype.Basic (Filter.supertypes view))
        HU.assertEqual "no power off battlefield" Nothing (Filter.power view)
        HU.assertEqual "no controller off battlefield" Nothing (Filter.controller view),
      HU.testCase "CR 114.4 an emblem's anthem buffs the controller's creatures from the command zone" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (creature, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            withEmblem = S.runPure S.identityAnswer gs0 (Resolve.applyEffect creature S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (S.anthemEmblemCard piker)))
        HU.assertEqual "piker is 2/1 -> 3/2" (Just 3) (Projection.powerOf creature withEmblem),
      HU.testCase "CR 114.4 the anthem is scoped to the controller's creatures" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (mine, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (theirs, gs1) = S.addCreature piker S.bob gs0
            withEmblem = S.runPure S.identityAnswer gs1 (Resolve.applyEffect mine S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (S.anthemEmblemCard piker)))
        HU.assertEqual "alice's creature buffed" (Just 3) (Projection.powerOf mine withEmblem)
        HU.assertEqual "bob's creature untouched" (Just 2) (Projection.powerOf theirs withEmblem),
      HU.testCase "CR 114.5 the emblem survives a battlefield wipe and buffs a fresh token" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (creature, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            withEmblem = S.runPure S.identityAnswer gs0 (Resolve.applyEffect creature S.alice Map.empty Map.empty Map.empty (Effect.CreateEmblem (S.anthemEmblemCard piker)))
            wiped = withEmblem {GameState.battlefield = mempty, GameState.objects = Map.filterWithKey (\oid _ -> Set.member oid (GameState.command withEmblem)) (GameState.objects withEmblem)}
            (token, afterToken) = S.addCreature piker S.alice wiped
        HU.assertEqual "emblem still buffs the new creature" (Just 3) (Projection.powerOf token afterToken),
      HU.testCase "CR 613.1 projectUpTo stops before the bound layer" $ do
        -- A layer-7c modification is invisible to a projection bounded at
        -- ModifyPT, and visible to an unbounded one. The bound is the whole
        -- termination argument for a projected count, so it gets its own test
        -- independent of any count.
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs1) = S.addCreature piker S.alice gs0
            gs = S.withEffect pikerId (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs1
            cands = Projection.gather gs
        HU.assertEqual
          "unbounded sees the pump"
          (Just 5)
          (PC.power (Projection.projectFrom cands pikerId gs))
        HU.assertEqual
          "bounded at 7c does not"
          (Just 2)
          (PC.power (Projection.projectUpTo Layer.ModifyPT cands pikerId gs)),
      keywordCounterTests registry
    ]

-- CR 122.1b: "A keyword counter on a permanent ... causes that object to gain
-- that keyword", and CR 613.1f puts that grant in LAYER 6 -- not the layer 7c
-- where CR 122.1a's +1/+1 counters land.
keywordCounterTests :: Registry.Registry -> Tasty.TestTree
keywordCounterTests registry =
  Tasty.testGroup
    "KeywordCounter"
    [ HU.testCase "CR 122.1b a flying counter grants flying; without one there is none" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            flying = S.addCounter (CounterKind.Keyword Keyword.Flying) 1 pikerId board
        HU.assertBool "a bare Piker does not fly" (not (Projection.hasKeyword Keyword.Flying pikerId board))
        HU.assertBool "with the counter it does" (Projection.hasKeyword Keyword.Flying pikerId flying),
      -- The grant is layer 6, so it must NOT disturb layer 7c. A keyword counter
      -- adds no power or toughness, which is what tells it apart from the +1/+1
      -- counter the same Map holds.
      HU.testCase "CR 613.1f the grant is layer 6, so it changes no power or toughness" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            flying = S.addCounter (CounterKind.Keyword Keyword.Flying) 1 pikerId board
        HU.assertEqual "power unchanged" (Projection.powerOf pikerId board) (Projection.powerOf pikerId flying)
        HU.assertEqual "toughness unchanged" (Projection.toughnessOf pikerId board) (Projection.toughnessOf pikerId flying),
      -- CR 702.164b's counting, reached through counters: the layer-6 arm counts
      -- INSTANCES (two grants are two abilities, not one absorbed into the other),
      -- so two counters must arrive as two grants.
      HU.testCase "CR 122.1b two counters grant two instances, not one" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            one = S.addCounter (CounterKind.Keyword Keyword.Flying) 1 pikerId board
            two = S.addCounter (CounterKind.Keyword Keyword.Flying) 2 pikerId board
        HU.assertEqual "one counter, one instance" (Just 1) (Map.lookup Keyword.Flying (Projection.keywordsOf pikerId one))
        HU.assertEqual "two counters, two instances" (Just 2) (Map.lookup Keyword.Flying (Projection.keywordsOf pikerId two)),
      HU.testCase "CR 122.1b a counter of a DIFFERENT keyword grants that one, not flying" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            hasted = S.addCounter (CounterKind.Keyword Keyword.Haste) 1 pikerId board
        HU.assertBool "haste granted" (Projection.hasKeyword Keyword.Haste pikerId hasted)
        HU.assertBool "flying is not" (not (Projection.hasKeyword Keyword.Flying pikerId hasted))
    ]
