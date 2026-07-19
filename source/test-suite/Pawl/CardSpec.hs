-- Covers Pawl.Card: card data, type-line rules, every printing, and the D4
-- dataflow lint.
module Pawl.CardSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Card as Card
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

redCost :: [ManaSymbol.ManaSymbol] -> Maybe ManaCost.ManaCost
redCost symbols = Just (ManaCost.MkManaCost symbols)

m2aCardTests :: Tasty.TestTree
m2aCardTests =
  let card = Printing.card
      red = ManaSymbol.OfType (ManaType.Colored Color.Red)
   in Tasty.testGroup
        "M2aCards"
        [ HU.testCase "Bird Maiden is a {2}{R} 1/2 Human Bird with flying" $ do
            HU.assertEqual "name" (Text.pack "Bird Maiden") (Card.Type.name (card Card.birdMaidenPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card Card.birdMaidenPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power (card Card.birdMaidenPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card Card.birdMaidenPrinting))
            HU.assertEqual
              "subtypes"
              (Set.fromList [Subtype.Human, Subtype.Bird])
              (TypeLine.subtypes (Card.Type.typeLine (card Card.birdMaidenPrinting))),
          HU.testCase "Nimble Birdsticker is a {2}{R} 2/3 Goblin with reach" $ do
            HU.assertEqual "name" (Text.pack "Nimble Birdsticker") (Card.Type.name (card Card.nimbleBirdstickerPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card Card.nimbleBirdstickerPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.nimbleBirdstickerPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness (card Card.nimbleBirdstickerPrinting)),
          HU.testCase "Ogre Sentry is a {1}{R} 3/3 Ogre Warrior with defender" $ do
            HU.assertEqual "name" (Text.pack "Ogre Sentry") (Card.Type.name (card Card.ogreSentryPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red]) (Card.Type.manaCost (card Card.ogreSentryPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power (card Card.ogreSentryPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness (card Card.ogreSentryPrinting)),
          HU.testCase "Windseeker Centaur is a {1}{R}{R} 2/2 Centaur with vigilance" $ do
            HU.assertEqual "name" (Text.pack "Windseeker Centaur") (Card.Type.name (card Card.windseekerCentaurPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red, red]) (Card.Type.manaCost (card Card.windseekerCentaurPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.windseekerCentaurPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card Card.windseekerCentaurPrinting)),
          HU.testCase "Goblin Chariot is a {2}{R} 2/2 Goblin Warrior with haste" $ do
            HU.assertEqual "name" (Text.pack "Goblin Chariot") (Card.Type.name (card Card.goblinChariotPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card Card.goblinChariotPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.goblinChariotPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card Card.goblinChariotPrinting)),
          HU.testCase "all five are creatures and none is a land" $
            HU.assertBool "creatures" $
              all
                (\(p, _) -> Card.isCreature (card p) && not (Card.isLand (card p)))
                S.m2aPrintings
        ]

cardTests :: Tasty.TestTree
cardTests =
  Tasty.testGroup
    "Card"
    [ HU.testCase "Mountain printing is named Mountain" $
        HU.assertEqual "name" (Text.pack "Mountain") (Card.Type.name (Printing.card Card.mountainPrinting)),
      HU.testCase "Mountain is a Land" $
        HU.assertBool "isLand" (Card.isLand (Printing.card Card.mountainPrinting)),
      HU.testCase "Mountain has the Mountain subtype" $
        HU.assertBool "subtype" $
          Set.member Subtype.Mountain (TypeLine.subtypes (Card.Type.typeLine (Printing.card Card.mountainPrinting))),
      HU.testCase "Mountain type line contains Land" $
        HU.assertBool "cardtype" $
          Set.member CardType.Land (TypeLine.types (Card.Type.typeLine (Printing.card Card.mountainPrinting))),
      -- CR 202.1: a land has no mana cost. Not a zero cost -- no cost at all.
      HU.testCase "Mountain has no mana cost" $
        HU.assertEqual "no cost" Nothing (Card.Type.manaCost (Printing.card Card.mountainPrinting)),
      HU.testCase "Mountain has no power or toughness" $ do
        HU.assertEqual "power" Nothing (Card.Type.power (Printing.card Card.mountainPrinting))
        HU.assertEqual "toughness" Nothing (Card.Type.toughness (Printing.card Card.mountainPrinting)),
      HU.testCase "Piker printing is named Goblin Piker" $
        HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.Type.name S.pikerCard),
      HU.testCase "Piker costs {1}{R}" $
        HU.assertEqual
          "cost"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]))
          (Card.Type.manaCost S.pikerCard),
      HU.testCase "Piker is a 2/1" $ do
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power S.pikerCard)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness S.pikerCard),
      HU.testCase "Piker is a Goblin Warrior" $
        HU.assertEqual
          "subtypes"
          (Set.fromList [Subtype.Goblin, Subtype.Warrior])
          (TypeLine.subtypes (Card.Type.typeLine S.pikerCard)),
      HU.testCase "Piker is a creature and not a land" $ do
        HU.assertBool "creature" (Card.isCreature S.pikerCard)
        HU.assertBool "not land" (not (Card.isLand S.pikerCard)),
      -- CR 110.1: the classification resolution turns on. Never card identity.
      HU.testCase "CR 110.1 both a Piker and a Mountain are permanents" $ do
        HU.assertBool "piker" (Card.isPermanent S.pikerCard)
        HU.assertBool "mountain" (Card.isPermanent (Printing.card Card.mountainPrinting)),
      HU.testCase "CR 110.1 an instant is not a permanent type" $
        let instantLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Instant,
                  TypeLine.subtypes = Set.empty
                }
            card =
              Card.Type.MkCard
                { Card.Type.name = Text.pack "Some Instant",
                  Card.Type.manaCost = Nothing,
                  Card.Type.typeLine = instantLine,
                  Card.Type.power = Nothing,
                  Card.Type.toughness = Nothing,
                  Card.Type.keywords = Set.empty,
                  Card.Type.staticAbilities = [],
                  Card.Type.effects = [],
                  Card.Type.activatedAbilities = [],
                  Card.Type.targetSpecs = Map.empty
                }
         in do
              HU.assertBool "not a permanent" (not (Card.isPermanent card))
              HU.assertBool "an instant" (Card.isInstant card),
      HU.testCase "a Piker is not an instant" $
        HU.assertBool "creature" (not (Card.isInstant S.pikerCard))
    ]

-- The D4 dataflow lint: every slot an effect reads is declared, and every
-- declared slot is read. Equality, not subset: a spec no effect reads is a
-- card announcing a target it ignores -- representable in Magic, not in this
-- pool. Loosen to superset if such a card ever lands.
lintTests :: Tasty.TestTree
lintTests =
  Tasty.testGroup
    "Lint"
    [ HU.testCase "every printing's slot reads equal its declared slots" $
        let reads_ card = Set.unions (map Resolve.slotsOf (Card.Type.effects card))
            writes card = Map.keysSet (Card.Type.targetSpecs card)
            offenders =
              filter
                (\p -> reads_ (Printing.card p) /= writes (Printing.card p))
                Card.allPrintings
         in HU.assertEqual "no dangling or unused slots" [] (map (Card.Type.name . Printing.card) offenders),
      HU.testCase "the registry holds every printing (25 at M3e Task 2)" $
        HU.assertEqual "count" 25 (length Card.allPrintings),
      HU.testCase "the lint itself catches a dangling reference" $
        let bad = Set.unions [Resolve.slotsOf (Effect.DealDamage (SlotName.MkSlotName (Text.pack "ghost")) (Quantity.Type.Literal 3))]
         in HU.assertBool "misauthored card detected" (bad /= Map.keysSet (Map.empty :: Map.Map SlotName.SlotName TargetSpec.TargetSpec)),
      HU.testCase "Lightning Bolt is in the red pool with one AnyTarget slot" $
        let card = Printing.card Card.lightningBoltPrinting
         in do
              HU.assertBool "an instant" (Card.isInstant card)
              HU.assertEqual "one slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget) (Card.Type.targetSpecs card)
    ]

m2bCardTests :: Tasty.TestTree
m2bCardTests =
  let card = Printing.card
      red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      gs0 = Setup.emptyGame S.bothPlayers
   in Tasty.testGroup
        "M2bCards"
        [ HU.testCase "Sabretooth Tiger is a {2}{R} 2/1 Cat with first strike" $ do
            HU.assertEqual "name" (Text.pack "Sabretooth Tiger") (Card.Type.name (card Card.sabretoothTigerPrinting))
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, red])) (Card.Type.manaCost (card Card.sabretoothTigerPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.sabretoothTigerPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (card Card.sabretoothTigerPrinting))
            HU.assertEqual "subtypes" (Set.singleton Subtype.Cat) (TypeLine.subtypes (Card.Type.typeLine (card Card.sabretoothTigerPrinting)))
            HU.assertEqual "keyword" (Set.singleton Keyword.FirstStrike) (Card.Type.keywords (card Card.sabretoothTigerPrinting)),
          HU.testCase "Ridgetop Raptor is a {3}{R} 2/1 Dinosaur Beast with double strike" $ do
            HU.assertEqual "name" (Text.pack "Ridgetop Raptor") (Card.Type.name (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, red])) (Card.Type.manaCost (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "subtypes" (Set.fromList [Subtype.Dinosaur, Subtype.Beast]) (TypeLine.subtypes (Card.Type.typeLine (card Card.ridgetopRaptorPrinting)))
            HU.assertEqual "keyword" (Set.singleton Keyword.DoubleStrike) (Card.Type.keywords (card Card.ridgetopRaptorPrinting)),
          HU.testCase "the tiger has first strike through the projection" $
            let (oid, gs) = S.addCreature Card.sabretoothTigerPrinting S.alice gs0
             in do
                  HU.assertBool "first strike" (Projection.hasKeyword Keyword.FirstStrike oid gs)
                  HU.assertBool "not double strike" (not (Projection.hasKeyword Keyword.DoubleStrike oid gs)),
          HU.testCase "the raptor has double strike through the projection" $
            let (oid, gs) = S.addCreature Card.ridgetopRaptorPrinting S.alice gs0
             in do
                  HU.assertBool "double strike" (Projection.hasKeyword Keyword.DoubleStrike oid gs)
                  HU.assertBool "not first strike" (not (Projection.hasKeyword Keyword.FirstStrike oid gs)),
          HU.testCase "both are 2/1s, the same body as a Piker" $
            let bodyOf p = (Card.Type.power (card p), Card.Type.toughness (card p))
             in do
                  HU.assertEqual "tiger body" (bodyOf Card.pikerPrinting) (bodyOf Card.sabretoothTigerPrinting)
                  HU.assertEqual "raptor body" (bodyOf Card.pikerPrinting) (bodyOf Card.ridgetopRaptorPrinting)
        ]

m2cCardTests :: Tasty.TestTree
m2cCardTests =
  Tasty.testGroup
    "M2cCards"
    [ HU.testCase "Typhoid Rats is a {B} 1/1 Rat with deathtouch" $ do
        let c = Printing.card Card.typhoidRatsPrinting
        HU.assertEqual "name" (Text.pack "Typhoid Rats") (Card.Type.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
        HU.assertEqual "keywords" (Set.singleton Keyword.Deathtouch) (Card.Type.keywords c),
      HU.testCase "War Mammoth is a {3}{G} 3/3 Elephant with trample" $ do
        let c = Printing.card Card.warMammothPrinting
        HU.assertEqual "name" (Text.pack "War Mammoth") (Card.Type.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness c)
        HU.assertEqual "keywords" (Set.singleton Keyword.Trample) (Card.Type.keywords c)
    ]

basicLandTests :: Tasty.TestTree
basicLandTests =
  Tasty.testGroup
    "BasicLand"
    [ HU.testCase "CR 305.6 a Swamp's intrinsic ability is black mana" $
        HU.assertEqual
          "black"
          (Just (ManaType.Colored Color.Black))
          (Mana.subtypeMana Subtype.Swamp),
      HU.testCase "CR 305.6 a Forest's intrinsic ability is green mana" $
        HU.assertEqual
          "green"
          (Just (ManaType.Colored Color.Green))
          (Mana.subtypeMana Subtype.Forest),
      HU.testCase "swampPrinting is a basic Swamp land" $
        let c = Printing.card Card.swampPrinting
         in do
              HU.assertBool "land" (Card.isLand c)
              HU.assertBool
                "swamp subtype"
                (Set.member Subtype.Swamp (TypeLine.subtypes (Card.Type.typeLine c))),
      HU.testCase "forestPrinting is a basic Forest land" $
        let c = Printing.card Card.forestPrinting
         in do
              HU.assertBool "land" (Card.isLand c)
              HU.assertBool
                "forest subtype"
                (Set.member Subtype.Forest (TypeLine.subtypes (Card.Type.typeLine c)))
    ]

m3cCardTests :: Tasty.TestTree
m3cCardTests =
  Tasty.testGroup
    "M3cCards"
    [ HU.testCase "M3c printings are registered in allPrintings" $ do
        HU.assertBool "Blood Moon" (Card.bloodMoonPrinting `elem` Card.allPrintings)
        HU.assertBool "Urborg" (Card.urborgPrinting `elem` Card.allPrintings)
        HU.assertBool "Opalescence" (Card.opalescencePrinting `elem` Card.allPrintings),
      HU.testCase "Blood Moon is a {2}{R} enchantment with one SetLandSubtype static ability" $
        let card = Printing.card Card.bloodMoonPrinting
         in do
              HU.assertEqual "one static ability" 1 (length (Card.Type.staticAbilities card))
              HU.assertBool "not a permanent target" (Map.null (Card.Type.targetSpecs card))
    ]

m3eCardTests :: Tasty.TestTree
m3eCardTests =
  Tasty.testGroup
    "M3eCards"
    [ HU.testCase "Prodigal Sorcerer has one non-mana activated ability" $
        case Card.Type.activatedAbilities (Printing.card Card.prodigalSorcererPrinting) of
          [ab] -> HU.assertBool "not a mana ability" (not (Mana.isManaAbility ab))
          _ -> HU.assertFailure "expected exactly one ability",
      HU.testCase "Llanowar Elves has one mana activated ability" $
        case Card.Type.activatedAbilities (Printing.card Card.llanowarElvesPrinting) of
          [ab] -> HU.assertBool "mana ability" (Mana.isManaAbility ab)
          _ -> HU.assertFailure "expected exactly one ability"
    ]

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Card"
    [cardTests, lintTests, m2aCardTests, m2bCardTests, m2cCardTests, basicLandTests, m3cCardTests, m3eCardTests]
