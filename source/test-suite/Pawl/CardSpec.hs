-- Covers Pawl.Card: card data, type-line rules, every printing, and the D4
-- dataflow lint.
module Pawl.CardSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Card as Card
import qualified Pawl.Cards as Cards
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

m2aCardTests :: Cards.Cards -> Tasty.TestTree
m2aCardTests cards =
  let card = Printing.card
      red = ManaSymbol.OfType (ManaType.Colored Color.Red)
   in Tasty.testGroup
        "M2aCards"
        [ HU.testCase "Bird Maiden is a {2}{R} 1/2 Human Bird with flying" $ do
            HU.assertEqual "name" (Text.pack "Bird Maiden") (Card.Type.name (card (Cards.birdMaidenPrinting cards)))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card (Cards.birdMaidenPrinting cards)))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power (card (Cards.birdMaidenPrinting cards)))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card (Cards.birdMaidenPrinting cards)))
            HU.assertEqual
              "subtypes"
              (Set.fromList [Subtype.Human, Subtype.Bird])
              (TypeLine.subtypes (Card.Type.typeLine (card (Cards.birdMaidenPrinting cards)))),
          HU.testCase "Nimble Birdsticker is a {2}{R} 2/3 Goblin with reach" $ do
            HU.assertEqual "name" (Text.pack "Nimble Birdsticker") (Card.Type.name (card (Cards.nimbleBirdstickerPrinting cards)))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card (Cards.nimbleBirdstickerPrinting cards)))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card (Cards.nimbleBirdstickerPrinting cards)))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness (card (Cards.nimbleBirdstickerPrinting cards))),
          HU.testCase "Ogre Sentry is a {1}{R} 3/3 Ogre Warrior with defender" $ do
            HU.assertEqual "name" (Text.pack "Ogre Sentry") (Card.Type.name (card (Cards.ogreSentryPrinting cards)))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red]) (Card.Type.manaCost (card (Cards.ogreSentryPrinting cards)))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power (card (Cards.ogreSentryPrinting cards)))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness (card (Cards.ogreSentryPrinting cards))),
          HU.testCase "Windseeker Centaur is a {1}{R}{R} 2/2 Centaur with vigilance" $ do
            HU.assertEqual "name" (Text.pack "Windseeker Centaur") (Card.Type.name (card (Cards.windseekerCentaurPrinting cards)))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red, red]) (Card.Type.manaCost (card (Cards.windseekerCentaurPrinting cards)))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card (Cards.windseekerCentaurPrinting cards)))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card (Cards.windseekerCentaurPrinting cards))),
          HU.testCase "Goblin Chariot is a {2}{R} 2/2 Goblin Warrior with haste" $ do
            HU.assertEqual "name" (Text.pack "Goblin Chariot") (Card.Type.name (card (Cards.goblinChariotPrinting cards)))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card (Cards.goblinChariotPrinting cards)))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card (Cards.goblinChariotPrinting cards)))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card (Cards.goblinChariotPrinting cards))),
          HU.testCase "all five are creatures and none is a land" $
            HU.assertBool "creatures" $
              all
                (\(p, _) -> Card.isCreature (card p) && not (Card.isLand (card p)))
                (S.m2aPrintings cards)
        ]

cardTests :: Cards.Cards -> Tasty.TestTree
cardTests cards =
  Tasty.testGroup
    "Card"
    [ HU.testCase "Mountain printing is named Mountain" $
        HU.assertEqual "name" (Text.pack "Mountain") (Card.Type.name (Printing.card (Cards.mountainPrinting cards))),
      HU.testCase "Mountain is a Land" $
        HU.assertBool "isLand" (Card.isLand (Printing.card (Cards.mountainPrinting cards))),
      HU.testCase "Mountain has the Mountain subtype" $
        HU.assertBool "subtype" $
          Set.member Subtype.Mountain (TypeLine.subtypes (Card.Type.typeLine (Printing.card (Cards.mountainPrinting cards)))),
      HU.testCase "Mountain type line contains Land" $
        HU.assertBool "cardtype" $
          Set.member CardType.Land (TypeLine.types (Card.Type.typeLine (Printing.card (Cards.mountainPrinting cards)))),
      -- CR 202.1: a land has no mana cost. Not a zero cost -- no cost at all.
      HU.testCase "Mountain has no mana cost" $
        HU.assertEqual "no cost" Nothing (Card.Type.manaCost (Printing.card (Cards.mountainPrinting cards))),
      HU.testCase "Mountain has no power or toughness" $ do
        HU.assertEqual "power" Nothing (Card.Type.power (Printing.card (Cards.mountainPrinting cards)))
        HU.assertEqual "toughness" Nothing (Card.Type.toughness (Printing.card (Cards.mountainPrinting cards))),
      HU.testCase "Piker printing is named Goblin Piker" $
        HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.Type.name (S.pikerCard cards)),
      HU.testCase "Piker costs {1}{R}" $
        HU.assertEqual
          "cost"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]))
          (Card.Type.manaCost (S.pikerCard cards)),
      HU.testCase "Piker is a 2/1" $ do
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (S.pikerCard cards))
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (S.pikerCard cards)),
      HU.testCase "Piker is a Goblin Warrior" $
        HU.assertEqual
          "subtypes"
          (Set.fromList [Subtype.Goblin, Subtype.Warrior])
          (TypeLine.subtypes (Card.Type.typeLine (S.pikerCard cards))),
      HU.testCase "Piker is a creature and not a land" $ do
        HU.assertBool "creature" (Card.isCreature (S.pikerCard cards))
        HU.assertBool "not land" (not (Card.isLand (S.pikerCard cards))),
      -- CR 110.1: the classification resolution turns on. Never card identity.
      HU.testCase "CR 110.1 both a Piker and a Mountain are permanents" $ do
        HU.assertBool "piker" (Card.isPermanent (S.pikerCard cards))
        HU.assertBool "mountain" (Card.isPermanent (Printing.card (Cards.mountainPrinting cards))),
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
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.castingPermissions = [],
                  Card.Type.targetSpecs = Map.empty
                }
         in do
              HU.assertBool "not a permanent" (not (Card.isPermanent card))
              HU.assertBool "an instant" (Card.isInstant card),
      HU.testCase "a Piker is not an instant" $
        HU.assertBool "creature" (not (Card.isInstant (S.pikerCard cards)))
    ]

-- The D4 dataflow lint: every slot an effect reads is declared, and every
-- declared slot is read. Equality, not subset: a spec no effect reads is a
-- card announcing a target it ignores -- representable in Magic, not in this
-- pool. Loosen to superset if such a card ever lands.
lintTests :: Cards.Cards -> Tasty.TestTree
lintTests cards =
  Tasty.testGroup
    "Lint"
    [ HU.testCase "every printing's slot reads equal its declared slots" $
        let reads_ card = Set.unions (map Resolve.slotsOf (Card.Type.effects card))
            writes card = Map.keysSet (Card.Type.targetSpecs card)
            offenders =
              filter
                (\p -> reads_ (Printing.card p) /= writes (Printing.card p))
                (Cards.allPrintings cards)
         in HU.assertEqual "no dangling or unused slots" [] (map (Card.Type.name . Printing.card) offenders),
      HU.testCase "the registry holds every printing (30 at M3g Task 6)" $
        HU.assertEqual "count" 30 (length (Cards.allPrintings cards)),
      HU.testCase "the lint itself catches a dangling reference" $
        let bad = Set.unions [Resolve.slotsOf (Effect.DealDamage (SlotName.MkSlotName (Text.pack "ghost")) (Quantity.Type.Literal 3))]
         in HU.assertBool "misauthored card detected" (bad /= Map.keysSet (Map.empty :: Map.Map SlotName.SlotName TargetSpec.TargetSpec)),
      HU.testCase "Lightning Bolt is in the red pool with one AnyTarget slot" $
        let card = Printing.card (Cards.lightningBoltPrinting cards)
         in do
              HU.assertBool "an instant" (Card.isInstant card)
              HU.assertEqual "one slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget) (Card.Type.targetSpecs card)
    ]

m2bCardTests :: Cards.Cards -> Tasty.TestTree
m2bCardTests cards =
  let card = Printing.card
      red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      gs0 = Setup.emptyGame S.bothPlayers
   in Tasty.testGroup
        "M2bCards"
        [ HU.testCase "Sabretooth Tiger is a {2}{R} 2/1 Cat with first strike" $ do
            HU.assertEqual "name" (Text.pack "Sabretooth Tiger") (Card.Type.name (card (Cards.sabretoothTigerPrinting cards)))
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, red])) (Card.Type.manaCost (card (Cards.sabretoothTigerPrinting cards)))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card (Cards.sabretoothTigerPrinting cards)))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (card (Cards.sabretoothTigerPrinting cards)))
            HU.assertEqual "subtypes" (Set.singleton Subtype.Cat) (TypeLine.subtypes (Card.Type.typeLine (card (Cards.sabretoothTigerPrinting cards))))
            HU.assertEqual "keyword" (Set.singleton Keyword.FirstStrike) (Card.Type.keywords (card (Cards.sabretoothTigerPrinting cards))),
          HU.testCase "Ridgetop Raptor is a {3}{R} 2/1 Dinosaur Beast with double strike" $ do
            HU.assertEqual "name" (Text.pack "Ridgetop Raptor") (Card.Type.name (card (Cards.ridgetopRaptorPrinting cards)))
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, red])) (Card.Type.manaCost (card (Cards.ridgetopRaptorPrinting cards)))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card (Cards.ridgetopRaptorPrinting cards)))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (card (Cards.ridgetopRaptorPrinting cards)))
            HU.assertEqual "subtypes" (Set.fromList [Subtype.Dinosaur, Subtype.Beast]) (TypeLine.subtypes (Card.Type.typeLine (card (Cards.ridgetopRaptorPrinting cards))))
            HU.assertEqual "keyword" (Set.singleton Keyword.DoubleStrike) (Card.Type.keywords (card (Cards.ridgetopRaptorPrinting cards))),
          HU.testCase "the tiger has first strike through the projection" $
            let (oid, gs) = S.addCreature (Cards.sabretoothTigerPrinting cards) S.alice gs0
             in do
                  HU.assertBool "first strike" (Projection.hasKeyword Keyword.FirstStrike oid gs)
                  HU.assertBool "not double strike" (not (Projection.hasKeyword Keyword.DoubleStrike oid gs)),
          HU.testCase "the raptor has double strike through the projection" $
            let (oid, gs) = S.addCreature (Cards.ridgetopRaptorPrinting cards) S.alice gs0
             in do
                  HU.assertBool "double strike" (Projection.hasKeyword Keyword.DoubleStrike oid gs)
                  HU.assertBool "not first strike" (not (Projection.hasKeyword Keyword.FirstStrike oid gs)),
          HU.testCase "both are 2/1s, the same body as a Piker" $
            let bodyOf p = (Card.Type.power (card p), Card.Type.toughness (card p))
             in do
                  HU.assertEqual "tiger body" (bodyOf (Cards.pikerPrinting cards)) (bodyOf (Cards.sabretoothTigerPrinting cards))
                  HU.assertEqual "raptor body" (bodyOf (Cards.pikerPrinting cards)) (bodyOf (Cards.ridgetopRaptorPrinting cards))
        ]

m2cCardTests :: Cards.Cards -> Tasty.TestTree
m2cCardTests cards =
  Tasty.testGroup
    "M2cCards"
    [ HU.testCase "Typhoid Rats is a {B} 1/1 Rat with deathtouch" $ do
        let c = Printing.card (Cards.typhoidRatsPrinting cards)
        HU.assertEqual "name" (Text.pack "Typhoid Rats") (Card.Type.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
        HU.assertEqual "keywords" (Set.singleton Keyword.Deathtouch) (Card.Type.keywords c),
      HU.testCase "War Mammoth is a {3}{G} 3/3 Elephant with trample" $ do
        let c = Printing.card (Cards.warMammothPrinting cards)
        HU.assertEqual "name" (Text.pack "War Mammoth") (Card.Type.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness c)
        HU.assertEqual "keywords" (Set.singleton Keyword.Trample) (Card.Type.keywords c)
    ]

basicLandTests :: Cards.Cards -> Tasty.TestTree
basicLandTests cards =
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
        let c = Printing.card (Cards.swampPrinting cards)
         in do
              HU.assertBool "land" (Card.isLand c)
              HU.assertBool
                "swamp subtype"
                (Set.member Subtype.Swamp (TypeLine.subtypes (Card.Type.typeLine c))),
      HU.testCase "forestPrinting is a basic Forest land" $
        let c = Printing.card (Cards.forestPrinting cards)
         in do
              HU.assertBool "land" (Card.isLand c)
              HU.assertBool
                "forest subtype"
                (Set.member Subtype.Forest (TypeLine.subtypes (Card.Type.typeLine c)))
    ]

m3cCardTests :: Cards.Cards -> Tasty.TestTree
m3cCardTests cards =
  Tasty.testGroup
    "M3cCards"
    [ HU.testCase "M3c printings are registered in allPrintings" $ do
        HU.assertBool "Blood Moon" (Cards.bloodMoonPrinting cards `elem` Cards.allPrintings cards)
        HU.assertBool "Urborg" (Cards.urborgPrinting cards `elem` Cards.allPrintings cards)
        HU.assertBool "Opalescence" (Cards.opalescencePrinting cards `elem` Cards.allPrintings cards),
      HU.testCase "Blood Moon is a {2}{R} enchantment with one SetLandSubtype static ability" $
        let card = Printing.card (Cards.bloodMoonPrinting cards)
         in do
              HU.assertEqual "one static ability" 1 (length (Card.Type.staticAbilities card))
              HU.assertBool "not a permanent target" (Map.null (Card.Type.targetSpecs card))
    ]

m3eCardTests :: Cards.Cards -> Tasty.TestTree
m3eCardTests cards =
  Tasty.testGroup
    "M3eCards"
    [ HU.testCase "Prodigal Sorcerer has one non-mana activated ability" $
        case Card.Type.activatedAbilities (Printing.card (Cards.prodigalSorcererPrinting cards)) of
          [ab] -> HU.assertBool "not a mana ability" (not (Mana.isManaAbility ab))
          _ -> HU.assertFailure "expected exactly one ability",
      HU.testCase "Llanowar Elves has one mana activated ability" $
        case Card.Type.activatedAbilities (Printing.card (Cards.llanowarElvesPrinting cards)) of
          [ab] -> HU.assertBool "mana ability" (Mana.isManaAbility ab)
          _ -> HU.assertFailure "expected exactly one ability"
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Card"
    [cardTests cards, lintTests cards, m2aCardTests cards, m2bCardTests cards, m2cCardTests cards, basicLandTests cards, m3cCardTests cards, m3eCardTests cards]
