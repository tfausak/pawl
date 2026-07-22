-- Covers Pawl.Card: card data, type-line rules, every printing, and the D4
-- dataflow lint.
module Pawl.CardSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Cards as Cards
import qualified Pawl.Codec as Codec
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
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified System.Directory as Directory
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
                  Card.Type.colorIndicator = Set.empty,
                  Card.Type.staticAbilities = [],
                  Card.Type.spell = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.delayedAbilities = Map.empty,
                  Card.Type.castingPermissions = [],
                  Card.Type.copyOnEnter = False,
                  Card.Type.characteristicPT = Nothing
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
    [ HU.testCase "every mode's slot reads equal its declared slots" $
        let modeOffends m =
              Set.unions (map Resolve.slotsOf (Foldable.toList (Mode.effects m)))
                /= Map.keysSet (Mode.targetSpecs m)
            cardOffends card =
              any modeOffends (Modal.modes (Card.Type.spell card))
            offenders =
              filter
                (cardOffends . Printing.card)
                (Cards.allPrintings cards)
         in HU.assertEqual "no dangling or unused slots" [] (map (Card.Type.name . Printing.card) offenders),
      HU.testCase "the data/cards directory and Cards.allPrintings agree, by slug" $ do
        -- A hand-bumped "N printings" count never caught the real hazard: a
        -- data/cards/*.json file that nobody registers in Pawl.Cards is invisible
        -- to both the M3.5 honesty round-trip and this lint suite (Pawl.Cards
        -- and the benchmark both load cards by explicit slug). Scanning the
        -- directory and comparing it against Cards.allPrintings by the SAME slug
        -- function Pawl.Cards.loadPrinting keys off of (Codec.slugify applied to
        -- the card's own name) makes a stray or missing file loud instead of
        -- silent.
        entries <- Directory.listDirectory "data/cards"
        let isJson name = Text.isSuffixOf (Text.pack ".json") (Text.pack name)
            onDisk = Set.fromList (map (Text.dropEnd 5 . Text.pack) (filter isJson entries))
            registered = Set.fromList (map (Codec.slugify . Card.Type.name . Printing.card) (Cards.allPrintings cards))
            unregistered = Set.difference onDisk registered
            missingFiles = Set.difference registered onDisk
        HU.assertEqual "data/cards files with no registered printing (each name IS the offender)" Set.empty unregistered
        HU.assertEqual "registered printings with no data/cards file (each slug IS the offender)" Set.empty missingFiles,
      HU.testCase "Blaze is a {X}{R} Sorcery dealing X to any target" $
        let card = Printing.card (Cards.blazePrinting cards)
            red = ManaSymbol.OfType (ManaType.Colored Color.Red)
         in do
              HU.assertEqual "name" (Text.pack "Blaze") (Card.Type.name card)
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Variable, red])) (Card.Type.manaCost card)
              HU.assertBool "sorcery, not instant" (not (Card.isInstant card))
              HU.assertEqual "one AnyTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget) (Card.allTargetSpecs card)
              HU.assertEqual "effect deals X" [Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) Quantity.Type.X] (Card.allEffects card),
      HU.testCase "the lint itself catches a dangling reference" $
        let bad = Set.unions [Resolve.slotsOf (Effect.DealDamage (SlotName.MkSlotName (Text.pack "ghost")) (Quantity.Type.Literal 3))]
         in HU.assertBool "misauthored card detected" (bad /= Map.keysSet (Map.empty :: Map.Map SlotName.SlotName TargetSpec.TargetSpec)),
      HU.testCase "every printing that reads X declares {X}, and vice versa" $
        let readsX c = Resolve.readsX (Card.allEffects c)
            hasVariable c = case Card.Type.manaCost c of
              Nothing -> False
              Just (ManaCost.MkManaCost syms) -> elem ManaSymbol.Variable syms
            offenders =
              filter
                (\p -> readsX (Printing.card p) /= hasVariable (Printing.card p))
                (Cards.allPrintings cards)
         in HU.assertEqual "X read iff {X} declared" [] (map (Card.Type.name . Printing.card) offenders),
      HU.testCase "the reserved X slot is never a declared target slot" $
        let offenders =
              filter
                (Map.member Binding.variableX . Card.allTargetSpecs . Printing.card)
                (Cards.allPrintings cards)
         in HU.assertEqual "no card names the X slot" [] (map (Card.Type.name . Printing.card) offenders),
      HU.testCase "the reserved modes slot is never a declared target slot" $
        let offenders =
              filter
                (Map.member Binding.chosenModes . Card.allTargetSpecs . Printing.card)
                (Cards.allPrintings cards)
         in HU.assertEqual "no card names the modes slot" [] (map (Card.Type.name . Printing.card) offenders),
      HU.testCase "the reserved trigger-source slot is never a declared target slot" $
        let offenders =
              filter
                (Map.member Binding.triggerSource . Card.allTargetSpecs . Printing.card)
                (Cards.allPrintings cards)
         in HU.assertEqual "no card names the self slot" [] (map (Card.Type.name . Printing.card) offenders),
      HU.testCase "the reserved you slot is never a declared target slot" $
        let offenders =
              filter
                (Map.member Binding.you . Card.allTargetSpecs . Printing.card)
                (Cards.allPrintings cards)
         in HU.assertEqual "no card names the you slot" [] (map (Card.Type.name . Printing.card) offenders),
      HU.testCase "Lightning Bolt is in the red pool with one AnyTarget slot" $
        let card = Printing.card (Cards.lightningBoltPrinting cards)
         in do
              HU.assertBool "an instant" (Card.isInstant card)
              HU.assertEqual "one slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget) (Card.allTargetSpecs card),
      -- The AbilityName half of the D4 dataflow lint (CR 603.7): an
      -- ArmDelayedTrigger naming an ability the card does not declare is a FAILING
      -- TEST, never a trigger that silently never fires. Equality, not subset: a
      -- declared ability nothing arms is dead card text.
      --
      -- SCOPE, same posture as Pawl.Binding's D4-lint-scope comment: this and the
      -- multi-token-binding lint below both walk `Card.allEffects`, which is
      -- `Modal.allEffects (Card.spell card)` -- a card's SPELL modes ONLY, never
      -- an activated or triggered ability's effects. An ArmDelayedTrigger placed
      -- inside an activated/triggered ability is therefore invisible to THIS
      -- lint's "every armed name is declared" half; if the card also declares no
      -- matching entry, the dangling arm passes silently. The reverse direction --
      -- a declared entry nothing arms, because the arm lives in an ability the
      -- lint can't see -- still fails loudly, which is the safe way round. No card
      -- in this pool arms from inside an ability today (only Tidal Wave arms
      -- anything, and it arms from its spell mode); widening the lint to
      -- non-spell modes is a separate, deliberately out-of-scope change.
      HU.testCase "every armed delayed ability is declared, and every declared one is armed" $
        let cardOffends card =
              Resolve.armedAbilities (Card.allEffects card) /= Map.keysSet (Card.Type.delayedAbilities card)
            offenders = filter (cardOffends . Printing.card) (Cards.allPrintings cards)
         in HU.assertEqual "no dangling or unused delayed abilities" [] (map (Card.Type.name . Printing.card) offenders),
      -- Every slot a delayed ability READS must be one the arming card DEFINES:
      -- the reserved trigger-source slot, or a token bound by a Create.
      HU.testCase "every slot a delayed ability reads is bound by its card" $
        let cardOffends card =
              let available = Set.insert Binding.triggerSource (Resolve.definedSlots (Card.allEffects card))
                  wanted = Set.unions (map Resolve.slotsOf (Card.delayedEffects card))
               in not (Set.isSubsetOf wanted available)
            offenders = filter (cardOffends . Printing.card) (Cards.allPrintings cards)
         in HU.assertEqual "no dangling delayed-ability slot" [] (map (Card.Type.name . Printing.card) offenders),
      -- CR 603.7c: binding a slot to a MULTI-token Create would silently name one
      -- of them. A named deferral (P4 spec section 8), rejected rather than guessed.
      HU.testCase "no Create binds a slot while making more than one token" $
        let offenders =
              filter
                (Resolve.bindsSeveralTokens . Card.allEffects . Printing.card)
                (Cards.allPrintings cards)
         in HU.assertEqual "no multi-token binding" [] (map (Card.Type.name . Printing.card) offenders)
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
              HU.assertBool "not a permanent target" (Map.null (Card.allTargetSpecs card))
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

m4bCardTests :: Cards.Cards -> Tasty.TestTree
m4bCardTests cards =
  Tasty.testGroup
    "M4bCards"
    [ HU.testCase "Darksteel Myr is a {3} 0/1 Artifact Creature with indestructible" $
        let c = Printing.card (Cards.darksteelMyrPrinting cards)
         in do
              HU.assertEqual "name" (Text.pack "Darksteel Myr") (Card.Type.name c)
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) (Card.Type.manaCost c)
              HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 0))) (Card.Type.power c)
              HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
              HU.assertEqual "keyword" (Set.singleton Keyword.Indestructible) (Card.Type.keywords c),
      HU.testCase "Murder is a {1}{B}{B} Instant that destroys a target creature" $
        let c = Printing.card (Cards.murderPrinting cards)
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
         in do
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, black, black])) (Card.Type.manaCost c)
              HU.assertBool "an instant" (Card.isInstant c)
              HU.assertEqual "effect destroys the target slot" [Effect.Destroy (SlotName.MkSlotName (Text.pack "target"))] (Card.allEffects c)
              HU.assertEqual "one CreatureTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.CreatureTarget) (Card.allTargetSpecs c),
      HU.testCase "Unsummon is a {U} Instant that bounces a target creature to hand" $
        let c = Printing.card (Cards.unsummonPrinting cards)
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
         in do
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [blue])) (Card.Type.manaCost c)
              HU.assertEqual "effect returns to hand" [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Hand] (Card.allEffects c),
      HU.testCase "Angelic Edict is a {4}{W} Sorcery exiling a creature or enchantment" $
        let c = Printing.card (Cards.angelicEdictPrinting cards)
         in do
              HU.assertBool "not an instant" (not (Card.isInstant c))
              HU.assertEqual "effect exiles" [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Exile] (Card.allEffects c)
              HU.assertEqual "creature-or-enchantment slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.CreatureOrEnchantmentTarget) (Card.allTargetSpecs c),
      HU.testCase "Divination is a {2}{U} Sorcery that draws two cards with no target" $
        let c = Printing.card (Cards.divinationPrinting cards)
         in do
              HU.assertEqual "effect draws two" [Effect.Draw (Quantity.Type.Literal 2)] (Card.allEffects c)
              HU.assertBool "no target slots" (Map.null (Card.allTargetSpecs c)),
      HU.testCase "Tome Scour is a {U} Sorcery milling five from a target player" $
        let c = Printing.card (Cards.tomeScourPrinting cards)
         in do
              HU.assertEqual "effect mills five" [Effect.Mill (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 5)] (Card.allEffects c)
              HU.assertEqual "one PlayerTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.PlayerTarget) (Card.allTargetSpecs c),
      HU.testCase "Mind Rot is a {2}{B} Sorcery making a target player discard two" $
        let c = Printing.card (Cards.mindRotPrinting cards)
         in do
              HU.assertEqual "effect discards two" [Effect.Discard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 2)] (Card.allEffects c)
              HU.assertEqual "one PlayerTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.PlayerTarget) (Card.allTargetSpecs c)
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Card"
    [cardTests cards, lintTests cards, m2aCardTests cards, m2bCardTests cards, m2cCardTests cards, basicLandTests cards, m3cCardTests cards, m3eCardTests cards, m4bCardTests cards]
