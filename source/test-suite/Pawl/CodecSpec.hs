-- Covers Pawl.Codec.
module Pawl.CodecSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Cards as Cards
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as J
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.AdditionalCost as AdditionalCost
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Decimal as Decimal
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.Json as Json
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

roundTrip :: (Eq a, Show a) => String -> (a -> Json.Value) -> (Json.Value -> Either Text a) -> a -> HU.Assertion
roundTrip label enc dec x = HU.assertEqual label (Right x) (dec (enc x))

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Pawl.CodecSpec"
    [ Tasty.testGroup
        "leaf enums"
        [ HU.testCase "Color" $
            mapM_ (roundTrip "color" Codec.colorToJson Codec.jsonToColor) [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green],
          HU.testCase "Keyword" $
            roundTrip "kw" Codec.keywordToJson Codec.jsonToKeyword Keyword.Trample,
          HU.testCase "Zone" $
            roundTrip "zone" Codec.zoneToJson Codec.jsonToZone Zone.Graveyard,
          HU.testCase "unknown tag fails" $
            HU.assertBool "left" (either (const True) (const False) (Codec.jsonToColor (Json.Object [])))
        ],
      Tasty.testGroup
        "newtypes"
        [ HU.testCase "SlotName" $
            roundTrip "slot" Codec.slotNameToJson Codec.jsonToSlotName (SlotName.MkSlotName (Text.pack "x")),
          HU.testCase "ObjectId" $
            roundTrip "oid" Codec.objectIdToJson Codec.jsonToObjectId (ObjectId.MkObjectId 7)
        ],
      Tasty.testGroup
        "mana + quantity (tagged-sum trap)"
        [ HU.testCase "Quantity.Literal is a tagged object with numeric value" $
            HU.assertEqual
              "shape"
              (Json.Object [(Text.pack "type", Json.String (Text.pack "Literal")), (Text.pack "value", Json.Number (Decimal.mkDecimal 3 0))])
              (Codec.quantityToJson (Quantity.Literal 3)),
          HU.testCase "Quantity.ManaValue is nullary tagged" $
            roundTrip "mv" Codec.quantityToJson Codec.jsonToQuantity Quantity.ManaValue,
          HU.testCase "Quantity.Literal round-trips" $
            roundTrip "lit" Codec.quantityToJson Codec.jsonToQuantity (Quantity.Literal 5),
          HU.testCase "ManaCost round-trips" $
            roundTrip
              "cost"
              Codec.manaCostToJson
              Codec.jsonToManaCost
              (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]),
          HU.testCase "Power round-trips" $
            roundTrip "pow" Codec.powerToJson Codec.jsonToPower (Power.MkPower (Quantity.Literal 2))
        ],
      Tasty.testGroup
        "modification + affected"
        [ HU.testCase "GainKeyword" $
            roundTrip "m1" Codec.modificationToJson Codec.jsonToModification (Modification.GainKeyword Keyword.Deathtouch),
          HU.testCase "SetBasePowerToughness" $
            roundTrip "m2" Codec.modificationToJson Codec.jsonToModification (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)),
          HU.testCase "ChangeSubtypeWord" $
            roundTrip "m3" Codec.modificationToJson Codec.jsonToModification (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island),
          HU.testCase "AllCreatures" $
            roundTrip "a1" Codec.affectedToJson Codec.jsonToAffected Affected.AllCreatures,
          HU.testCase "TheseObjects" $
            roundTrip "a2" Codec.affectedToJson Codec.jsonToAffected (Affected.TheseObjects (Set.fromList [ObjectId.MkObjectId 1]))
        ],
      Tasty.testGroup
        "effect"
        [ HU.testCase "DealDamage" $
            roundTrip "e1" Codec.effectToJson Codec.jsonToEffect (Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 3)),
          HU.testCase "ModifyTarget" $
            roundTrip "e2" Codec.effectToJson Codec.jsonToEffect (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (SlotName.MkSlotName (Text.pack "t"))),
          HU.testCase "AddMana" $
            roundTrip "e3" Codec.effectToJson Codec.jsonToEffect (Effect.AddMana (ManaType.Colored Color.Green)),
          HU.testCase "ExileAllGraveyards" $
            roundTrip "e4" Codec.effectToJson Codec.jsonToEffect Effect.ExileAllGraveyards
        ],
      Tasty.testGroup
        "records"
        [ HU.testCase "TypeLine" $
            roundTrip
              "tl"
              Codec.typeLineToJson
              Codec.jsonToTypeLine
              (TypeLine.MkTypeLine (Set.singleton Supertype.Basic) (Set.singleton CardType.Land) (Set.singleton Subtype.Mountain)),
          HU.testCase "ActivatedAbility" $
            roundTrip
              "aa"
              Codec.activatedAbilityToJson
              Codec.jsonToActivatedAbility
              ( ActivatedAbility.MkActivatedAbility
                  (AbilityCost.MkAbilityCost Nothing [AdditionalCost.TapSelf])
                  [Effect.AddMana (ManaType.Colored Color.Green)]
                  (Map.fromList [(SlotName.MkSlotName (Text.pack "t"), TargetSpec.CreatureTarget)])
              ),
          HU.testCase "ReplacementEffect" $
            roundTrip
              "re"
              Codec.replacementEffectToJson
              Codec.jsonToReplacementEffect
              (ReplacementEffect.RedirectZoneChange Zone.Graveyard Zone.Exile)
        ],
      Tasty.testGroup
        "honesty round-trip over allPrintings"
        [ HU.testCase "P1: jsonToPrinting . printingToJson == Right" $
            mapM_ (\p -> HU.assertEqual (show (CardT.name (Printing.card p))) (Right p) (Codec.jsonToPrinting (Codec.printingToJson p))) (Cards.allPrintings cards),
          HU.testCase "P2: through text" $
            mapM_
              (\p -> HU.assertEqual (show (CardT.name (Printing.card p))) (Right p) (J.parse (J.render (Codec.printingToJson p)) >>= Codec.jsonToPrinting))
              (Cards.allPrintings cards)
        ]
    ]
