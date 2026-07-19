-- Covers Pawl.Codec.
module Pawl.CodecSpec where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec as Codec
import qualified Pawl.Type.Affected as Affected
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
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

roundTrip :: (Eq a, Show a) => String -> (a -> Json.Value) -> (Json.Value -> Either Text a) -> a -> HU.Assertion
roundTrip label enc dec x = HU.assertEqual label (Right x) (dec (enc x))

tests :: Tasty.TestTree
tests =
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
        ]
    ]
