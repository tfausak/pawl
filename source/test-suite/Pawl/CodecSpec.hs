-- Covers Pawl.Codec.
module Pawl.CodecSpec where

import Data.Text (Text)
import qualified Pawl.Codec as Codec
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Json as Json
import qualified Pawl.Type.Keyword as Keyword
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
        ]
    ]
