-- | The @Color ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Color where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Color as Color

colorToJson :: Color.Color -> Value
colorToJson c = Json.nullary . Text.pack $ case c of
  Color.White -> "White"
  Color.Blue -> "Blue"
  Color.Black -> "Black"
  Color.Red -> "Red"
  Color.Green -> "Green"

jsonToColor :: Value -> Either Text Color.Color
jsonToColor =
  Json.decodeNullary
    (Text.pack "Color")
    [ (Text.pack "White", Color.White),
      (Text.pack "Blue", Color.Blue),
      (Text.pack "Black", Color.Black),
      (Text.pack "Red", Color.Red),
      (Text.pack "Green", Color.Green)
    ]
