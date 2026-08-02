module Pawl.Codec.Color where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Color as Color

toJson :: Color.Color -> Value.Value
toJson c = Common.nullary $ case c of
  Color.White -> "White"
  Color.Blue -> "Blue"
  Color.Black -> "Black"
  Color.Red -> "Red"
  Color.Green -> "Green"

fromJson :: Value.Value -> Either Text.Text Color.Color
fromJson =
  Common.decodeNullary
    "Color"
    [ ("White", Color.White),
      ("Blue", Color.Blue),
      ("Black", Color.Black),
      ("Red", Color.Red),
      ("Green", Color.Green)
    ]
