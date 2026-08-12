module Pawl.Codec.Comparison where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Comparison as Comparison

toJson :: Comparison.Comparison -> Value.Value
toJson c = Common.nullary $ case c of
  Comparison.Exactly -> "Exactly"
  Comparison.AtLeast -> "AtLeast"
  Comparison.AtMost -> "AtMost"

fromJson :: Value.Value -> Either Text.Text Comparison.Comparison
fromJson =
  Common.decodeNullary
    "Comparison"
    [ ("Exactly", Comparison.Exactly),
      ("AtLeast", Comparison.AtLeast),
      ("AtMost", Comparison.AtMost)
    ]
