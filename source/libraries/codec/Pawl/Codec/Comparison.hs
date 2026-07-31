-- | The @Comparison ⇆ Json@ codec (#481).
module Pawl.Codec.Comparison where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Comparison as Comparison

comparisonToJson :: Comparison.Comparison -> Value
comparisonToJson c = Json.nullary . Text.pack $ case c of
  Comparison.Exactly -> "Exactly"
  Comparison.AtLeast -> "AtLeast"
  Comparison.AtMost -> "AtMost"

jsonToComparison :: Value -> Either Text Comparison.Comparison
jsonToComparison =
  Json.decodeNullary
    (Text.pack "Comparison")
    [ (Text.pack "Exactly", Comparison.Exactly),
      (Text.pack "AtLeast", Comparison.AtLeast),
      (Text.pack "AtMost", Comparison.AtMost)
    ]
