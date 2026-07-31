-- | The @Toughness ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Toughness where

import Data.Text (Text)
import Pawl.Codec.Quantity (jsonToQuantity, quantityToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Toughness as Toughness

toughnessToJson :: Toughness.Toughness -> Value
toughnessToJson (Toughness.MkToughness q) = quantityToJson q

jsonToToughness :: Value -> Either Text Toughness.Toughness
jsonToToughness value = Toughness.MkToughness <$> jsonToQuantity value
