-- | The @Toughness ⇆ Json@ codec (#481).
module Pawl.Codec.Toughness where

import Data.Text (Text)
import qualified Pawl.Codec.Quantity as Quantity
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Toughness as Toughness

toughnessToJson :: Toughness.Toughness -> Value
toughnessToJson (Toughness.MkToughness q) = Quantity.toJson q

jsonToToughness :: Value -> Either Text Toughness.Toughness
jsonToToughness value = Toughness.MkToughness <$> Quantity.fromJson value
