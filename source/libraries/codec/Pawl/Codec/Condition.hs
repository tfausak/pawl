module Pawl.Codec.Condition where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Condition as Condition

-- | A BARE OBJECT keyed by the record's field names, which is what every
-- single-constructor record in this codec writes. Common.tagged is for sum
-- types, where the tag picks a constructor; Condition has exactly one, so a tag
-- would carry no information and every parent that holds a Condition is already
-- tagged itself.
--
-- Naming the sides is what a positional array could not do: both are a
-- Quantity, so a card file that swapped them would decode silently into the
-- wrong condition. "measured" and "threshold" cannot be swapped by accident.
toJson :: Condition.Condition -> Value.Value
toJson condition =
  Common.object . concat $
    [ Common.requiredPair "measured" Quantity.toJson (Condition.measured condition),
      Common.requiredPair "comparison" Comparison.toJson (Condition.comparison condition),
      Common.requiredPair "threshold" Quantity.toJson (Condition.threshold condition)
    ]

fromJson :: Value.Value -> Either Text.Text Condition.Condition
fromJson value = do
  ps <- Common.asObject value
  m <- Common.field "measured" ps >>= Quantity.fromJson
  c <- Common.field "comparison" ps >>= Comparison.fromJson
  t <- Common.field "threshold" ps >>= Quantity.fromJson
  pure
    Condition.MkCondition
      { Condition.measured = m,
        Condition.comparison = c,
        Condition.threshold = t
      }
