module Pawl.Codec.Condition where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Condition as Condition

-- | A BARE OBJECT keyed by the record's field names, which is what every
-- single-constructor record in this codec writes (Pawl.Codec.Countering,
-- Pawl.Codec.Mode, Pawl.Codec.TriggeredAbility, ...). Common.tagged is for sum
-- types, where the tag picks a constructor; Condition has exactly one, so a tag
-- would carry no information and every parent that holds a Condition
-- (TriggerCondition.StateIs, Duration.ForAsLongAs, an ability's intervening
-- "if") is already tagged itself.
--
-- Naming the sides on the wire is what the positional array could not do: the
-- first and third elements were both a Quantity, so a card file that swapped
-- them decoded silently into the wrong condition. "measured" and "threshold"
-- cannot be swapped by accident.
--
-- Both sides go through Quantity.toJson, which is what keeps a Count-valued side
-- indistinguishable from the Count-on-the-left shape this type replaced:
-- Quantity.toJson's Count arm delegates to Count.toJson and emits no wrapper of
-- its own, so a `Quantity.Count c` is byte-for-byte the JSON `Count.toJson c`
-- used to produce. Naming the fields moved the three sides into a keyed object
-- and left every side's own payload alone, which is the whole of what the card
-- files' condition nodes changed by.
toJson :: Condition.Condition -> Value.Value
toJson condition =
  Common.object
    [ Common.pair "measured" . Quantity.toJson $ Condition.measured condition,
      Common.pair "comparison" . Comparison.toJson $ Condition.comparison condition,
      Common.pair "threshold" . Quantity.toJson $ Condition.threshold condition
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
