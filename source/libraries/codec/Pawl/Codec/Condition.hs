module Pawl.Codec.Condition where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Condition as Condition

-- | TWO BARE OBJECT shapes, told apart by their keys rather than by
-- Common.tagged's "type": a comparison is @measured@/@comparison@/@threshold@
-- and a disjunction is a lone @any@. Naming the comparison's sides is what a
-- positional array could not do -- both are a Quantity, so a card file that
-- swapped them would decode silently into the wrong condition.
--
-- Untagged even though Condition is now a sum, which is the one place this
-- codec departs from that convention: the comparison shape is what every card
-- file in the pool already writes, and a key no comparison has separates the
-- two as surely as a tag would.
toJson :: Condition.Condition -> Value.Value
toJson condition = case condition of
  Condition.Compares m c t ->
    Common.object . concat $
      [ Common.requiredPair "measured" Quantity.toJson m,
        Common.requiredPair "comparison" Comparison.toJson c,
        Common.requiredPair "threshold" Quantity.toJson t
      ]
  Condition.Any cs -> Common.object (Common.requiredPair "any" (Common.array . fmap toJson) cs)

fromJson :: Value.Value -> Either Text.Text Condition.Condition
fromJson value = do
  ps <- Common.asObject value
  case Common.optionalField "any" ps of
    Just v -> Common.asArray v >>= fmap Condition.Any . traverse fromJson
    Nothing -> do
      m <- Common.field "measured" ps >>= Quantity.fromJson
      c <- Common.field "comparison" ps >>= Comparison.fromJson
      t <- Common.field "threshold" ps >>= Quantity.fromJson
      pure $ Condition.Compares m c t
