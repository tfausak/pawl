module Pawl.Codec.UnlessPaid where

import qualified Data.Text as Text
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.UnlessPaid as UnlessPaid

-- | Both keys are required: an "unless" with no payer names nobody and one with
-- no cost offers nothing, so neither has a default an absent key could mean.
toJson :: UnlessPaid.UnlessPaid -> Value.Value
toJson u =
  Common.object
    ( Common.requiredPair "payer" SlotName.toJson (UnlessPaid.payer u)
        <> Common.requiredPair "cost" (Cost.toJson Keyword.toJson) (UnlessPaid.cost u)
    )

fromJson :: Value.Value -> Either Text.Text UnlessPaid.UnlessPaid
fromJson value = do
  ps <- Common.asObject value
  p <- Common.field "payer" ps >>= SlotName.fromJson
  c <- Common.field "cost" ps >>= Cost.fromJson Keyword.fromJson
  pure UnlessPaid.MkUnlessPaid {UnlessPaid.payer = p, UnlessPaid.cost = c}
