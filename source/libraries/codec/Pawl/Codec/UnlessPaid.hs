module Pawl.Codec.UnlessPaid where

import qualified Data.Text as Text
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.UnlessPaid as UnlessPaid

-- | Both keys are required: an "unless" with no payer names nobody and one with
-- no cost offers nothing, so neither has a default an absent key could mean.
toJson :: UnlessPaid.UnlessPaid -> Value.Value
toJson u =
  Value.object
    ( Common.requiredPair "payer" (Codec.encode SlotName.codec) (UnlessPaid.payer u)
        <> Common.requiredPair "cost" (Codec.encode (Cost.codec Keyword.codec)) (UnlessPaid.cost u)
    )

fromJson :: Value.Value -> Either Text.Text UnlessPaid.UnlessPaid
fromJson value = do
  ps <- Common.asObject value
  p <- Common.field "payer" ps >>= Codec.decode SlotName.codec
  c <- Common.field "cost" ps >>= Codec.decode (Cost.codec Keyword.codec)
  pure UnlessPaid.MkUnlessPaid {UnlessPaid.payer = p, UnlessPaid.cost = c}
