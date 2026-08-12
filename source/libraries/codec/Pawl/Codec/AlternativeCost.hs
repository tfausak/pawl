module Pawl.Codec.AlternativeCost where

import qualified Data.Text as Text
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.AlternativeCost as AlternativeCost

-- | 'cost' is required and 'condition' defaults to Nothing, which is the honest
-- default here where Pawl.Codec.Cost's 'mana' has none: an absent condition means
-- CR 118.9's unconditioned alternative, the case every printing but
-- Asmoranomardicadaistinaculdacar's is.
--
-- NESTED rather than flattened into the Cost's own keys, so this codec states no
-- opinion about what a Cost looks like.
toJson :: AlternativeCost.AlternativeCost -> Value.Value
toJson a =
  Value.object
    ( Common.optionalPair "condition" Nothing (Common.encodeMaybe Condition.toJson) (AlternativeCost.condition a)
        <> Common.requiredPair "cost" (Codec.encode (Cost.codec Keyword.codec)) (AlternativeCost.cost a)
    )

fromJson :: Value.Value -> Either Text.Text AlternativeCost.AlternativeCost
fromJson value = do
  ps <- Common.asObject value
  condition <- Common.defaultedField "condition" Nothing (Common.decodeMaybe Condition.fromJson) ps
  cost <- Common.field "cost" ps >>= Codec.decode (Cost.codec Keyword.codec)
  pure AlternativeCost.MkAlternativeCost {AlternativeCost.condition = condition, AlternativeCost.cost = cost}
