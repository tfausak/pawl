module Pawl.Codec.Clause where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Clause as Clause

toJson :: (Eq card) => (card -> Value.Value) -> Clause.Clause card -> Value.Value
toJson codec c =
  Common.object $
    Common.optionalPair "effects" Seq.empty (Common.encodeSeq (Effect.toJson codec)) (Clause.effects c)

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Clause.Clause card)
fromJson decode value = do
  ps <- Common.asObject value
  es <- Common.defaultedField "effects" Seq.empty (Common.decodeSeq (Effect.fromJson decode)) ps
  pure (Clause.MkClause es)
