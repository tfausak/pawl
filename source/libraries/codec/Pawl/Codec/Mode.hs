module Pawl.Codec.Mode where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Clause as Clause
import qualified Pawl.Codec.TargetSpec as TargetSpec
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Mode as Mode

toJson :: (Eq card) => (card -> Value.Value) -> Mode.Mode card -> Value.Value
toJson codec m =
  Common.object $
    Common.optionalPair "clauses" Seq.empty (Common.encodeSeq (Clause.toJson codec)) (Mode.clauses m)
      <> Common.optionalPair "targetSpecs" Map.empty TargetSpec.toJsonMap (Mode.targetSpecs m)

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Mode.Mode card)
fromJson decode value = do
  ps <- Common.asObject value
  cs <- Common.defaultedField "clauses" Seq.empty (Common.decodeSeq (Clause.fromJson decode)) ps
  ts <- Common.defaultedField "targetSpecs" Map.empty TargetSpec.fromJsonMap ps
  pure (Mode.MkMode cs ts)
