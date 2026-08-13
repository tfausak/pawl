module Pawl.Codec.Mode where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Clause as Clause
import qualified Pawl.Codec.TargetSlot as TargetSlot
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Mode as Mode

toJson :: (Eq card) => (card -> Value.Value) -> Mode.Mode card -> Value.Value
toJson codec m =
  Value.object $
    Common.optionalPair "clauses" Seq.empty (Common.encodeSeq (Clause.toJson codec)) (Mode.clauses m)
      <> Common.optionalPair "targetSlots" Map.empty (Codec.encode TargetSlot.codecMap) (Mode.targetSlots m)

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Mode.Mode card)
fromJson decode value = do
  ps <- Common.asObject value
  cs <- Common.defaultedField "clauses" Seq.empty (Common.decodeSeq (Clause.fromJson decode)) ps
  ts <- Common.defaultedField "targetSlots" Map.empty (Codec.decode TargetSlot.codecMap) ps
  pure (Mode.MkMode cs ts)
