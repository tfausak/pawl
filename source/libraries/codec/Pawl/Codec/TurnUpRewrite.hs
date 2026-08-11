module Pawl.Codec.TurnUpRewrite where

import qualified Data.Text as Text
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite

toJson :: TurnUpRewrite.TurnUpRewrite -> Value.Value
toJson r = case r of
  TurnUpRewrite.WithCounters kind n -> Common.tagged "WithCounters" . Just . Value.array $ [Codec.encode (CounterKind.codec Keyword.codec) kind, Common.encodeNatural n]
  TurnUpRewrite.MayAttachTo f -> Common.tagged "MayAttachTo" . Just $ Codec.encode (Filter.codec Keyword.codec) f

fromJson :: Value.Value -> Either Text.Text TurnUpRewrite.TurnUpRewrite
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("WithCounters", Just (Value.Array (Array.MkArray [k, n]))) -> do
      kind <- Codec.decode (CounterKind.codec Keyword.codec) k
      count <- Common.decodeNatural n
      pure (TurnUpRewrite.WithCounters kind count)
    ("MayAttachTo", Just f) -> TurnUpRewrite.MayAttachTo <$> Codec.decode (Filter.codec Keyword.codec) f
    _ -> Left . Text.pack $ "unknown TurnUpRewrite: " <> t
