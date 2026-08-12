module Pawl.Codec.ReplacementEffect where

import qualified Data.Text as Text
import qualified Pawl.Codec.CounterPattern as CounterPattern
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Codec.DamageRewrite as DamageRewrite
import qualified Pawl.Codec.DestructionRewrite as DestructionRewrite
import qualified Pawl.Codec.EntryRewrite as EntryRewrite
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PhasePattern as PhasePattern
import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.Codec.TokenPattern as TokenPattern
import qualified Pawl.Codec.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Codec.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

toJson :: ReplacementEffect.ReplacementEffect -> Value.Value
toJson re = case re of
  ReplacementEffect.ZoneChangeR p z ->
    Common.tagged "ZoneChangeR" . Just . Value.array $ [ZoneChangePattern.toJson p, Codec.encode Zone.codec z]
  ReplacementEffect.EntryR p r ->
    Common.tagged "EntryR" . Just . Value.array $ [Codec.encode (Filter.codec Keyword.codec) p, EntryRewrite.toJson r]
  ReplacementEffect.DamageR p r ->
    Common.tagged "DamageR" . Just . Value.array $ [DamagePattern.toJson p, DamageRewrite.toJson r]
  ReplacementEffect.DestructionR r ->
    Common.tagged "DestructionR" . Just $ Codec.encode DestructionRewrite.codec r
  ReplacementEffect.CounterR p sc ->
    Common.tagged "CounterR" . Just . Value.array $ [CounterPattern.toJson p, Codec.encode Scaling.codec sc]
  ReplacementEffect.TokenR p sc ->
    Common.tagged "TokenR" . Just . Value.array $ [TokenPattern.toJson p, Codec.encode Scaling.codec sc]
  ReplacementEffect.TurnUpR p r ->
    Common.tagged "TurnUpR" . Just . Value.array $ [Codec.encode (Filter.codec Keyword.codec) p, Codec.encode TurnUpRewrite.codec r]
  ReplacementEffect.PhaseR p ->
    Common.tagged "PhaseR" . Just $ Codec.encode PhasePattern.codec p

fromJson :: Value.Value -> Either Text.Text ReplacementEffect.ReplacementEffect
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("ZoneChangeR", Just (Value.Array (Array.MkArray [p, z]))) -> do
      pattern_ <- ZoneChangePattern.fromJson p
      dest <- Codec.decode Zone.codec z
      pure (ReplacementEffect.ZoneChangeR pattern_ dest)
    ("EntryR", Just (Value.Array (Array.MkArray [p, r]))) -> do
      pattern_ <- Codec.decode (Filter.codec Keyword.codec) p
      rewrite <- EntryRewrite.fromJson r
      pure (ReplacementEffect.EntryR pattern_ rewrite)
    ("DamageR", Just (Value.Array (Array.MkArray [p, r]))) -> do
      pattern_ <- DamagePattern.fromJson p
      rewrite <- DamageRewrite.fromJson r
      pure (ReplacementEffect.DamageR pattern_ rewrite)
    ("DestructionR", Just v) -> ReplacementEffect.DestructionR <$> Codec.decode DestructionRewrite.codec v
    ("CounterR", Just (Value.Array (Array.MkArray [p, sc]))) -> do
      pattern_ <- CounterPattern.fromJson p
      scaling <- Codec.decode Scaling.codec sc
      pure (ReplacementEffect.CounterR pattern_ scaling)
    ("TokenR", Just (Value.Array (Array.MkArray [p, sc]))) -> do
      pattern_ <- TokenPattern.fromJson p
      scaling <- Codec.decode Scaling.codec sc
      pure (ReplacementEffect.TokenR pattern_ scaling)
    ("TurnUpR", Just (Value.Array (Array.MkArray [p, r]))) -> do
      pattern_ <- Codec.decode (Filter.codec Keyword.codec) p
      rewrite <- Codec.decode TurnUpRewrite.codec r
      pure (ReplacementEffect.TurnUpR pattern_ rewrite)
    ("PhaseR", Just v) -> ReplacementEffect.PhaseR <$> Codec.decode PhasePattern.codec v
    _ -> Left . Text.pack $ "unknown ReplacementEffect: " <> t
