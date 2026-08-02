module Pawl.Codec.ReplacementEffect where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.CounterPattern as CounterPattern
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Codec.DamageRewrite as DamageRewrite
import qualified Pawl.Codec.DestructionRewrite as DestructionRewrite
import qualified Pawl.Codec.EntryRewrite as EntryRewrite
import qualified Pawl.Codec.PhasePattern as PhasePattern
import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.Codec.TokenPattern as TokenPattern
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Codec.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

toJson :: ReplacementEffect.ReplacementEffect -> Value.Value
toJson re = case re of
  ReplacementEffect.ZoneChangeR p z ->
    Common.tagged "ZoneChangeR" . Just . Common.array $ [ZoneChangePattern.toJson p, Zone.toJson z]
  ReplacementEffect.EntryR r ->
    Common.tagged "EntryR" . Just $ EntryRewrite.toJson r
  ReplacementEffect.DamageR p r ->
    Common.tagged "DamageR" . Just . Common.array $ [DamagePattern.toJson p, DamageRewrite.toJson r]
  ReplacementEffect.DestructionR r ->
    Common.tagged "DestructionR" . Just $ DestructionRewrite.toJson r
  ReplacementEffect.CounterR p sc ->
    Common.tagged "CounterR" . Just . Common.array $ [CounterPattern.toJson p, Scaling.toJson sc]
  ReplacementEffect.TokenR p sc ->
    Common.tagged "TokenR" . Just . Common.array $ [TokenPattern.toJson p, Scaling.toJson sc]
  ReplacementEffect.PhaseR p ->
    Common.tagged "PhaseR" . Just $ PhasePattern.toJson p

fromJson :: Value.Value -> Either Text.Text ReplacementEffect.ReplacementEffect
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("ZoneChangeR", Just (Value.Array (Array.MkArray [p, z]))) -> do
      pattern_ <- ZoneChangePattern.fromJson p
      dest <- Zone.fromJson z
      pure (ReplacementEffect.ZoneChangeR pattern_ dest)
    ("EntryR", Just v) -> ReplacementEffect.EntryR <$> EntryRewrite.fromJson v
    ("DamageR", Just (Value.Array (Array.MkArray [p, r]))) -> do
      pattern_ <- DamagePattern.fromJson p
      rewrite <- DamageRewrite.fromJson r
      pure (ReplacementEffect.DamageR pattern_ rewrite)
    ("DestructionR", Just v) -> ReplacementEffect.DestructionR <$> DestructionRewrite.fromJson v
    ("CounterR", Just (Value.Array (Array.MkArray [p, sc]))) -> do
      pattern_ <- CounterPattern.fromJson p
      scaling <- Scaling.fromJson sc
      pure (ReplacementEffect.CounterR pattern_ scaling)
    ("TokenR", Just (Value.Array (Array.MkArray [p, sc]))) -> do
      pattern_ <- TokenPattern.fromJson p
      scaling <- Scaling.fromJson sc
      pure (ReplacementEffect.TokenR pattern_ scaling)
    ("PhaseR", Just v) -> ReplacementEffect.PhaseR <$> PhasePattern.fromJson v
    _ -> Left . Text.pack $ "unknown ReplacementEffect: " <> t
