-- | The @ReplacementEffect ⇆ Json@ codec (#481).
module Pawl.Codec.ReplacementEffect where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.CounterPattern (counterPatternToJson, jsonToCounterPattern)
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Codec.DamageRewrite as DamageRewrite
import qualified Pawl.Codec.DestructionRewrite as DestructionRewrite
import Pawl.Codec.EntryRewrite (entryRewriteToJson, jsonToEntryRewrite)
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.PhasePattern as PhasePattern
import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.Codec.TokenPattern as TokenPattern
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Codec.ZoneChangePattern as ZoneChangePattern
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

replacementEffectToJson :: ReplacementEffect.ReplacementEffect -> Value
replacementEffectToJson re = case re of
  ReplacementEffect.ZoneChangeR p z ->
    Json.tagged (Text.pack "ZoneChangeR") (Just (Array (MkArray [ZoneChangePattern.toJson p, Zone.toJson z])))
  ReplacementEffect.EntryR r ->
    Json.tagged (Text.pack "EntryR") (Just (entryRewriteToJson r))
  ReplacementEffect.DamageR p r ->
    Json.tagged (Text.pack "DamageR") (Just (Array (MkArray [DamagePattern.toJson p, DamageRewrite.toJson r])))
  ReplacementEffect.DestructionR r ->
    Json.tagged (Text.pack "DestructionR") (Just (DestructionRewrite.toJson r))
  ReplacementEffect.CounterR p s ->
    Json.tagged (Text.pack "CounterR") (Just (Array (MkArray [counterPatternToJson p, Scaling.toJson s])))
  ReplacementEffect.TokenR p s ->
    Json.tagged (Text.pack "TokenR") (Just (Array (MkArray [TokenPattern.toJson p, Scaling.toJson s])))
  ReplacementEffect.PhaseR p ->
    Json.tagged (Text.pack "PhaseR") (Just (PhasePattern.toJson p))

jsonToReplacementEffect :: Value -> Either Text ReplacementEffect.ReplacementEffect
jsonToReplacementEffect value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ZoneChangeR", Just (Array (MkArray [p, z]))) -> do
      pattern_ <- ZoneChangePattern.fromJson p
      dest <- Zone.fromJson z
      pure (ReplacementEffect.ZoneChangeR pattern_ dest)
    ("EntryR", Just v) -> fmap ReplacementEffect.EntryR (jsonToEntryRewrite v)
    ("DamageR", Just (Array (MkArray [p, r]))) -> do
      pattern_ <- DamagePattern.fromJson p
      rewrite <- DamageRewrite.fromJson r
      pure (ReplacementEffect.DamageR pattern_ rewrite)
    ("DestructionR", Just v) -> fmap ReplacementEffect.DestructionR (DestructionRewrite.fromJson v)
    ("CounterR", Just (Array (MkArray [p, s]))) -> do
      pattern_ <- jsonToCounterPattern p
      scaling <- Scaling.fromJson s
      pure (ReplacementEffect.CounterR pattern_ scaling)
    ("TokenR", Just (Array (MkArray [p, s]))) -> do
      pattern_ <- TokenPattern.fromJson p
      scaling <- Scaling.fromJson s
      pure (ReplacementEffect.TokenR pattern_ scaling)
    ("PhaseR", Just v) -> fmap ReplacementEffect.PhaseR (PhasePattern.fromJson v)
    _ -> Left (Text.pack "unknown ReplacementEffect: " <> t)
