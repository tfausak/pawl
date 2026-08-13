module Pawl.Codec.ReplacementEffect where

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
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
--
-- Every arm here is a pattern paired with a rewrite, so all but two take a
-- 'Common.tuple'. Under the #1305 decision each owes a record of its own; that
-- lands with the payload-records unit.
codec :: Codec.Codec ReplacementEffect.ReplacementEffect
codec =
  Arm.tagged
    encode
    [ Arm.payload "ZoneChangeR" (Common.tuple ZoneChangePattern.codec Zone.codec) (uncurry ReplacementEffect.ZoneChangeR),
      Arm.payload "EntryR" (Common.tuple filterCodec EntryRewrite.codec) (uncurry ReplacementEffect.EntryR),
      Arm.payload "DamageR" (Common.tuple DamagePattern.codec DamageRewrite.codec) (uncurry ReplacementEffect.DamageR),
      Arm.payload "DestructionR" DestructionRewrite.codec ReplacementEffect.DestructionR,
      Arm.payload "CounterR" (Common.tuple CounterPattern.codec Scaling.codec) (uncurry ReplacementEffect.CounterR),
      Arm.payload "TokenR" (Common.tuple TokenPattern.codec Scaling.codec) (uncurry ReplacementEffect.TokenR),
      Arm.payload "TurnUpR" (Common.tuple filterCodec TurnUpRewrite.codec) (uncurry ReplacementEffect.TurnUpR),
      Arm.payload "PhaseR" PhasePattern.codec ReplacementEffect.PhaseR
    ]
  where
    filterCodec = Filter.codec Keyword.codec
    encode re = case re of
      ReplacementEffect.ZoneChangeR p z ->
        Common.tagged "ZoneChangeR" . Just . Value.array $ [Codec.encode ZoneChangePattern.codec p, Codec.encode Zone.codec z]
      ReplacementEffect.EntryR p r ->
        Common.tagged "EntryR" . Just . Value.array $ [Codec.encode filterCodec p, Codec.encode EntryRewrite.codec r]
      ReplacementEffect.DamageR p r ->
        Common.tagged "DamageR" . Just . Value.array $ [Codec.encode DamagePattern.codec p, Codec.encode DamageRewrite.codec r]
      ReplacementEffect.DestructionR r ->
        Common.tagged "DestructionR" . Just $ Codec.encode DestructionRewrite.codec r
      ReplacementEffect.CounterR p sc ->
        Common.tagged "CounterR" . Just . Value.array $ [Codec.encode CounterPattern.codec p, Codec.encode Scaling.codec sc]
      ReplacementEffect.TokenR p sc ->
        Common.tagged "TokenR" . Just . Value.array $ [Codec.encode TokenPattern.codec p, Codec.encode Scaling.codec sc]
      ReplacementEffect.TurnUpR p r ->
        Common.tagged "TurnUpR" . Just . Value.array $ [Codec.encode filterCodec p, Codec.encode TurnUpRewrite.codec r]
      ReplacementEffect.PhaseR p ->
        Common.tagged "PhaseR" . Just $ Codec.encode PhasePattern.codec p
