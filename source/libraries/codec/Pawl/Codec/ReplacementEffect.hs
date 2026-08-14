module Pawl.Codec.ReplacementEffect where

import qualified Pawl.Codec.CounterR as CounterR
import qualified Pawl.Codec.DamageR as DamageR
import qualified Pawl.Codec.DestructionRewrite as DestructionRewrite
import qualified Pawl.Codec.EntryR as EntryR
import qualified Pawl.Codec.PhasePattern as PhasePattern
import qualified Pawl.Codec.TokenR as TokenR
import qualified Pawl.Codec.TurnUpR as TurnUpR
import qualified Pawl.Codec.ZoneChangeR as ZoneChangeR
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec ReplacementEffect.ReplacementEffect
codec =
  Arm.tagged
    encode
    [ Arm.payload "ZoneChangeR" ZoneChangeR.codec ReplacementEffect.ZoneChangeR,
      Arm.payload "EntryR" EntryR.codec ReplacementEffect.EntryR,
      Arm.payload "DamageR" DamageR.codec ReplacementEffect.DamageR,
      Arm.payload "DestructionR" DestructionRewrite.codec ReplacementEffect.DestructionR,
      Arm.payload "CounterR" CounterR.codec ReplacementEffect.CounterR,
      Arm.payload "TokenR" TokenR.codec ReplacementEffect.TokenR,
      Arm.payload "TurnUpR" TurnUpR.codec ReplacementEffect.TurnUpR,
      Arm.payload "PhaseR" PhasePattern.codec ReplacementEffect.PhaseR
    ]
  where
    encode re = case re of
      ReplacementEffect.ZoneChangeR x ->
        Common.tagged "ZoneChangeR" . Just $ Codec.encode ZoneChangeR.codec x
      ReplacementEffect.EntryR x ->
        Common.tagged "EntryR" . Just $ Codec.encode EntryR.codec x
      ReplacementEffect.DamageR x ->
        Common.tagged "DamageR" . Just $ Codec.encode DamageR.codec x
      ReplacementEffect.DestructionR r ->
        Common.tagged "DestructionR" . Just $ Codec.encode DestructionRewrite.codec r
      ReplacementEffect.CounterR x ->
        Common.tagged "CounterR" . Just $ Codec.encode CounterR.codec x
      ReplacementEffect.TokenR x ->
        Common.tagged "TokenR" . Just $ Codec.encode TokenR.codec x
      ReplacementEffect.TurnUpR x ->
        Common.tagged "TurnUpR" . Just $ Codec.encode TurnUpR.codec x
      ReplacementEffect.PhaseR p ->
        Common.tagged "PhaseR" . Just $ Codec.encode PhasePattern.codec p
