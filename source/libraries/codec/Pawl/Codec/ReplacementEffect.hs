module Pawl.Codec.ReplacementEffect where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CounterR as CounterR
import qualified Pawl.Codec.DamageR as DamageR
import qualified Pawl.Codec.DestructionRewrite as DestructionRewrite
import qualified Pawl.Codec.EntryR as EntryR
import qualified Pawl.Codec.PhasePattern as PhasePattern
import qualified Pawl.Codec.TokenR as TokenR
import qualified Pawl.Codec.TurnUpR as TurnUpR
import qualified Pawl.Codec.UntapRewrite as UntapRewrite
import qualified Pawl.Codec.ZoneChangeR as ZoneChangeR
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
--
-- The effect codec is a PARAMETER rather than an import, for the reason
-- Pawl.Types.DamageR gives: the DamageR arm carries CR 615.5's riders and the
-- EntryR arm CR 614.1c's as-enters effects.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (ReplacementEffect.ReplacementEffect effect)
codec effectCodec =
  Arm.tagged
    [ Arm.payload "ZoneChangeR" ZoneChangeR.codec ReplacementEffect.ZoneChangeR (\x -> case x of ReplacementEffect.ZoneChangeR y -> Just y; _ -> Nothing),
      Arm.payload "EntryR" (EntryR.codec effectCodec) ReplacementEffect.EntryR (\x -> case x of ReplacementEffect.EntryR y -> Just y; _ -> Nothing),
      Arm.payload "DamageR" (DamageR.codec effectCodec) ReplacementEffect.DamageR (\x -> case x of ReplacementEffect.DamageR y -> Just y; _ -> Nothing),
      Arm.payload "DestructionR" DestructionRewrite.codec ReplacementEffect.DestructionR (\x -> case x of ReplacementEffect.DestructionR y -> Just y; _ -> Nothing),
      Arm.payload "CounterR" CounterR.codec ReplacementEffect.CounterR (\x -> case x of ReplacementEffect.CounterR y -> Just y; _ -> Nothing),
      Arm.payload "TokenR" TokenR.codec ReplacementEffect.TokenR (\x -> case x of ReplacementEffect.TokenR y -> Just y; _ -> Nothing),
      Arm.payload "TurnUpR" TurnUpR.codec ReplacementEffect.TurnUpR (\x -> case x of ReplacementEffect.TurnUpR y -> Just y; _ -> Nothing),
      Arm.payload "UntapR" UntapRewrite.codec ReplacementEffect.UntapR (\x -> case x of ReplacementEffect.UntapR y -> Just y; _ -> Nothing),
      Arm.payload "PhaseR" PhasePattern.codec ReplacementEffect.PhaseR (\x -> case x of ReplacementEffect.PhaseR y -> Just y; _ -> Nothing)
    ]
