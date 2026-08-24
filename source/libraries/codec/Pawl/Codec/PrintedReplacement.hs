{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PrintedReplacement where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement

-- | A bare object keyed by the record's field names, the shape Pawl.Codec.Replace
-- already writes for the floating twin. 'Fields.defaulted' elides the
-- unconditional case rather than writing an explicit null.
--
-- The effect codec is a PARAMETER rather than an import, for the reason
-- Pawl.Types.DamageR gives.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (PrintedReplacement.PrintedReplacement effect)
codec effectCodec = Fields.object $ do
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) PrintedReplacement.condition
  effect <- Fields.required "effect" (ReplacementEffect.codec effectCodec) PrintedReplacement.effect
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) PrintedReplacement.name
  pure
    PrintedReplacement.MkPrintedReplacement
      { PrintedReplacement.condition = condition,
        PrintedReplacement.effect = effect,
        PrintedReplacement.name = name
      }
