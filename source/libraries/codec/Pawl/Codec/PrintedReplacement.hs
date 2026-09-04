{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PrintedReplacement where

import qualified Data.Set as Set
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.Zone as Zone
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
  (Typeable.Typeable card, Eq card, Typeable.Typeable effect, Eq effect) =>
  Codec.Codec card ->
  Codec.Codec effect ->
  Codec.Codec (PrintedReplacement.PrintedReplacement card effect)
codec cardCodec effectCodec = Fields.object $ do
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) PrintedReplacement.condition
  effect <- Fields.required "effect" (ReplacementEffect.codec cardCodec effectCodec) PrintedReplacement.effect
  functionsFrom <- Fields.defaulted "functionsFrom" Set.empty (Common.set Zone.codec) PrintedReplacement.functionsFrom
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) PrintedReplacement.name
  pure
    PrintedReplacement.MkPrintedReplacement
      { PrintedReplacement.condition = condition,
        PrintedReplacement.effect = effect,
        PrintedReplacement.functionsFrom = functionsFrom,
        PrintedReplacement.name = name
      }
