{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Replace where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Codec.Uses as Uses
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Replace as Replace

-- | A bare object keyed by the record's field names. The positional payload this
-- replaces wrote the unconditional case as an explicit null in the fourth slot;
-- 'Fields.defaulted' elides it instead, which is the posture every other
-- optional rider in the DSL already takes.
codec :: Codec.Codec Replace.Replace
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec Replace.duration
  uses <- Fields.required "uses" Uses.codec Replace.uses
  origin <- Fields.required "origin" ReplacementOrigin.codec Replace.origin
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) Replace.condition
  effect <- Fields.required "effect" ReplacementEffect.codec Replace.effect
  pure
    Replace.MkReplace
      { Replace.duration = duration,
        Replace.uses = uses,
        Replace.origin = origin,
        Replace.condition = condition,
        Replace.effect = effect
      }
