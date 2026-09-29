{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RepeatIf where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RepeatIf as RepeatIf

-- | A bare object keyed by the record's field names, every one required, as
-- Pawl.Codec.ForEachNumber's are. The effect codec is a parameter for
-- Pawl.Codec.ForEach's reason.
codec ::
  (Typeable.Typeable effect) =>
  Codec.Codec effect ->
  Codec.Codec (RepeatIf.RepeatIf effect)
codec effectCodec = Fields.object $ do
  process <- Fields.required "process" (Common.seq effectCodec) RepeatIf.process
  condition <- Fields.required "condition" Condition.codec RepeatIf.condition
  ifHolds <- Fields.required "ifHolds" (Common.seq effectCodec) RepeatIf.ifHolds
  pure
    RepeatIf.MkRepeatIf
      { RepeatIf.process = process,
        RepeatIf.condition = condition,
        RepeatIf.ifHolds = ifHolds
      }
