{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DieResult where

import qualified Data.Typeable as Typeable
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DieResult as DieResult

-- | A bare object keyed by the record's field names, the roller written with
-- whichever codec its parameter takes.
codec :: (Typeable.Typeable player) => Codec.Codec player -> Codec.Codec (DieResult.DieResult player)
codec player = Fields.object $ do
  roller <- Fields.required "roller" player DieResult.roller
  result <- Fields.required "result" Common.natural DieResult.result
  pure DieResult.MkDieResult {DieResult.roller = roller, DieResult.result = result}
