{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.StoredResult where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.StoredResult as StoredResult

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec StoredResult.StoredResult
codec = Fields.object $ do
  sides <- Fields.required "sides" Common.natural StoredResult.sides
  value <- Fields.required "value" Common.natural StoredResult.value
  pure StoredResult.MkStoredResult {StoredResult.sides = sides, StoredResult.value = value}
