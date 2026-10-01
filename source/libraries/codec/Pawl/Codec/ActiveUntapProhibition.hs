{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveUntapProhibition where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveUntapProhibition as ActiveUntapProhibition

codec :: Codec.Codec ActiveUntapProhibition.ActiveUntapProhibition
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveUntapProhibition.source
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveUntapProhibition.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveUntapProhibition.expiry
  object <- Fields.required "object" ObjectId.codec ActiveUntapProhibition.object
  pure
    ActiveUntapProhibition.MkActiveUntapProhibition
      { ActiveUntapProhibition.source = source,
        ActiveUntapProhibition.timestamp = timestamp,
        ActiveUntapProhibition.expiry = expiry,
        ActiveUntapProhibition.object = object
      }
