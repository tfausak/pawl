{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveUnregeneratable where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveUnregeneratable as ActiveUnregeneratable

codec :: Codec.Codec ActiveUnregeneratable.ActiveUnregeneratable
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveUnregeneratable.source
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveUnregeneratable.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveUnregeneratable.expiry
  object <- Fields.required "object" ObjectId.codec ActiveUnregeneratable.object
  pure
    ActiveUnregeneratable.MkActiveUnregeneratable
      { ActiveUnregeneratable.source = source,
        ActiveUnregeneratable.timestamp = timestamp,
        ActiveUnregeneratable.expiry = expiry,
        ActiveUnregeneratable.object = object
      }
