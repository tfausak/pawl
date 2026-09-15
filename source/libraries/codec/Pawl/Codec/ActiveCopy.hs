{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveCopy where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveCopy as ActiveCopy

codec :: Codec.Codec ActiveCopy.ActiveCopy
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveCopy.source
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveCopy.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveCopy.expiry
  objects <- Fields.required "objects" (Common.set ObjectId.codec) ActiveCopy.objects
  snapshot <- Fields.required "snapshot" ProjectedCharacteristics.codec ActiveCopy.snapshot
  pure
    ActiveCopy.MkActiveCopy
      { ActiveCopy.source = source,
        ActiveCopy.timestamp = timestamp,
        ActiveCopy.expiry = expiry,
        ActiveCopy.objects = objects,
        ActiveCopy.snapshot = snapshot
      }
