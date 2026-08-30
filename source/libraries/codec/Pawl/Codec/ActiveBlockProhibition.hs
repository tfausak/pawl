{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveBlockProhibition where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveBlockProhibition as ActiveBlockProhibition

codec :: Codec.Codec ActiveBlockProhibition.ActiveBlockProhibition
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveBlockProhibition.source
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveBlockProhibition.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveBlockProhibition.expiry
  object <- Fields.required "object" ObjectId.codec ActiveBlockProhibition.object
  pure
    ActiveBlockProhibition.MkActiveBlockProhibition
      { ActiveBlockProhibition.source = source,
        ActiveBlockProhibition.timestamp = timestamp,
        ActiveBlockProhibition.expiry = expiry,
        ActiveBlockProhibition.object = object
      }
