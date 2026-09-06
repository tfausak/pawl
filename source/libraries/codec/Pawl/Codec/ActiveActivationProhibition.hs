{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveActivationProhibition where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveActivationProhibition as ActiveActivationProhibition

codec :: Codec.Codec ActiveActivationProhibition.ActiveActivationProhibition
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveActivationProhibition.source
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveActivationProhibition.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveActivationProhibition.expiry
  object <- Fields.required "object" ObjectId.codec ActiveActivationProhibition.object
  pure
    ActiveActivationProhibition.MkActiveActivationProhibition
      { ActiveActivationProhibition.source = source,
        ActiveActivationProhibition.timestamp = timestamp,
        ActiveActivationProhibition.expiry = expiry,
        ActiveActivationProhibition.object = object
      }
