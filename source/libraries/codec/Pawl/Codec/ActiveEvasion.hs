{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveEvasion where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveEvasion as ActiveEvasion

codec :: Codec.Codec ActiveEvasion.ActiveEvasion
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveEvasion.source
  controller <- Fields.required "controller" PlayerId.codec ActiveEvasion.controller
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveEvasion.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveEvasion.expiry
  affected <- Fields.required "affected" (Filter.codec Keyword.codec) ActiveEvasion.affected
  pure
    ActiveEvasion.MkActiveEvasion
      { ActiveEvasion.source = source,
        ActiveEvasion.controller = controller,
        ActiveEvasion.timestamp = timestamp,
        ActiveEvasion.expiry = expiry,
        ActiveEvasion.affected = affected
      }
