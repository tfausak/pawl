{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveObjectProhibition where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Prohibition as Prohibition
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveObjectProhibition as ActiveObjectProhibition

codec :: Codec.Codec ActiveObjectProhibition.ActiveObjectProhibition
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveObjectProhibition.source
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveObjectProhibition.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveObjectProhibition.expiry
  what <- Fields.required "what" Prohibition.codec ActiveObjectProhibition.what
  object <- Fields.required "object" ObjectId.codec ActiveObjectProhibition.object
  pure
    ActiveObjectProhibition.MkActiveObjectProhibition
      { ActiveObjectProhibition.source = source,
        ActiveObjectProhibition.timestamp = timestamp,
        ActiveObjectProhibition.expiry = expiry,
        ActiveObjectProhibition.what = what,
        ActiveObjectProhibition.object = object
      }
