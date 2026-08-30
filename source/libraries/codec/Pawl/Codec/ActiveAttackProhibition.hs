{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveAttackProhibition where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition

codec :: Codec.Codec ActiveAttackProhibition.ActiveAttackProhibition
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveAttackProhibition.source
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveAttackProhibition.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveAttackProhibition.expiry
  object <- Fields.required "object" ObjectId.codec ActiveAttackProhibition.object
  pure
    ActiveAttackProhibition.MkActiveAttackProhibition
      { ActiveAttackProhibition.source = source,
        ActiveAttackProhibition.timestamp = timestamp,
        ActiveAttackProhibition.expiry = expiry,
        ActiveAttackProhibition.object = object
      }
