{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveAttackProhibition where

import qualified Pawl.Codec.AimedAt as AimedAt
import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition

codec :: Codec.Codec ActiveAttackProhibition.ActiveAttackProhibition
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveAttackProhibition.source
  controller <- Fields.required "controller" PlayerId.codec ActiveAttackProhibition.controller
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveAttackProhibition.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveAttackProhibition.expiry
  affected <- Fields.required "affected" (RestrictedCreatures.codec ObjectId.codec) ActiveAttackProhibition.affected
  aimedAt <- Fields.defaulted "aimedAt" Nothing (Common.maybe AimedAt.codec) ActiveAttackProhibition.aimedAt
  pure
    ActiveAttackProhibition.MkActiveAttackProhibition
      { ActiveAttackProhibition.source = source,
        ActiveAttackProhibition.controller = controller,
        ActiveAttackProhibition.timestamp = timestamp,
        ActiveAttackProhibition.expiry = expiry,
        ActiveAttackProhibition.affected = affected,
        ActiveAttackProhibition.aimedAt = aimedAt
      }
