{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveAttackRequirement where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement

codec :: Codec.Codec ActiveAttackRequirement.ActiveAttackRequirement
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActiveAttackRequirement.source
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveAttackRequirement.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveAttackRequirement.expiry
  attacker <- Fields.required "attacker" ObjectId.codec ActiveAttackRequirement.attacker
  defender <- Fields.required "defender" PlayerId.codec ActiveAttackRequirement.defender
  pure
    ActiveAttackRequirement.MkActiveAttackRequirement
      { ActiveAttackRequirement.source = source,
        ActiveAttackRequirement.timestamp = timestamp,
        ActiveAttackRequirement.expiry = expiry,
        ActiveAttackRequirement.attacker = attacker,
        ActiveAttackRequirement.defender = defender
      }
