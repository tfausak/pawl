{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CardIdentity where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CardIdentity as CardIdentity

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec CardIdentity.CardIdentity
codec = Fields.object $ do
  serial <- Fields.required "serial" Common.natural CardIdentity.serial
  startingOwner <- Fields.required "startingOwner" PlayerId.codec CardIdentity.startingOwner
  pure
    CardIdentity.MkCardIdentity
      { CardIdentity.serial = serial,
        CardIdentity.startingOwner = startingOwner
      }
