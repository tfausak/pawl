{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManaAbilityResolved where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManaAbilityResolved as ManaAbilityResolved

-- | A bare object keyed by the record's field names, both keys required for
-- Pawl.Codec.ManaAdded's reason: a log entry whose one producer
-- (Pawl.Engine.Cost.tapForManaWith) always knows both.
codec :: Codec.Codec ManaAbilityResolved.ManaAbilityResolved
codec = Fields.object $ do
  permanent <- Fields.required "permanent" ObjectId.codec ManaAbilityResolved.permanent
  amount <- Fields.required "amount" Common.natural ManaAbilityResolved.amount
  pure ManaAbilityResolved.MkManaAbilityResolved {ManaAbilityResolved.permanent = permanent, ManaAbilityResolved.amount = amount}
