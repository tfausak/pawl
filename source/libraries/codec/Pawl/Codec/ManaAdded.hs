{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManaAdded where

import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManaAdded as ManaAdded

-- | A bare object keyed by the record's field names, every key required for
-- Pawl.Codec.TappedForMana's reason: a log entry whose one producer
-- (Pawl.Engine.Cost.tapForManaWith) always knows all three.
codec :: Codec.Codec ManaAdded.ManaAdded
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec ManaAdded.player
  source <- Fields.required "source" ObjectId.codec ManaAdded.source
  mana <- Fields.required "mana" (Common.set ManaType.codec) ManaAdded.mana
  pure ManaAdded.MkManaAdded {ManaAdded.player = player, ManaAdded.source = source, ManaAdded.mana = mana}
