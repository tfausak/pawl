{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TopOfLibrary where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
--
-- The depth is a full 'Pawl.Codec.Quantity' rather than a bare number, so Act on
-- Impulse's three is now @{"type":"Literal","value":3}@ and Commune with Lava's X
-- is an @InSlot@ naming it.
codec :: Codec.Codec TopOfLibrary.TopOfLibrary
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec TopOfLibrary.player
  count <- Fields.required "count" Quantity.codec TopOfLibrary.count
  pure TopOfLibrary.MkTopOfLibrary {TopOfLibrary.player = player, TopOfLibrary.count = count}
