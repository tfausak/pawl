{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerQuantity where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by whichever Pawl.Codec.Effect arm carries it, which is what keeps
-- seven arms sharing one payload codec without sharing a tag.
codec :: Codec.Codec PlayerQuantity.PlayerQuantity
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec PlayerQuantity.player
  quantity <- Fields.required "quantity" Quantity.codec PlayerQuantity.quantity
  pure
    PlayerQuantity.MkPlayerQuantity
      { PlayerQuantity.player = player,
        PlayerQuantity.quantity = quantity
      }
