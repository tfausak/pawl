{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LifeLoss where

import qualified Pawl.Codec.LifeLossCause as LifeLossCause
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LifeLoss as LifeLoss
import qualified Pawl.Types.LifeLossCause as LifeLossCause.Type

-- | Pawl.Codec.PlayerQuantity's two keys, so a printing that loses life is
-- written exactly as it was before the cause existed, plus a "cause" ELIDED at
-- CR 119.3's ordinary one -- which is every printing in data\/cards\/.
codec :: Codec.Codec LifeLoss.LifeLoss
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec LifeLoss.player
  quantity <- Fields.required "quantity" Quantity.codec LifeLoss.quantity
  cause <- Fields.defaulted "cause" LifeLossCause.Type.ByEffect LifeLossCause.codec LifeLoss.cause
  pure
    LifeLoss.MkLifeLoss
      { LifeLoss.player = player,
        LifeLoss.quantity = quantity,
        LifeLoss.cause = cause
      }
