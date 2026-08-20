{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Mill where

import qualified Pawl.Codec.MillTally as MillTally
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Mill as Mill

-- | The tally and the slot are each ELIDED when absent, as Destroy's bound slot
-- is.
codec :: Codec.Codec Mill.Mill
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec Mill.player
  quantity <- Fields.required "quantity" Quantity.codec Mill.quantity
  tally <- Fields.defaulted "tally" Nothing (Common.maybe MillTally.codec) Mill.tally
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Mill.slot
  pure
    Mill.MkMill
      { Mill.player = player,
        Mill.quantity = quantity,
        Mill.tally = tally,
        Mill.slot = slot
      }
