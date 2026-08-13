module Pawl.Codec.Mill where

import qualified Pawl.Codec.MillTally as MillTally
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Mill as Mill

-- | The tally is ELIDED when absent, as Destroy's bound slot is.
codec :: Codec.Codec Mill.Mill
codec =
  Fields.object $
    Mill.MkMill
      <$> Fields.required "player" PlayerRef.codec Mill.player
      <*> Fields.required "quantity" Quantity.codec Mill.quantity
      <*> Fields.defaulted "tally" Nothing (Common.maybe MillTally.codec) Mill.tally
