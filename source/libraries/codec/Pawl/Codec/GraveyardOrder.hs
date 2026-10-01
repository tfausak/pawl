module Pawl.Codec.GraveyardOrder where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.GraveyardOrder as GraveyardOrder

-- | Nullary tags, derived from the type by Arm.enum.
codec :: Codec.Codec GraveyardOrder.GraveyardOrder
codec = Arm.enum
