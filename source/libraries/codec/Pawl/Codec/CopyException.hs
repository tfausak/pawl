module Pawl.Codec.CopyException where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CopyException as CopyException

-- | The payload is a two-element array rather than a named object, so
-- 'powerToughness' is the ONE place its order is stated: both directions go
-- through that binding, which is what keeps the encoder from writing the pair
-- in an order the decoder does not read.
powerToughness :: Codec.Codec (Integer, Integer)
powerToughness = Common.tuple Common.integer Common.integer

codec :: Codec.Codec CopyException.CopyException
codec =
  Arm.tagged
    encode
    [ Arm.payload "SetPowerToughness" powerToughness (uncurry CopyException.SetPowerToughness)
    ]
  where
    encode e = case e of
      CopyException.SetPowerToughness p t ->
        Common.tagged "SetPowerToughness" . Just $ Codec.encode powerToughness (p, t)
