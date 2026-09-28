module Pawl.Codec.PlanarDieFace where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PlanarDieFace as PlanarDieFace

codec :: Codec.Codec PlanarDieFace.PlanarDieFace
codec = Arm.enum
