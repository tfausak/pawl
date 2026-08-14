module Pawl.Codec.Layout where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Layout as Layout

codec :: Codec.Codec Layout.Layout
codec = Arm.enum
