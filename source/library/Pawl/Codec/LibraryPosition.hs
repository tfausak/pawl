module Pawl.Codec.LibraryPosition where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.LibraryPosition as LibraryPosition

codec :: Codec.Codec LibraryPosition.LibraryPosition
codec = Arm.enum
