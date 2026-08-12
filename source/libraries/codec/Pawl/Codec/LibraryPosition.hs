module Pawl.Codec.LibraryPosition where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.LibraryPosition as LibraryPosition

codec :: Codec.Codec LibraryPosition.LibraryPosition
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Top" LibraryPosition.Top,
      Arm.nullary "Bottom" LibraryPosition.Bottom
    ]
  where
    encode p = Common.nullary $ case p of
      LibraryPosition.Top -> "Top"
      LibraryPosition.Bottom -> "Bottom"
