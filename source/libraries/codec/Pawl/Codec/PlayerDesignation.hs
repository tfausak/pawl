module Pawl.Codec.PlayerDesignation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PlayerDesignation as PlayerDesignation

codec :: Codec.Codec PlayerDesignation.PlayerDesignation
codec = Arm.enum
