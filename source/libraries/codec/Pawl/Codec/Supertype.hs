module Pawl.Codec.Supertype where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Supertype as Supertype

codec :: Codec.Codec Supertype.Supertype
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Basic" Supertype.Basic,
      Arm.nullary "Legendary" Supertype.Legendary,
      Arm.nullary "Ongoing" Supertype.Ongoing,
      Arm.nullary "Snow" Supertype.Snow,
      Arm.nullary "World" Supertype.World
    ]
  where
    encode s = Common.nullary $ case s of
      Supertype.Basic -> "Basic"
      Supertype.Legendary -> "Legendary"
      Supertype.Ongoing -> "Ongoing"
      Supertype.Snow -> "Snow"
      Supertype.World -> "World"
