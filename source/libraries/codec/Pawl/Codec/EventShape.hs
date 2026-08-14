module Pawl.Codec.EventShape where

import qualified Pawl.Codec.MovedBetween as MovedBetween
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EventShape as EventShape

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec EventShape.EventShape
codec =
  Arm.tagged
    encode
    [ Arm.payload "MovedBetween" MovedBetween.codec EventShape.MovedBetween,
      Arm.nullary "SpellCast" EventShape.SpellCast
    ]
  where
    encode s = case s of
      EventShape.MovedBetween x -> Common.tagged "MovedBetween" . Just $ Codec.encode MovedBetween.codec x
      EventShape.SpellCast -> Common.nullary "SpellCast"
