module Pawl.Codec.EventShape where

import qualified Pawl.Codec.MovedBetween as MovedBetween
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.EventShape as EventShape

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec EventShape.EventShape
codec =
  Arm.tagged
    [ Arm.payload "MovedBetween" MovedBetween.codec EventShape.MovedBetween (\x -> case x of EventShape.MovedBetween y -> Just y; _ -> Nothing),
      Arm.nullary "SpellCast" EventShape.SpellCast
    ]
