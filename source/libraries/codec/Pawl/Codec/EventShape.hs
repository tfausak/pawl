module Pawl.Codec.EventShape where

import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Value as Value
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
    [ Arm.payload "MovedBetween" (Common.tuple Zone.codec Zone.codec) (uncurry EventShape.MovedBetween),
      Arm.nullary "SpellCast" EventShape.SpellCast
    ]
  where
    encode s = case s of
      EventShape.MovedBetween from to ->
        Common.tagged "MovedBetween" . Just . Value.array $
          [Codec.encode Zone.codec from, Codec.encode Zone.codec to]
      EventShape.SpellCast -> Common.nullary "SpellCast"
