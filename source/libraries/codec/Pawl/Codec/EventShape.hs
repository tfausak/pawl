module Pawl.Codec.EventShape where

import qualified Pawl.Codec.CardArrivedIn as CardArrivedIn
import qualified Pawl.Codec.MovedBetween as MovedBetween
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.EventShape as EventShape

-- | Every arm is tagged, and each payload's own codec says what its value looks
-- like: CardArrivedIn's is an object rather than the bare Zone it once was,
-- since the shape now carries an origin exclusion beside the destination.
codec :: Codec.Codec EventShape.EventShape
codec =
  Arm.tagged
    [ Arm.payload "MovedBetween" MovedBetween.codec EventShape.MovedBetween (\x -> case x of EventShape.MovedBetween y -> Just y; _ -> Nothing),
      Arm.payload "CardArrivedIn" CardArrivedIn.codec EventShape.CardArrivedIn (\x -> case x of EventShape.CardArrivedIn y -> Just y; _ -> Nothing),
      Arm.nullary "SpellCast" EventShape.SpellCast
    ]
