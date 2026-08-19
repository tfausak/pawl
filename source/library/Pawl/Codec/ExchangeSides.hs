module Pawl.Codec.ExchangeSides where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ExchangeSides as ExchangeSides

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec ExchangeSides.ExchangeSides
codec =
  Arm.tagged
    [ Arm.payload "WithController" SlotName.codec ExchangeSides.WithController (\x -> case x of ExchangeSides.WithController y -> Just y; _ -> Nothing),
      Arm.payload "BetweenTargets" SlotName.codec ExchangeSides.BetweenTargets (\x -> case x of ExchangeSides.BetweenTargets y -> Just y; _ -> Nothing)
    ]
