module Pawl.Codec.ExchangeSides where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ExchangeSides as ExchangeSides

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec ExchangeSides.ExchangeSides
codec =
  Arm.tagged
    encode
    [ Arm.payload "WithController" SlotName.codec ExchangeSides.WithController,
      Arm.payload "BetweenTargets" SlotName.codec ExchangeSides.BetweenTargets
    ]
  where
    encode sides = case sides of
      ExchangeSides.WithController n -> Common.tagged "WithController" . Just $ Codec.encode SlotName.codec n
      ExchangeSides.BetweenTargets n -> Common.tagged "BetweenTargets" . Just $ Codec.encode SlotName.codec n
