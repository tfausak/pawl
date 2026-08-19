module Pawl.Codec.CostScale where

import qualified Pawl.Codec.Color as Color
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CostScale as CostScale

-- | Tagged rather than an enum, since one arm carries a colour.
codec :: Codec.Codec CostScale.CostScale
codec =
  Arm.tagged
    [ Arm.nullary "Once" CostScale.Once,
      Arm.payload "PerColoredSymbol" Color.codec CostScale.PerColoredSymbol (\x -> case x of CostScale.PerColoredSymbol y -> Just y; _ -> Nothing)
    ]
