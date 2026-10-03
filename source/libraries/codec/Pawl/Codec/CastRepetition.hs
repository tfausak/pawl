module Pawl.Codec.CastRepetition where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CastRepetition as CastRepetition

-- | Tagged rather than an enum, since one arm carries a quantity.
codec :: Codec.Codec CastRepetition.CastRepetition
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Once" CastRepetition.Once,
      Arm.nullary "AnyNumber" CastRepetition.AnyNumber,
      Arm.payload "WithinTotalManaValue" Quantity.codec CastRepetition.WithinTotalManaValue (\x -> case x of CastRepetition.WithinTotalManaValue y -> Just y; _ -> Nothing)
    ]

tagOf :: CastRepetition.CastRepetition -> String
tagOf x = case x of
  CastRepetition.Once {} -> "Once"
  CastRepetition.AnyNumber {} -> "AnyNumber"
  CastRepetition.WithinTotalManaValue {} -> "WithinTotalManaValue"
