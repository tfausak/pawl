module Pawl.Codec.ExchangedValue where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ExchangedValue as ExchangedValue

codec :: Codec.Codec ExchangedValue.ExchangedValue
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "LifeTotal" PlayerRef.codec ExchangedValue.LifeTotal (\x -> case x of ExchangedValue.LifeTotal y -> Just y; _ -> Nothing),
      Arm.payload "Power" ObjectRef.codec ExchangedValue.Power (\x -> case x of ExchangedValue.Power y -> Just y; _ -> Nothing),
      Arm.payload "Toughness" ObjectRef.codec ExchangedValue.Toughness (\x -> case x of ExchangedValue.Toughness y -> Just y; _ -> Nothing)
    ]

tagOf :: ExchangedValue.ExchangedValue -> String
tagOf x = case x of
  ExchangedValue.LifeTotal {} -> "LifeTotal"
  ExchangedValue.Power {} -> "Power"
  ExchangedValue.Toughness {} -> "Toughness"
