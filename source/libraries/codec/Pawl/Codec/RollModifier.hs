module Pawl.Codec.RollModifier where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.RollModifier as RollModifier

codec :: Codec.Codec RollModifier.RollModifier
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Reroll" RollModifier.Reroll,
      Arm.payload "IncreaseOrDecrease" Common.natural RollModifier.IncreaseOrDecrease (\x -> case x of RollModifier.IncreaseOrDecrease y -> Just y; _ -> Nothing)
    ]

tagOf :: RollModifier.RollModifier -> String
tagOf x = case x of
  RollModifier.Reroll {} -> "Reroll"
  RollModifier.IncreaseOrDecrease {} -> "IncreaseOrDecrease"
