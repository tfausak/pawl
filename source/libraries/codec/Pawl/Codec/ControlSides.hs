module Pawl.Codec.ControlSides where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ControlSides as ControlSides

codec :: Codec.Codec ControlSides.ControlSides
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "BetweenTargets" SlotName.codec ControlSides.BetweenTargets (\x -> case x of ControlSides.BetweenTargets y -> Just y; _ -> Nothing),
      Arm.payload "WithSource" SlotName.codec ControlSides.WithSource (\x -> case x of ControlSides.WithSource y -> Just y; _ -> Nothing)
    ]

tagOf :: ControlSides.ControlSides -> String
tagOf x = case x of
  ControlSides.BetweenTargets {} -> "BetweenTargets"
  ControlSides.WithSource {} -> "WithSource"
