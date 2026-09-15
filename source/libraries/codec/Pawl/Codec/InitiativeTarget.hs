module Pawl.Codec.InitiativeTarget where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.InitiativeTarget as InitiativeTarget

-- | CR 726.1: which player an Effect.TakeTheInitiative names, on the wire.
codec :: Codec.Codec InitiativeTarget.InitiativeTarget
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "TheController" InitiativeTarget.TheController,
      Arm.nullary "ControllerOfSource" InitiativeTarget.ControllerOfSource
    ]

tagOf :: InitiativeTarget.InitiativeTarget -> String
tagOf x = case x of
  InitiativeTarget.TheController {} -> "TheController"
  InitiativeTarget.ControllerOfSource {} -> "ControllerOfSource"
