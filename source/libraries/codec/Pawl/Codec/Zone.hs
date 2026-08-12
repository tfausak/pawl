module Pawl.Codec.Zone where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Zone as Zone

codec :: Codec.Codec Zone.Zone
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Library" Zone.Library,
      Arm.nullary "Hand" Zone.Hand,
      Arm.nullary "Graveyard" Zone.Graveyard,
      Arm.nullary "Battlefield" Zone.Battlefield,
      Arm.nullary "Stack" Zone.Stack,
      Arm.nullary "Exile" Zone.Exile,
      Arm.nullary "Command" Zone.Command
    ]
  where
    encode z = Common.nullary $ case z of
      Zone.Library -> "Library"
      Zone.Hand -> "Hand"
      Zone.Graveyard -> "Graveyard"
      Zone.Battlefield -> "Battlefield"
      Zone.Stack -> "Stack"
      Zone.Exile -> "Exile"
      Zone.Command -> "Command"
