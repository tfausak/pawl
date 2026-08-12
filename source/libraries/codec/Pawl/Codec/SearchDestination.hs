module Pawl.Codec.SearchDestination where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SearchDestination as SearchDestination

codec :: Codec.Codec SearchDestination.SearchDestination
codec =
  Arm.tagged
    encode
    [ Arm.nullary "BattlefieldTapped" SearchDestination.BattlefieldTapped,
      Arm.nullary "RevealThenHand" SearchDestination.RevealThenHand,
      Arm.nullary "Exile" SearchDestination.Exile
    ]
  where
    encode d = Common.nullary $ case d of
      SearchDestination.BattlefieldTapped -> "BattlefieldTapped"
      SearchDestination.RevealThenHand -> "RevealThenHand"
      SearchDestination.Exile -> "Exile"
