module Pawl.Codec.TurnWindow where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TurnWindow as TurnWindow

-- | CR 603.7a: the STORED onset, which like Expiry beside it never appears in card
-- JSON -- a card carries an Onset and Pawl.Engine.Event.armOnset turns it into
-- this. The one thing that serialises a TurnWindow is a DelayedTrigger.
codec :: Codec.Codec TurnWindow.TurnWindow
codec =
  Arm.tagged
    [ Arm.nullary "AnyTurn" TurnWindow.AnyTurn,
      Arm.nullary "ControllersNextTurn" TurnWindow.ControllersNextTurn,
      Arm.payload "OnTurn" Common.natural TurnWindow.OnTurn (\x -> case x of TurnWindow.OnTurn y -> Just y; _ -> Nothing)
    ]
