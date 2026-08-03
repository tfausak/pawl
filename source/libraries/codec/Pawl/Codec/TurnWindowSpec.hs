module Pawl.Codec.TurnWindowSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TurnWindow as TurnWindow
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TurnWindow as TurnWindow

-- | One case per arm of the stored onset's life cycle (Pawl.Types.TurnWindow): no
-- restriction, waiting for the controller's next turn, and settled on the turn
-- that turned out to be.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TurnWindow" $ do
  Spec.it s "AnyTurn" $
    Common.assertJsonCodec
      s
      TurnWindow.toJson
      TurnWindow.fromJson
      TurnWindow.AnyTurn
      "{\"type\":\"AnyTurn\"}"
  Spec.it s "ControllersNextTurn" $
    Common.assertJsonCodec
      s
      TurnWindow.toJson
      TurnWindow.fromJson
      TurnWindow.ControllersNextTurn
      "{\"type\":\"ControllersNextTurn\"}"
  Spec.it s "OnTurn carries the settled turn number" $
    Common.assertJsonCodec
      s
      TurnWindow.toJson
      TurnWindow.fromJson
      (TurnWindow.OnTurn 7)
      "{\"type\":\"OnTurn\",\"value\":7}"
