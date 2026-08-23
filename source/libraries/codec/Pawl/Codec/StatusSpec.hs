module Pawl.Codec.StatusSpec where

import qualified Pawl.Codec.Status as Status
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Departure as Departure
import qualified Pawl.Types.Status as Status

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Status" $ do
  Spec.it s "Playing" $
    Common.assertCodec
      s
      Status.codec
      Status.Playing
      " {\"type\":\"Playing\"} "
  -- CR 104.3: HOW the player left is part of the state, not just that they did.
  Spec.it s "Departed carries the departure" $
    Common.assertCodec
      s
      Status.codec
      (Status.Departed Departure.Conceded)
      " {\"type\":\"Departed\",\"value\":{\"type\":\"Conceded\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Status.codec
