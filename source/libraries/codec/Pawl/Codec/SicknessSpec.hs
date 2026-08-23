module Pawl.Codec.SicknessSpec where

import qualified Pawl.Codec.Sickness as Sickness
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Sickness as Sickness

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Sickness" $ do
  Spec.it s "Sick" $
    Common.assertCodec
      s
      Sickness.codec
      Sickness.Sick
      " {\"type\":\"Sick\"} "
  -- CR 302.6's subject is a PLAYER, so the seat is the payload rather than a
  -- flag: "settled" alone got Control Magic wrong (#198).
  Spec.it s "Settled names the player it is settled under" $
    Common.assertCodec
      s
      Sickness.codec
      (Sickness.Settled (PlayerId.MkPlayerId 2))
      " {\"type\":\"Settled\",\"value\":2} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Sickness.codec
