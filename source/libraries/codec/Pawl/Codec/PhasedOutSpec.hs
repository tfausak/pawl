module Pawl.Codec.PhasedOutSpec where

import qualified Pawl.Codec.PhasedOut as PhasedOut
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PhasedOut as PhasedOut
import qualified Pawl.Types.PlayerId as PlayerId

-- | Each arm under a DIFFERENT seat, so an encoder that wrote a constant tag
-- with the right payload, or the right tag with a constant payload, fails.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PhasedOut" $ do
  -- CR 702.26a: phased out under that player, and phasing in during their next
  -- untap step.
  Spec.it s "Directly" $
    Common.assertCodec
      s
      PhasedOut.codec
      (PhasedOut.Directly (PlayerId.MkPlayerId 1))
      " {\"type\":\"Directly\",\"value\":1} "
  -- CR 702.26g: the seat is who controlled it, not a schedule -- an indirectly
  -- phased-out Aura phases in with its host.
  Spec.it s "Indirectly" $
    Common.assertCodec
      s
      PhasedOut.codec
      (PhasedOut.Indirectly (PlayerId.MkPlayerId 2))
      " {\"type\":\"Indirectly\",\"value\":2} "
  -- CR 702.26n: the seat has left the game (CR 800.4k), so the schedule is
  -- rescheduled while the stored player stays who CR 702.26a defines.
  Spec.it s "Orphaned" $
    Common.assertCodec
      s
      PhasedOut.codec
      (PhasedOut.Orphaned (PlayerId.MkPlayerId 3))
      " {\"type\":\"Orphaned\",\"value\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PhasedOut.codec
