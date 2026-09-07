module Pawl.Codec.PlayerControlSpec where

import qualified Pawl.Codec.PlayerControl as PlayerControl
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControlDuration as ControlDuration
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.PlayerControl as PlayerControl
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerControl" $ do
  -- CR 723.1's control, which is what Mindslaver installs: no rule 723.7
  -- restriction, so the key is elided.
  Spec.it s "MkPlayerControl, restriction elided" $
    Common.assertCodec
      s
      PlayerControl.codec
      ( PlayerControl.MkPlayerControl
          { PlayerControl.decider = Decider.MkDecider (PlayerId.MkPlayerId 1),
            PlayerControl.duration = ControlDuration.UntilTurnEnds,
            PlayerControl.manaFromLandsOnly = False
          }
      )
      " {\"decider\":1,\"duration\":{\"type\":\"UntilTurnEnds\"}} "
  -- CR 723.2's, with rule 723.7's restriction written: Word of Command's row.
  Spec.it s "MkPlayerControl, restriction written" $
    Common.assertCodec
      s
      PlayerControl.codec
      ( PlayerControl.MkPlayerControl
          { PlayerControl.decider = Decider.MkDecider (PlayerId.MkPlayerId 2),
            PlayerControl.duration = ControlDuration.UntilResolutionEnds,
            PlayerControl.manaFromLandsOnly = True
          }
      )
      " {\"decider\":2,\"duration\":{\"type\":\"UntilResolutionEnds\"},\"manaFromLandsOnly\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerControl.codec
