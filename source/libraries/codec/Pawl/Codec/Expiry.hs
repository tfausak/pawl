module Pawl.Codec.Expiry where

import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.While as While
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Expiry as Expiry

-- | CR 611.2: the STORED duration, which never appears in card JSON -- a card
-- carries a Duration and Pawl.Engine.Expiry.arm turns it into this. The one
-- thing that serialises an Expiry is a DelayedTrigger, since CR 603.7b lets a
-- delayed ability state one.
codec :: Codec.Codec Expiry.Expiry
codec =
  Arm.tagged
    encode
    [ Arm.nullary "AtCleanup" Expiry.AtCleanup,
      Arm.nullary "Never" Expiry.Never,
      Arm.payload "While" While.codec Expiry.While,
      Arm.payload "AtTurnOf" PlayerId.codec Expiry.AtTurnOf,
      Arm.payload "AtEndOf" PhaseSelector.codec Expiry.AtEndOf
    ]
  where
    encode e = case e of
      Expiry.AtCleanup -> Common.nullary "AtCleanup"
      Expiry.Never -> Common.nullary "Never"
      Expiry.While x -> Common.tagged "While" . Just $ Codec.encode While.codec x
      Expiry.AtTurnOf p -> Common.tagged "AtTurnOf" . Just $ Codec.encode PlayerId.codec p
      Expiry.AtEndOf sel -> Common.tagged "AtEndOf" . Just $ Codec.encode PhaseSelector.codec sel
