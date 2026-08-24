module Pawl.Codec.Expiry where

import qualified Pawl.Codec.AfterTurn as AfterTurn
import qualified Pawl.Codec.PaidExpiry as PaidExpiry
import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.While as While
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Expiry as Expiry

-- | CR 611.2: the STORED duration, which never appears in card JSON -- a card
-- carries a Duration and Pawl.Engine.Expiry.arm turns it into this. The one
-- thing that serialises an Expiry is a DelayedTrigger, since CR 603.7b lets a
-- delayed ability state one.
codec :: Codec.Codec Expiry.Expiry
codec =
  Arm.tagged
    [ Arm.nullary "AtCleanup" Expiry.AtCleanup,
      Arm.nullary "Never" Expiry.Never,
      Arm.payload "While" While.codec Expiry.While (\x -> case x of Expiry.While y -> Just y; _ -> Nothing),
      Arm.payload "AtTurnOf" PlayerId.codec Expiry.AtTurnOf (\x -> case x of Expiry.AtTurnOf y -> Just y; _ -> Nothing),
      Arm.payload "AtEndOfTurnOf" AfterTurn.codec Expiry.AtEndOfTurnOf (\x -> case x of Expiry.AtEndOfTurnOf y -> Just y; _ -> Nothing),
      Arm.payload "AtEndOf" PhaseSelector.codec Expiry.AtEndOf (\x -> case x of Expiry.AtEndOf y -> Just y; _ -> Nothing),
      Arm.payload "WhenPaid" PaidExpiry.codec Expiry.WhenPaid (\x -> case x of Expiry.WhenPaid y -> Just y; _ -> Nothing)
    ]
