{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerControl where

import qualified Pawl.Codec.ControlDuration as ControlDuration
import qualified Pawl.Codec.Decider as Decider
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerControl as PlayerControl

-- | An object keyed by the record's field names, with rule 723.7's restriction
-- elided when it is absent -- which is every rule 723.1 control.
codec :: Codec.Codec PlayerControl.PlayerControl
codec = Fields.object $ do
  decider <- Fields.required "decider" Decider.codec PlayerControl.decider
  duration <- Fields.required "duration" ControlDuration.codec PlayerControl.duration
  manaFromLandsOnly <- Fields.defaulted "manaFromLandsOnly" False Common.boolean PlayerControl.manaFromLandsOnly
  pure
    PlayerControl.MkPlayerControl
      { PlayerControl.decider = decider,
        PlayerControl.duration = duration,
        PlayerControl.manaFromLandsOnly = manaFromLandsOnly
      }
