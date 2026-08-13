{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TakeExtraTurn where

import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's TakeExtraTurn arm.
codec :: Codec.Codec TakeExtraTurn.TakeExtraTurn
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec TakeExtraTurn.player
  skips <- Fields.required "skips" (Common.set PhaseSelector.codec) TakeExtraTurn.skips
  pure
    TakeExtraTurn.MkTakeExtraTurn
      { TakeExtraTurn.player = player,
        TakeExtraTurn.skips = skips
      }
