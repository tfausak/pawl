{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SkipNextPhase where

import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's SkipNextPhase arm.
codec :: Codec.Codec SkipNextPhase.SkipNextPhase
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec SkipNextPhase.player
  selector <- Fields.required "selector" PhaseSelector.codec SkipNextPhase.selector
  pure
    SkipNextPhase.MkSkipNextPhase
      { SkipNextPhase.player = player,
        SkipNextPhase.selector = selector
      }
