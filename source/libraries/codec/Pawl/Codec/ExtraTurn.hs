{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExtraTurn where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExtraTurn as ExtraTurn

-- | The CR 500.11 skips travel WITH the turn rather than referencing it, so they
-- are a field here and not a separate collection keyed by turn.
codec :: Codec.Codec ExtraTurn.ExtraTurn
codec = Fields.object $ do
  taker <- Fields.required "taker" PlayerId.codec ExtraTurn.taker
  source <- Fields.required "source" ObjectId.codec ExtraTurn.source
  skipped <- Fields.required "skipped" (Common.set PhaseSelector.codec) ExtraTurn.skipped
  pure
    ExtraTurn.MkExtraTurn
      { ExtraTurn.taker = taker,
        ExtraTurn.source = source,
        ExtraTurn.skipped = skipped
      }
