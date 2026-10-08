{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RemovePlayerCounters where

import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RemovePlayerCounters as RemovePlayerCounters

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's RemovePlayerCounters arm. The tally slot is
-- ELIDED when absent, Pawl.Codec.RemoveCounters' posture, so a removal that
-- nothing looks back at writes only the three keys it always did.
codec :: Codec.Codec RemovePlayerCounters.RemovePlayerCounters
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec RemovePlayerCounters.player
  kind <- Fields.required "kind" PlayerCounterKind.codec RemovePlayerCounters.kind
  quantity <- Fields.required "quantity" Quantity.codec RemovePlayerCounters.quantity
  tally <- Fields.defaulted "tally" Nothing (Common.maybe SlotName.codec) RemovePlayerCounters.tally
  pure
    RemovePlayerCounters.MkRemovePlayerCounters
      { RemovePlayerCounters.player = player,
        RemovePlayerCounters.kind = kind,
        RemovePlayerCounters.quantity = quantity,
        RemovePlayerCounters.tally = tally
      }
