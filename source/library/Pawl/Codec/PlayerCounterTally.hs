{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerCounterTally where

import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464). The tag that picks it is
-- Pawl.Codec.Quantity's "PlayerCounters", which this module is deliberately not
-- named after -- see Pawl.Types.PlayerCounterTally.
codec :: Codec.Codec PlayerCounterTally.PlayerCounterTally
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec PlayerCounterTally.player
  kind <- Fields.required "kind" PlayerCounterKind.codec PlayerCounterTally.kind
  pure PlayerCounterTally.MkPlayerCounterTally {PlayerCounterTally.player = player, PlayerCounterTally.kind = kind}
