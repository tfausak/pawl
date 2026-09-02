{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FlipCoin where

import qualified Pawl.Codec.CoinReading as CoinReading
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FlipCoin as FlipCoin

-- | A bare object keyed by the record's field names. Only `slot` is required: a
-- flip nothing reads is an effect with no observable outcome, where the other
-- three fields have the value all but a few cards in data\/cards\/ print.
codec :: Codec.Codec FlipCoin.FlipCoin
codec = Fields.object $ do
  count <- Fields.defaulted "count" FlipCoin.defaultCount Quantity.codec FlipCoin.count
  reading <- Fields.defaulted "reading" FlipCoin.defaultReading CoinReading.codec FlipCoin.reading
  slot <- Fields.required "slot" SlotName.codec FlipCoin.slot
  misses <- Fields.defaulted "misses" Nothing (Common.maybe SlotName.codec) FlipCoin.misses
  pure FlipCoin.MkFlipCoin {FlipCoin.count = count, FlipCoin.reading = reading, FlipCoin.slot = slot, FlipCoin.misses = misses}
