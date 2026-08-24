{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FlipCoin where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FlipCoin as FlipCoin

-- | A bare object keyed by the record's field name, Pawl.Codec.RollDie's shape
-- one type over: the slot is required, since a flip nothing reads is an effect
-- with no observable outcome.
codec :: Codec.Codec FlipCoin.FlipCoin
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec FlipCoin.slot
  pure FlipCoin.MkFlipCoin {FlipCoin.slot = slot}
