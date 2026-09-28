{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AnyNumberDiscard where

import qualified Pawl.Codec.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AnyNumberDiscard as AnyNumberDiscard

-- | A bare object keyed by the record's field names, Pawl.Codec.CountedDiscard's
-- shape: the tag is Pawl.Codec.Discard's, and the bound slot is ELIDED when
-- absent.
codec :: Codec.Codec AnyNumberDiscard.AnyNumberDiscard
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec AnyNumberDiscard.slot
  cards <- Fields.required "cards" AnyNumberMatching.codec AnyNumberDiscard.cards
  discarded <- Fields.defaulted "discarded" Nothing (Common.maybe SlotName.codec) AnyNumberDiscard.discarded
  pure
    AnyNumberDiscard.MkAnyNumberDiscard
      { AnyNumberDiscard.slot = slot,
        AnyNumberDiscard.cards = cards,
        AnyNumberDiscard.discarded = discarded
      }
