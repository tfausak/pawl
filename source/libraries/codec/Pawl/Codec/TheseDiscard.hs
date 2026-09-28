{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TheseDiscard where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TheseDiscard as TheseDiscard

-- | A bare object keyed by the record's field names, Pawl.Codec.CountedDiscard's
-- shape: the tag is Pawl.Codec.Discard's, and the bound slot is ELIDED when
-- absent.
codec :: Codec.Codec TheseDiscard.TheseDiscard
codec = Fields.object $ do
  cards <- Fields.required "cards" ObjectRef.codec TheseDiscard.cards
  discarded <- Fields.defaulted "discarded" Nothing (Common.maybe SlotName.codec) TheseDiscard.discarded
  pure
    TheseDiscard.MkTheseDiscard
      { TheseDiscard.cards = cards,
        TheseDiscard.discarded = discarded
      }
