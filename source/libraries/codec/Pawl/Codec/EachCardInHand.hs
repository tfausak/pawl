{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EachCardInHand where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.GraveyardScope as GraveyardScope
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EachCardInHand as EachCardInHand

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.ObjectRef's EachCardInHand arm.
--
-- @filter@ is omitted when absent, where Pawl.Codec.EachCardInGraveyard's is
-- required: the whole hand is the commoner of the two readings -- Amnesia's
-- reveal takes it -- so the shorter spelling is the unfiltered one.
codec :: Codec.Codec EachCardInHand.EachCardInHand
codec = Fields.object $ do
  hands <- Fields.required "hands" GraveyardScope.codec EachCardInHand.hands
  filter_ <- Fields.defaulted "filter" Nothing (Common.maybe (Filter.codec Keyword.codec)) EachCardInHand.filter
  pure
    EachCardInHand.MkEachCardInHand
      { EachCardInHand.hands = hands,
        EachCardInHand.filter = filter_
      }
