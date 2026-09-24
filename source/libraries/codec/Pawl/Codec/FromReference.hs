{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FromReference where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FromReference as FromReference

-- | A bare object keyed by the record's field names. The amount is omitted where
-- the filter names none, Pawl.Codec.TargetSlot's posture for its own amount.
codec :: Codec.Codec FromReference.FromReference
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) FromReference.filter
  amount <- Fields.defaulted "amount" Nothing (Common.maybe Quantity.codec) FromReference.amount
  pure
    FromReference.MkFromReference
      { FromReference.filter = filter_,
        FromReference.amount = amount
      }
