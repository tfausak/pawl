{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Times where

import qualified Data.Typeable as Typeable
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Times as Times

-- | A bare object keyed by the record's field names, Pawl.Codec.Halved's shape.
--
-- The quantity codec is a PARAMETER rather than an import, for the reason
-- Pawl.Types.Times gives: the record is parametric in the quantity so that
-- neither module has to name the other.
codec :: (Typeable.Typeable quantity) => Codec.Codec quantity -> Codec.Codec (Times.Times quantity)
codec quantityCodec = Fields.object $ do
  factor <- Fields.required "factor" Common.natural Times.factor
  quantity <- Fields.required "quantity" quantityCodec Times.quantity
  pure Times.MkTimes {Times.factor = factor, Times.quantity = quantity}
