{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Suspend where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Suspend as Suspend

-- | A bare object keyed by the record's field names, Pawl.Codec.Equip's shape.
-- The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Suspend.Suspend keyword)
codec keywordCodec = Fields.object $ do
  counters <- Fields.required "counters" Common.natural Suspend.counters
  cost <- Fields.required "cost" (Cost.codec keywordCodec) Suspend.cost
  pure Suspend.MkSuspend {Suspend.counters = counters, Suspend.cost = cost}
