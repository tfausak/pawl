{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Impending where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Impending as Impending

-- | A bare object keyed by the record's field names, Pawl.Codec.Reinforce's
-- shape. The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Impending.Impending keyword)
codec keywordCodec = Fields.object $ do
  counters <- Fields.required "counters" Common.natural Impending.counters
  cost <- Fields.required "cost" (Cost.codec keywordCodec) Impending.cost
  pure Impending.MkImpending {Impending.counters = counters, Impending.cost = cost}
